;;; elspell.el --- Hunspell-compatible spell checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; URL: https://github.com/astoff/elspell
;; Keywords: text
;; Package-Requires: ((emacs "28.1") (compat "31.1"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs Lisp port of the word-checking and suggestion algorithms of
;; Nuspell (https://github.com/nuspell/nuspell), a C++ reimplementation
;; of Hunspell.  It reads Hunspell dictionaries (.aff and .dic files)
;; and provides `elspell-check' and `elspell-suggest', plus a
;; personal word list (`elspell-add-word', `elspell-remove-word').
;;
;; Known limitations:
;;
;; - Dictionaries that set COMPLEXPREFIXES (double prefix stripping,
;;   used by a handful of languages) are rejected at `elspell-create'
;;   time.
;; - LANG-specific casing rules (e.g. Turkish dotless i) are not
;;   implemented; casing uses Emacs's own `upcase'/`downcase'.
;; - PHONE (phonetic suggestions, a Hunspell-only feature) is not
;;   supported.
;; - Words longer than 360 characters are treated as correctly spelled,
;;   rather than as misspellings, so that long non-word tokens are not
;;   flagged as errors; no suggestions are attempted for them.

;;; Code:

(require 'cl-lib)
(require 'compat)

(defgroup elspell nil
  "Hunspell-compatible spell checking library in pure Emacs Lisp."
  :prefix "elspell-"
  :group 'text)

(defcustom elspell-dictionary-path
  (append
   (parse-colon-path (getenv "DICPATH"))
   (if (memq system-type '(windows-nt ms-dos))
       (append
        (mapcan (lambda (var)
                  (when-let* ((d (getenv var)))
                    (list (expand-file-name "hunspell/" d))))
                '("LOCALAPPDATA" "APPDATA" "PROGRAMDATA"))
        '("C:/Hunspell/"))
     (require 'xdg)
     (declare-function xdg-data-home "xdg")
     (declare-function xdg-data-dirs "xdg")
     (cons (expand-file-name "hunspell/" (xdg-data-home))
           (mapcar (lambda (d) (expand-file-name "hunspell/" d))
                   (xdg-data-dirs))))
   (when (eq system-type 'darwin)
     (list (expand-file-name "~/Library/Spelling/")
           "/Library/Spelling/")))
  "List of directories to search for dictionaries.
Given a dictionary NAME, we expect to find files NAME.aff and NAME.dic
in one of those directories."
  :type '(repeat directory))

(defcustom elspell-personal-dictionary-format
  (append
   (when-let* ((file (getenv "WORDLIST")))
     (list (string-replace "%" "%%" (expand-file-name file "~/"))))
   (list (expand-file-name "elspell/%s" user-emacs-directory))
   (pcase system-type
     ((or 'windows-nt 'ms-dos)
      (when-let* ((home (getenv "USERPROFILE")))
        (list (expand-file-name "hunspell_%s" home))))
     (_ (list (expand-file-name "~/.hunspell_%s")))))
  "Name pattern of personal dictionaries.
Each string is passed to `format' with the dictionary name as argument.
The first file to be found is used to save additional words.  If none of
the files exist, create one from the first item of the list."
  :type '(choice string (repeat string)))

(defvar -max-word-length
  (1+ (length "\
Donaudampfschiffahrtselektrizitätenhauptbetriebswerkbauunterbeamtengesellschaft"))
  "Maximum word length to be processed by the spell checker.
Longer words are always reported as correct.")

(defcustom elspell-check-time-limit 5
  "Time limit, in milliseconds, for checking a single word.
If checking a word takes longer than that, `elspell-check' returns
`timeout'.  If nil, there is no limit."
  :type '(choice (natnum :tag "Milliseconds")
                 (const :tag "No limit" nil)))

(defcustom elspell-suggest-time-limit 500
  "Time limit, in milliseconds, for suggesting corrections for a word.
When it is exceeded, the suggestions found so far are returned.  If
nil, there is no limit."
  :type '(choice (natnum :tag "Milliseconds")
                 (const :tag "No limit" nil)))

;; Time limit of the current operation, or nil.  This is a cons
;; (DEADLINE . GC-ELAPSED): the value that `car' of `current-time' may
;; reach before giving up, and the value of `gc-elapsed' when DEADLINE
;; was last set; time spent in GC does not count toward the limit.
;; This is let-bound by the public API functions, together with
;; `current-time-list' set to nil.
(defvar -time-limit nil)

;;; Public API

(defun elspell-create (name)
  "Create a spell checker object for dictionary NAME.
The returned object can be used as DICT argument to the other functions
of this library.

This searches `elspell-dictionary-path' for files named NAME.aff and
NAME.dic."
  (let* ((base (-find-dictionary-base name))
         (aff-file (concat base ".aff"))
         (dic-file (concat base ".dic")))
    (let ((dict (-dict-create))
          ;; Loading briefly allocates a lot (the words table, flag
          ;; lists, ...); let it run without GC interrupting partway.
          (gc-cons-threshold most-positive-fixnum))
      (-parse-aff dict aff-file)
      (when (-dict-complex-prefixes dict)
        (error "elspell: %s uses COMPLEXPREFIXES, which is not supported" name))
      (-parse-dic dict dic-file)
      (-init-personal-dictionary dict name)
      dict)))

(defun elspell-list-dictionaries ()
  "Return a list of available dictionaries.
These are the names you can pass to `elspell-create'."
  (let (names)
    (dolist (dir elspell-dictionary-path)
      (when (file-directory-p dir)
        (dolist (aff (directory-files dir t "\\.aff\\'" t))
          (let ((base (file-name-sans-extension aff)))
            (when (and (file-readable-p aff)
                       (file-readable-p (concat base ".dic")))
              (push (file-name-nondirectory base) names))))))
    ;; For Emacs ≤29
    (compat-call sort (delete-dups names) :in-place t)))

(defun elspell-check (dict word)
  "Return non-nil if WORD is spelled correctly according to DICT.
If the check exceeds `elspell-check-time-limit', return `timeout'."
  (or (length> word -max-word-length)
      (let* ((current-time-list nil)
             (-time-limit (-time-limit-from-now elspell-check-time-limit)))
        (catch '-time-limit
          (-spell dict word)))))

(defun elspell-suggest (dict word)
  "Return a list of corrections for WORD based on DICT.
See also `elspell-suggest-time-limit'."
  (unless (length> word -max-word-length)
    (let* ((gc-cons-percentage (max gc-cons-percentage 1.0))
           (current-time-list nil)
           (-time-limit (-time-limit-from-now elspell-suggest-time-limit))
           ;; `-suggest-for-casing' catches its own timeouts; this only
           ;; catches the early compound check in `-suggest-priv'.
           (sugs (catch '-time-limit (-suggest-priv dict word))))
      (unless (eq sugs 'timeout) sugs))))

(defun elspell-add-word (dict word &optional save)
  "Add WORD to DICT, and optionally SAVE it to personal dictionary file."
  (-dict-add-word dict word nil)
  (when save
    (-save-personal-word (-dict-pdict dict) word)))

(defun elspell-remove-word (dict word &optional save)
  "Remove WORD from DICT, and optionally SAVE that to the personal dictionary file.
Return non-nil if WORD was actually present and therefore removed."
  (let ((removed (-dict-remove-word dict word)))
    (when save
      (-remove-personal-word (-dict-pdict dict) word))
    removed))

(defun elspell-word-regexp (dict)
  "Return a regexp matching a word according to DICT."
  (-dict-word-regexp dict))

;;; LLM notes to self

;; Nuspell was chosen over Hunspell as the porting base because its
;; checking code is organized as small, separately named functions with
;; little incidental state, which maps much more directly onto Lisp than
;; Hunspell's.  Suggestions follow Nuspell's `Suggester', with
;; occasional reference to Hunspell's own `SuggestMgr' for comparison.
;;
;; Besides the limitations listed in the Commentary, the following
;; simplifications were made relative to upstream Nuspell:
;;
;; - For CHECKCOMPOUNDPATTERN's "0" (first word not modified by an
;;   affix), a compound part counts as modified when its root differs
;;   from its text, rather than when its affix has a non-empty strip or
;;   append; the two only disagree for an affix that strips exactly
;;   what it appends.
;; - PHONE is not parsed; Nuspell's own `Suggester' never calls it
;;   either.
;; - MAXCPDSUGS, SUBSTANDARD, NOSPLITSUGS and SUGSWITHDOTS are not
;;   parsed; Nuspell itself parses these but its `Suggester' never
;;   consults them, so they would have no effect here either.
;; - Nuspell's suggestion-casing-fix loop, which moves a
;;   newly-recased, space-containing CAMEL/PASCAL suggestion to the
;;   front of the list one at a time, is instead done as a single
;;   stable partition (recased suggestions first, in their original
;;   relative order); when more than one suggestion qualifies, their
;;   relative order can come out differently than upstream's.
;; - `-move-char-suggest' and `-distant-swap-suggest' enumerate
;;   candidate character moves/swaps directly instead of Nuspell's
;;   swap-and-revert traversal; the budget used to cap either
;;   algorithm is still consulted once per candidate, as upstream
;;   does, but exhausting it while moving through backward moves,
;;   candidates that upstream's forward-then-backward traversal order
;;   would have tried first may be skipped instead.
;; - SIMPLIFIEDTRIPLE's extra "insert the elided letter back and
;;   retry" fallback is not implemented; CHECKCOMPOUNDTRIPLE itself is.
;; - COMPOUNDSYLLABLE/SYLLABLENUM, the Hungarian-specific syllable
;;   counting rules that let a compound exceed COMPOUNDWORDMAX, are not
;;   implemented; COMPOUNDWORDMAX is always a hard cap.  Neither is the
;;   LANG hu "moving rule" for COMPOUNDFORBIDFLAG.
;; - Morphological fields in .dic and the AM directive are ignored,
;;   same as in Nuspell itself.
;; - FULLSTRIP is parsed but has no effect, matching Nuspell's own
;;   (apparently unused) implementation of it.
;; - The 360-character length limit is counted in characters rather
;;   than UTF-8 bytes, and a word over the limit is accepted rather
;;   than rejected.
;; - Unlike Nuspell, a suggestion that OCONV maps back to the
;;   misspelled word itself is dropped, as Hunspell does (its gh#1002).
;; - Unlike Nuspell, checking and suggesting have time limits
;;   (`elspell-check-time-limit', `elspell-suggest-time-limit'),
;;   in the spirit of Hunspell's TIMELIMIT_* ones; but a check that
;;   runs out accepts the word, where Hunspell rejects it.  GC time
;;   does not count toward the limits.
;;
;; For affix stripping, Nuspell tries combinations of up to one prefix
;; and up to two suffixes (or vice versa); this port implements that
;; same set of combinations.  A small number of Nuspell's own
;; performance-only early-exit checks (annotated as such in its
;; sources) are omitted here, since they do not affect correctness.
;;
;; Where Nuspell and Hunspell disagree, this port follows Nuspell.
;; Known cases seen on real dictionaries:
;;
;; - COMPOUNDRULE: Nuspell (`Checker::check_compound_with_rules') looks
;;   up each part as a bare dictionary word; Hunspell also lets the
;;   last part take suffixes (hunspell(5): "COMPOUNDRULE only handles
;;   the suffixation of the last word part").  The Korean dictionary
;;   relies on this for noun + particle forms such as 사람들이, which
;;   Hunspell accepts and Nuspell and this port reject.
;; - FLAG UTF-8 flags above U+FFFF: Hunspell stores flags in 16 bits
;;   and decodes every such character to U+FFFD, so all of them
;;   collapse into one flag; Nuspell rejects the dictionary outright
;;   (FLAG_ABOVE_65535).  This port keeps full code points, i.e. reads
;;   the dictionary as written.  es_ES (Debian) uses three emoji flags,
;;   so e.g. "audacísimo" is accepted only by Hunspell.

;;; Implementation

;;;; Helpers

(defun -time-limit-from-now (ms)
;; Return the `-time-limit' value for MS milliseconds from now, or nil
;; if MS is nil.  Assumes `current-time-list' is nil.
  (when ms
    (let ((now (current-time)))
      (cons (+ (car now) (/ (* ms (cdr now)) 1000)) gc-elapsed))))

(defsubst -check-time-limit ()
;; Throw to the `-time-limit' tag, with value `timeout', if
;; `-time-limit' has passed even after postponing it by the GC time
;; since it was set.  A check catching the throw reports the word as
;; correct; a suggestion returns what it found so far.
  (when -time-limit
    (let ((now (current-time)))
      (when (> (car now) (car -time-limit))
        (setcar -time-limit (+ (car -time-limit)
                               (round (* (- gc-elapsed (cdr -time-limit)) (cdr now)))))
        (setcdr -time-limit gc-elapsed)
        (when (> (car now) (car -time-limit))
          (throw '-time-limit 'timeout))))))

(defun -find-dictionary-base (name)
;; Return the path, sans extension, of dictionary NAME.  Searches
;; `elspell-dictionary-path' for the first directory containing
;; both a readable NAME.aff and a readable NAME.dic, and returns that
;; directory's `expand-file-name' of NAME.  Signals an error if no
;; directory in the list has both files.
  (or (named-let recur ((tail elspell-dictionary-path))
        (when tail
          (or (let ((dir (car tail)))
                (let ((base (expand-file-name name dir)))
                  (and (file-readable-p (concat base ".aff"))
                       (file-readable-p (concat base ".dic"))
                       base)))
              (recur (cdr tail)))))
      (user-error "\
elspell: cannot find %s.aff and %s.dic under `elspell-dictionary-path'"
                  name name)))

;;;; Data structures

(cl-defstruct (-prefix (:constructor -prefix-create))
  ;; One PFX entry.  Flags are fixnums, see `-decode-flags'.
  (flag 0 :type fixnum)
  (cross-product nil :type boolean)
  (stripping "" :type string)
  (appending "" :type string)
  (cont-flags nil :type list)          ; list of flags
  (condition nil :type list)           ; see `-compile-condition'
  (bits (make-bool-vector 9 nil) :type bool-vector)) ; see `-affix-bits'

(cl-defstruct (-suffix (:constructor -suffix-create))
  ;; One SFX entry; slots as in `-prefix'.
  (flag 0 :type fixnum)
  (cross-product nil :type boolean)
  (stripping "" :type string)
  (appending "" :type string)
  (cont-flags nil :type list)
  (condition nil :type list)
  (bits (make-bool-vector 9 nil) :type bool-vector))

(cl-defstruct (-similarity-group (:constructor -similarity-group-create))
  ;; One MAP entry: a set of characters/strings considered
  ;; interchangeable.
  (chars "" :type string)
  (strings nil :type list))            ; list of strings

(cl-defstruct (-compound-pattern (:constructor -compound-pattern-create))
  ;; One CHECKCOMPOUNDPATTERN entry.  TEXT is the first word's ending
  ;; followed by the second word's beginning, and IDX the length of
  ;; the former, so that the join is at IDX.  REPLACEMENT, if non-nil,
  ;; is how TEXT is written at the join instead.
  (text "" :type string)
  (idx 0 :type natnum)
  (replacement nil :type (or null string))
  (first-flag 0 :type fixnum)          ; 0 means none
  (second-flag 0 :type fixnum)
  (first-unmodified nil :type boolean))

(cl-defstruct (-dict (:constructor -dict-create))
  ;; A loaded dictionary.  Flags are fixnums, see `-decode-flags'; a
  ;; flag slot holding 0 means the directive is absent.
  (words nil :type (or null hash-table)) ; word -> (LOWERCASED      . list of flag lists)
  (pdict nil :type (or null string))   ; personal dictionary file name
  (prefix-table nil :type (or null vector)) ; sorted vector of (KEY . `-prefix'), KEY = appending
  (suffix-table nil :type (or null vector)) ; sorted vector of (KEY . `-suffix'), KEY = (reverse appending)
  (prefix-cont-flags nil :type (or null hash-table)) ; all prefixes' cont flags, or nil if none
  (suffix-cont-flags nil :type (or null hash-table)) ; same for suffixes
  (compound-rules nil :type list)      ; list of patterns; pattern = list of (FLAG . QUANT)
  (compound-rules-all-flags nil :type list) ; list of flags
  (break-start nil :type list)         ; lists of strings, see `-partition-break-patterns'
  (break-end nil :type list)
  (break-middle nil :type list)
  (flag-aliases nil :type (or null vector)) ; vector of flag lists, alias N at index N-1; may be empty
  (flag-type 'single-char :type (member single-char utf-8 long num))
  (coding 'undecided :type symbol)     ; coding system of the       . aff/.dic files
  (ignored-chars "" :type string)
  (word-regexp nil :type (or null string)) ; see `elspell-word-regexp'
  (complex-prefixes nil :type boolean)
  (fullstrip nil :type boolean)
  (checksharps nil :type boolean)
  (forbid-warn nil :type boolean)
  (compound-onlyin-flag 0 :type fixnum)
  (circumfix-flag 0 :type fixnum)
  (forbiddenword-flag 0 :type fixnum)
  (keepcase-flag 0 :type fixnum)
  (need-affix-flag 0 :type fixnum)
  (warn-flag 0 :type fixnum)
  (compound-flag 0 :type fixnum)
  (compound-begin-flag 0 :type fixnum)
  (compound-last-flag 0 :type fixnum)
  (compound-middle-flag 0 :type fixnum)
  (compound-min-length 0 :type natnum)
  (compound-max-word-count 0 :type natnum)
  (compound-permit-flag 0 :type fixnum)
  (compound-forbid-flag 0 :type fixnum)
  (compound-root-flag 0 :type fixnum)
  (compound-force-uppercase 0 :type fixnum)
  (compound-check-duplicate nil :type boolean)
  (compound-check-case nil :type boolean)
  (compound-check-triple nil :type boolean)
  (compound-check-rep nil :type boolean)
  (compound-patterns nil :type list)   ; list of `-compound-pattern'
  ;; suggestion options
  (try-chars "" :type string)
  (keyboard-closeness "" :type string)
  (rep-whole nil :type list)           ; lists of (FROM             . TO), see `-build-replacement-table'
  (rep-start nil :type list)
  (rep-end nil :type list)
  (rep-any nil :type list)
  (similarities nil :type list)        ; list of `-similarity-group'
  (input-iconv nil :type (or null hash-table)) ; see `-build-substr-replacer'
  (output-oconv nil :type (or null hash-table))
  (nosuggest-flag 0 :type fixnum)
  (max-ngram-suggestions 4 :type natnum)
  (max-diff-factor 5 :type natnum)
  (only-max-diff nil :type boolean)
  (ngram-candidates nil :type (or null vector))) ; cache, see `-ngram-candidates'

(defsubst -homonyms (dict word)
;; Return the list of flag lists of WORD's homonym entries in DICT.
  (cdr (gethash word (-dict-words dict))))

;;;; Flags and flag sets

(defun -flag-bytes (dict str)
;; Return flags field STR, from a file decoded with DICT's coding
;; system, as its original bytes.  Single-char and long flags are
;; bytes regardless of the SET encoding, as in Hunspell and Nuspell;
;; hu_HU, for one, mixes 8-bit flags into an otherwise UTF-8 file.
  (if (= (string-bytes str) (length str))
      str
    (encode-coding-string str (-dict-coding dict) t)))

(defun -decode-flags (dict str)
;; Decode flags field text STR into a list of flag integers, according
;; to DICT's flag type: one of `single-char', `utf-8', `long' or
;; `num', matching the affix file's FLAG directive (default
;; `single-char').
  (pcase (-dict-flag-type dict)
    ('single-char
     (append (-flag-bytes dict str) nil))
    ('utf-8
     (append str nil))
    ('long
     (let* ((str (-flag-bytes dict str)) (i 0) (n (length str)) result)
       (when (oddp n)
         (error "elspell: odd number of characters in long flags %S" str))
       (while (< i n)
         (push (logior (ash (aref str i) 8) (aref str (1+ i))) result)
         (setq i (+ i 2)))
       (nreverse result)))
    ('num
     (mapcar #'string-to-number (split-string str ",")))
    (type (error "elspell: unknown flag type %S" type))))

(defun -decode-first-flag (dict str)
  (let ((flags (-decode-flags dict str)))
    (unless flags (error "elspell: missing flag in %S" str))
    (car flags)))

(defun -decode-flags-possible-alias (dict str)
;; Decode a .dic word's flags field STR, honoring flag aliases.
  (let ((aliases (-dict-flag-aliases dict)))
    ;; ALIASES is nil before any AF line, and an empty vector after
    ;; parsing an .aff file without them.
    (if (length= aliases 0)
        (-decode-flags dict str)
      (let ((idx (string-to-number str)))
        (if (and (> idx 0) (<= idx (length aliases)))
            (aref aliases (1- idx))
          (error "elspell: invalid flag alias %S" str))))))

(defun -flags-contain (flags flag)
;; Return non-nil if list of flag integers FLAGS contains FLAG.  A
;; FLAG of 0 means "not configured" and never matches, mirroring
;; Nuspell, where such flag fields default to 0 when the corresponding
;; affix-file directive is absent.
  (and (not (zerop flag)) (memq flag flags) t))

(defsubst -hidden-homonym-p (flags)
;; Return non-nil if flag list FLAGS marks a dictionary entry as a
;; hidden homonym.  A hidden homonym is excluded from direct lookup,
;; so that checking the title-cased form of a word whose canonical
;; form has different casing (e.g. an acronym) falls through to other
;; checks rather than accepting it outright; see `-dict-add-word'.
;; Mirrors Nuspell's `Aff_Data::HIDDEN_HOMONYM_FLAG', represented here
;; by the literal flag -1 -- never a valid flag otherwise, since real
;; flags are always non-negative.
  (memq -1 flags))

(defsubst -cross-valid-p (inner-flags outer-flag)
;; Return non-nil if INNER-FLAGS (a flag list) contains OUTER-FLAG.
  (-flags-contain inner-flags outer-flag))

;;;; Affix conditions
;;
;; An affix condition is a tiny regexp-like pattern: "." matches any
;; character, "[abc]"/"[^abc]" matches a set of characters (with no
;; special meaning for "-", unlike POSIX/Emacs bracket expressions),
;; and any other character matches itself.  It is compiled into a
;; list of specs (one per matched character) and is checked against
;; exactly that many characters at the start (for a prefix) or end
;; (for a suffix) of the root word.

(defun -compile-condition (str)
;; Compile affix condition STR into a list of specs.  Each element is
;; a character (literal match), t (any character, from "."), or
;; (NEGATED . CHARS) for a "[...]"/"[^...]" bracket expression.
  (when (string-empty-p str)
    (setq str "."))
  (let ((i 0) (n (length str)) (specs nil))
    (while (< i n)
      (let ((c (aref str i)))
        (cond
         ((eq c ?.)
          (push t specs)
          (setq i (1+ i)))
         ((eq c ?\[)
          (setq i (1+ i))
          (when (>= i n)
            (error "elspell: unterminated bracket in condition %S" str))
          (let (negated)
            (when (eq (aref str i) ?^)
              (setq negated t)
              (setq i (1+ i)))
            (let ((start i))
              (while (and (< i n) (not (eq (aref str i) ?\])))
                (setq i (1+ i)))
              (when (= i start)
                (error "elspell: empty bracket in condition %S" str))
              (when (>= i n)
                (error "elspell: unterminated bracket in condition %S" str))
              (push (cons negated (substring str start i)) specs)
              (setq i (1+ i)))))
         ((eq c ?\])
          (error "elspell: stray closing bracket in condition %S" str))
         (t
          (push c specs)
          (setq i (1+ i))))))
    (nreverse specs)))

(defun -condition-match-spec (spec ch)
  (cond
   ((eq spec t) t)
   ((integerp spec) (eq spec ch))
   ((consp spec)
    (let ((negated (car spec)) (chars (cdr spec)))
      (xor negated (cl-position ch chars))))))

(defun -condition-match-at (specs word start)
;; Return non-nil if SPECS match the characters of WORD from START on.
;; WORD must have at least (length SPECS) characters from START.
  (named-let recur ((specs specs) (idx start))
    (or (null specs)
        (and (-condition-match-spec (car specs) (aref word idx))
             (recur (cdr specs) (1+ idx))))))

(defun -condition-match-prefix (specs word)
;; Return non-nil if WORD's first (length SPECS) characters match
;; SPECS.
  (and (>= (length word) (length specs))
       (-condition-match-at specs word 0)))

(defun -condition-match-suffix (specs word)
;; Return non-nil if WORD's last (length SPECS) characters match
;; SPECS.
  (let ((start (- (length word) (length specs))))
    (and (>= start 0)
         (-condition-match-at specs word start))))

;;;; Affix helpers

(defun -affix-bits (dict cont permit-mode)
;; Return the `bits' slot value, a bool-vector, of a `-prefix' or
;; `-suffix' whose continuation flags are CONT; PERMIT-MODE is the
;; compounding mode in which such an affix needs COMPOUNDPERMITFLAG,
;; :compound-end for a prefix or :compound-begin for a suffix.  These
;; bits cache what would otherwise be several flag-list searches per
;; candidate affix: bit I (for the Ith mode of :full-word, :compound-begin,
;; :compound-middle, :compound-end) is set if the affix is not valid in
;; that mode, as tested by `-prefix-not-valid-p'/`-suffix-not-valid-p';
;; bit 4+I if it is not valid as an outer affix, as tested by
;; `-outer-prefix-not-valid-p'/`-outer-suffix-not-valid-p'; and bit 8
;; if it has CIRCUMFIX.  Mirrors Nuspell's `affix_NOT_valid' and
;; `outer_affix_NOT_valid'.
  (let ((bits (make-bool-vector 9 nil)) (i 0))
    (dolist (mode '(:full-word :compound-begin :compound-middle :compound-end))
      (let ((not-valid
             (or (and (eq mode :full-word)
                      (-flags-contain cont (-dict-compound-onlyin-flag dict)))
                 (and (eq mode permit-mode)
                      (not (-flags-contain cont (-dict-compound-permit-flag dict))))
                 (and (not (eq mode :full-word))
                      (-flags-contain cont (-dict-compound-forbid-flag dict))))))
        (aset bits i (and not-valid t))
        (aset bits (+ 4 i)
              (and (or not-valid (-flags-contain cont (-dict-need-affix-flag dict))) t)))
      (incf i))
    (aset bits 8 (-flags-contain cont (-dict-circumfix-flag dict)))
    bits))

(defsubst -mode-index (mode)
  (pcase mode (:full-word 0) (:compound-begin 1) (:compound-middle 2) (:compound-end 3)))

(defsubst -prefix-not-valid-p (mode pfx)
  (aref (-prefix-bits pfx) (-mode-index mode)))

(defsubst -suffix-not-valid-p (mode sfx)
  (aref (-suffix-bits sfx) (-mode-index mode)))

(defsubst -outer-prefix-not-valid-p (mode pfx)
  (aref (-prefix-bits pfx) (+ 4 (-mode-index mode))))

(defsubst -outer-suffix-not-valid-p (mode sfx)
  (aref (-suffix-bits sfx) (+ 4 (-mode-index mode))))

(defsubst -prefix-circumfix-p (pfx)
  (aref (-prefix-bits pfx) 8))

(defsubst -suffix-circumfix-p (sfx)
  (aref (-suffix-bits sfx) 8))

(defun -valid-inside-compound-p (dict mode flags)
  (cond
   ((eq mode :compound-begin)
    (or (-flags-contain flags (-dict-compound-flag dict))
        (-flags-contain flags (-dict-compound-begin-flag dict))))
   ((eq mode :compound-middle)
    (or (-flags-contain flags (-dict-compound-flag dict))
        (-flags-contain flags (-dict-compound-middle-flag dict))))
   ((eq mode :compound-end)
    (or (-flags-contain flags (-dict-compound-flag dict))
        (-flags-contain flags (-dict-compound-last-flag dict))))
   (t t)))

(defun -affix-table-lower-bound (table lo hi pos ch)
;; Return the first index in TABLE[LO,HI) whose key's char at POS is
;; >= CH.  TABLE[LO,HI) must already be sorted by that character.
  (while (< lo hi)
    (let ((mid (/ (+ lo hi) 2)))
      (if (< (aref (car (aref table mid)) pos) ch)
          (setq lo (1+ mid))
        (setq hi mid))))
  lo)

(defmacro -affix-table-find (var table probe from-end &rest body)
;; Evaluate BODY with VAR bound to each entry of TABLE whose key is a
;; prefix of PROBE -- of PROBE reversed, if FROM-END is non-nil, which
;; must be a constant -- until BODY returns non-nil, and return that
;; value, or nil.  TABLE is a vector of (KEY . ENTRY), sorted
;; ascending by KEY, as built by `-build-affix-table'.  This
;; mirrors Nuspell's `Prefix_Multiset'/`Suffix_Multiset': instead of
;; scanning every entry and testing each key against PROBE directly, it
;; narrows a candidate range of TABLE one character of PROBE at a time,
;; via binary search, visiting an entry whenever its key is fully
;; consumed; so shorter keys come first.  Entries are visited in place,
;; by a loop expanded inline, rather than collected into a list or
;; passed to a closure: that list (over a hundred entries per word for
;; some dictionaries, e.g. pt_BR) was most of the garbage produced
;; while checking, and a closure costs about as much.
  (declare (indent 4))
  (let ((tbl (make-symbol "table")) (prb (make-symbol "probe"))
        (lo (make-symbol "lo")) (hi (make-symbol "hi")) (len (make-symbol "len"))
        (plen (make-symbol "plen")) (ch (make-symbol "ch"))
        (done (make-symbol "done")) (result (make-symbol "result")))
    `(let* ((,tbl ,table) (,prb ,probe)
            (,lo 0) (,hi (length ,tbl)) (,len 0) (,plen (length ,prb))
            (,done nil) (,result nil))
       (while (not ,done)
         ;; TABLE[lo,hi) all share key[0..len) == probe[0..len); visit
         ;; those whose key is now exactly that (nothing left to match).
         (while (and (not ,result) (< ,lo ,hi)
                     (= (length (car (aref ,tbl ,lo))) ,len))
           (setq ,result (let ((,var (cdr (aref ,tbl ,lo)))) ,@body))
           (setq ,lo (1+ ,lo)))
         (if (or ,result (>= ,lo ,hi) (= ,len ,plen))
             (setq ,done t)
           ;; Narrow [lo,hi) to the entries whose next character, at
           ;; POS=len, matches PROBE's; everything in it now has a key
           ;; longer than LEN, so that character is always there.
           (let ((,ch (aref ,prb ,(if from-end `(- ,plen ,len 1) len))))
             (setq ,hi (-affix-table-lower-bound ,tbl ,lo ,hi ,len (1+ ,ch)))
             (setq ,lo (-affix-table-lower-bound ,tbl ,lo ,hi ,len ,ch))
             (setq ,len (1+ ,len)))))
       ,result)))

(defun -build-affix-table (entries key-fn)
;; Build a sorted affix table from the list ENTRIES.  KEY-FN extracts
;; the sort/match key string from one entry (the appending text for
;; prefixes, or its reverse for suffixes); see `-affix-table-find'.
  (let ((table (vconcat (mapcar (lambda (e) (cons (funcall key-fn e) e)) entries))))
    (sort table (lambda (a b) (string< (car a) (car b))))))

(defun -all-affix-table-entries (table)
;; Return the `-prefix'/`-suffix' entries of affix TABLE as a plain
;; list.  Unlike `-affix-table-find', this does not filter by any
;; probe word.
  (mapcar #'cdr table))

(defmacro -do-prefixes-of (spec &rest body)
;; Iterate over the `-prefix' entries of DICT whose text
;; could precede WORD; see `-affix-table-find'.  SPEC has the form (VAR
;; DICT WORD): BODY is evaluated with VAR bound to each entry in turn,
;; until it returns non-nil, which is then the value of the whole form;
;; otherwise the value is nil.
  (declare (indent 1) (debug ((symbolp form form) body)))
  (pcase-let ((`(,var ,dict ,word) spec))
    `(-affix-table-find ,var (-dict-prefix-table ,dict) ,word nil
       ,@body)))

(defmacro -do-suffixes-of (spec &rest body)
;; Like `-do-prefixes-of', over the `-suffix' entries of DICT whose text
;; could follow WORD; see `-affix-table-find'.  SPEC has the form (VAR
;; DICT WORD), as for `-do-prefixes-of'.
  (declare (indent 1) (debug ((symbolp form form) body)))
  (pcase-let ((`(,var ,dict ,word) spec))
    `(-affix-table-find ,var (-dict-suffix-table ,dict) ,word t
       ,@body)))

(defsubst -has-cont-flag-p (cont-flags flag)
;; Return non-nil if FLAG is a continuation flag of some affix, given
;; CONT-FLAGS, the `-dict-prefix-cont-flags' or
;; `-dict-suffix-cont-flags' table.  As in Nuspell, this is purely a
;; shortcut: an outer affix whose flag is in no other affix's
;; continuation flags can never take part in a two-level affixation,
;; so the multi-affix strippers skip it before testing its condition
;; and scanning for inner affixes.
  (and cont-flags (hash-table-contains-p flag cont-flags)))

(defun -prefix-to-root (pfx word)
  (concat (-prefix-stripping pfx)
          (substring word (length (-prefix-appending pfx)))))

(defun -suffix-to-root (sfx word)
  (let ((n (length (-suffix-appending sfx))))
    (concat (substring word 0 (- (length word) n)) (-suffix-stripping sfx))))

(defun -prefix-from-root (pfx root)
;; Apply PFX to ROOT, the opposite direction of `-prefix-to-root'.
  (concat (-prefix-appending pfx) (substring root (length (-prefix-stripping pfx)))))

(defun -suffix-from-root (sfx root)
;; Apply SFX to ROOT, the opposite direction of `-suffix-to-root'.
  (let ((n (length (-suffix-stripping sfx))))
    (concat (substring root 0 (- (length root) n)) (-suffix-appending sfx))))

(defun -prefix-check-condition (pfx word)
  (-condition-match-prefix (-prefix-condition pfx) word))

(defun -suffix-check-condition (sfx word)
  (-condition-match-suffix (-suffix-condition sfx) word))

(defun -prefix-condition-matches-p (pfx word)
;; Like (-prefix-check-condition PFX (-prefix-to-root PFX WORD)), but
;; without ever building that root string.  Nuspell's dictionaries can
;; have many affix entries sharing the same `appending' text (so that
;; `-do-prefixes-of' visits many candidates for one WORD), each
;; telling candidates apart only by its condition; since most such
;; candidates fail their condition, checking it against a WORD-and-
;; STRIPPING "virtual" root -- indexing directly into WORD and
;; STRIPPING instead of consing a new string to throw away -- avoids
;; the allocation for the common case where it was going to be wasted.
  (let* ((specs (-prefix-condition pfx))
         (n (length specs))
         (stripping (-prefix-stripping pfx))
         (strip-len (length stripping))
         (app-len (length (-prefix-appending pfx))))
    (and (>= (+ strip-len (- (length word) app-len)) n)
         (named-let recur ((specs specs) (idx 0))
           (or (null specs)
               (and (-condition-match-spec
                     (car specs)
                     (if (< idx strip-len)
                         (aref stripping idx)
                       (aref word (+ app-len (- idx strip-len)))))
                    (recur (cdr specs) (1+ idx))))))))

(defun -suffix-condition-matches-p (sfx word)
;; Like (-suffix-check-condition SFX (-suffix-to-root SFX WORD)), but
;; without ever building that root string; see
;; `-prefix-condition-matches-p', which this mirrors.
  (let* ((specs (-suffix-condition sfx))
         (n (length specs))
         (stripping (-suffix-stripping sfx))
         (strip-len (length stripping))
         (app-len (length (-suffix-appending sfx)))
         (wordlen (length word))
         (rootlen (+ (- wordlen app-len) strip-len))
         (boundary (- rootlen strip-len)))
    (and (>= rootlen n)
         (named-let recur ((specs specs) (idx (- rootlen n)))
           (or (null specs)
               (and (-condition-match-spec
                     (car specs)
                     (if (>= idx boundary)
                         (aref stripping (- idx boundary))
                       (aref word idx)))
                    (recur (cdr specs) (1+ idx))))))))

;;;; Affix stripping
;;
;; Each of these functions returns (ROOT-WORD . FLAGS), where
;; ROOT-WORD is the dictionary key that was found and FLAGS is the
;; specific flag list of that homonym entry (`eq'-comparable to
;; whatever `-homonyms' returns for it), or nil if no
;; match was found.  This is a simplified version of Nuspell's
;; `Affixing_Result', which also carries the affix objects used; this
;; port has no use for that, since it does not implement Nuspell's
;; syllable/word-count suggestion modifiers.
;;
;; MODE is one of :full-word, :compound-begin, :compound-middle or
;; :compound-end, mirroring Nuspell's `Affixing_Mode'.  The 2- and
;; 3-affix combinations below are only ever used at :full-word (see
;; `-check-simple-word'), matching Nuspell's own usage.
;;
;; The other common arguments: DICT is the `-dict' to look roots up
;; in; WORD is the string to analyze; SKIP-HIDDEN non-nil means to
;; ignore hidden homonym entries (see `-hidden-homonym-p').
;;
;; Terminology: of two affixes, the "outer" one is the one added last
;; when deriving the word, hence the first one stripped, and the
;; "inner" one is closer to the root.  An affix may only be added on
;; top of a root or inner affix whose flags (the root's flags, or the
;; inner affix's continuation flags) contain the affix's own flag;
;; that is what the `-cross-valid-p' tests check.
;;
;; The multi-affix strippers are each split into an entry point, which
;; strips the outer affix (or two) and checks it can be stripped at
;; all, and a helper, which receives the intermediate WORD left over
;; along with the affix objects stripped so far, strips the last affix
;; and looks the result up.  The helper names are those of Nuspell
;; (`strip_pfx_then_sfx_comm_2' and so on), whose trailing digit is
;; the number of the affix the helper strips.

(defun -strip-suffix-only (dict mode word skip-hidden)
;; Analyze WORD as ROOT+SUFFIX, where ROOT is a dictionary entry whose
;; flags contain SUFFIX's flag.  In compound MODEs, either the root or
;; the suffix must carry the flag for that compound position.
  (-do-suffixes-of (se dict word)
    (and (not (or (-outer-suffix-not-valid-p mode se)
                  (and (eq mode :compound-end)
                       (not (string-empty-p (-suffix-appending se)))
                       (-flags-contain (-suffix-cont-flags se) (-dict-compound-onlyin-flag dict)))
                  (-suffix-circumfix-p se)))
         (-suffix-condition-matches-p se word)
         (let ((root (-suffix-to-root se word)))
           (named-let recur ((tail (-homonyms dict root)))
             (when tail
               (or (let ((flags (car tail)))
                     (and (-cross-valid-p flags (-suffix-flag se))
                          (not (and (eq mode :full-word)
                                    (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                          (not (and skip-hidden (-hidden-homonym-p flags)))
                          (or (-valid-inside-compound-p dict mode flags)
                              (-valid-inside-compound-p dict mode (-suffix-cont-flags se)))
                          (cons root flags)))
                   (recur (cdr tail)))))))))

(defun -strip-prefix-only (dict mode word skip-hidden)
;; Analyze WORD as PREFIX+ROOT; the mirror image of
;; `-strip-suffix-only'.
  (-do-prefixes-of (pe dict word)
    (and (not (or (-outer-prefix-not-valid-p mode pe)
                  (-prefix-circumfix-p pe)))
         (-prefix-condition-matches-p pe word)
         (let ((root (-prefix-to-root pe word)))
           (named-let recur ((tail (-homonyms dict root)))
             (when tail
               (or (let ((flags (car tail)))
                     (and (-cross-valid-p flags (-prefix-flag pe))
                          (not (and (eq mode :full-word)
                                    (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                          (not (and skip-hidden (-hidden-homonym-p flags)))
                          (or (-valid-inside-compound-p dict mode flags)
                              (-valid-inside-compound-p dict mode (-prefix-cont-flags pe)))
                          (cons root flags)))
                   (recur (cdr tail)))))))))

(defun -strip-prefix-then-suffix-commutative (dict mode word skip-hidden)
;; Strip one prefix and one suffix, in either order.  A derived word
;; formed as prefix+root+suffix can have been built by adding either
;; affix first, as long as the other one's flag allows cross-product
;; with it; this checks both directions at once, as Nuspell's
;; "commutative" variant does.  This strips the prefix and lets
;; `-strip-pfx-then-sfx-comm-2' do the rest.
  (-do-prefixes-of (pe dict word)
    (and (-prefix-cross-product pe)
         (not (-prefix-not-valid-p mode pe))
         (-prefix-condition-matches-p pe word)
         (let ((root1 (-prefix-to-root pe word)))
           (-strip-pfx-then-sfx-comm-2 dict mode pe root1 skip-hidden)))))

(defun -strip-pfx-then-sfx-comm-2 (dict mode pe word skip-hidden)
;; Second step of `-strip-prefix-then-suffix-commutative': WORD is the
;; original word with prefix PE already stripped.  Find a suffix SE
;; such that WORD is ROOT+SE for a dictionary entry ROOT, and the
;; derivation PE+ROOT+SE is valid with either affix as the outer one:
;;  - PE outer: ROOT allows SE, and SE's continuation flags or ROOT
;;    allow PE;
;;  - SE outer: ROOT allows PE, and PE's continuation flags or ROOT
;;    allow SE.
;; An affix with NEEDAFFIX can't be outer, and both can't have it.
;; PE and SE must agree on CIRCUMFIX: both or neither.
  (let* ((pe-cont (-prefix-cont-flags pe))
         (has-needaffix-pe (-flags-contain pe-cont (-dict-need-affix-flag dict)))
         (circumfix-pe (-prefix-circumfix-p pe)))
    (-do-suffixes-of (se dict word)
      (and (-suffix-cross-product se)
           (not (-suffix-not-valid-p mode se))
           (let* ((se-cont (-suffix-cont-flags se))
                  (has-needaffix-se (-flags-contain se-cont (-dict-need-affix-flag dict))))
             (and (not (and has-needaffix-pe has-needaffix-se))
                  (eq circumfix-pe (-suffix-circumfix-p se))
                  (-suffix-condition-matches-p se word)
                  (let ((root (-suffix-to-root se word)))
                    (named-let recur ((tail (-homonyms dict root)))
                      (when tail
                        (or (let ((flags (car tail)))
                              (let* ((valid-pe-outer
                                      (and (not has-needaffix-pe)
                                           (-cross-valid-p flags (-suffix-flag se))
                                           (or (-cross-valid-p se-cont (-prefix-flag pe))
                                               (-cross-valid-p flags (-prefix-flag pe)))))
                                     (valid-se-outer
                                      (and (not has-needaffix-se)
                                           (-cross-valid-p flags (-prefix-flag pe))
                                           (or (-cross-valid-p pe-cont (-suffix-flag se))
                                               (-cross-valid-p flags (-suffix-flag se))))))
                                (and (or valid-pe-outer valid-se-outer)
                                     (not (and (eq mode :full-word)
                                               (-flags-contain flags (-dict-compound-onlyin-flag dict))))
                                     (not (and skip-hidden (-hidden-homonym-p flags)))
                                     (or (-valid-inside-compound-p dict mode flags)
                                         (-valid-inside-compound-p dict mode se-cont)
                                         (-valid-inside-compound-p dict mode pe-cont))
                                     (cons root flags))))
                            (recur (cdr tail))))))))))))

(defun -strip-suffix-then-suffix (dict word skip-hidden)
;; Analyze WORD as ROOT+SE2+SE1, two suffixes with SE1 outer.  This
;; strips SE1 and lets `-strip-sfx-then-sfx-2' do the rest.  Only a
;; suffix whose flag is among some suffix's continuation flags can be
;; SE1.
  (and
   (-dict-suffix-cont-flags dict)
   (-do-suffixes-of (se1 dict word)
     (and (-has-cont-flag-p (-dict-suffix-cont-flags dict) (-suffix-flag se1))
          (not (or (-outer-suffix-not-valid-p :full-word se1)
                   (-suffix-circumfix-p se1)))
          (-suffix-condition-matches-p se1 word)
          (let ((root1 (-suffix-to-root se1 word)))
            (-strip-sfx-then-sfx-2 dict se1 root1 skip-hidden))))))

(defun -strip-sfx-then-sfx-2 (dict se1 word skip-hidden)
;; Second step of `-strip-suffix-then-suffix': WORD is the original
;; word with outer suffix SE1 already stripped.  Find an inner suffix
;; SE2 allowing SE1, such that WORD is ROOT+SE2 for a dictionary entry
;; ROOT allowing SE2.
  (-do-suffixes-of (se2 dict word)
    (and (-cross-valid-p (-suffix-cont-flags se2) (-suffix-flag se1))
         (not (or (-suffix-not-valid-p :full-word se2)
                  (-suffix-circumfix-p se2)))
         (-suffix-condition-matches-p se2 word)
         (let ((root (-suffix-to-root se2 word)))
           (named-let recur ((tail (-homonyms dict root)))
             (when tail
               (or (let ((flags (car tail)))
                     (and (-cross-valid-p flags (-suffix-flag se2))
                          (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                          (not (and skip-hidden (-hidden-homonym-p flags)))
                          (cons root flags)))
                   (recur (cdr tail)))))))))

(defun -strip-prefix-then-2-suffixes (dict word skip-hidden)
;; Analyze WORD as PE1+ROOT+SE2+SE1, with suffix SE2 inner to SE1.
;; This strips PE1 and then SE1, which must agree on CIRCUMFIX, and
;; lets `-strip-pfx-2-sfx-3' do the rest.
  (and
   (-dict-suffix-cont-flags dict)
   (-do-prefixes-of (pe1 dict word)
     (and (-prefix-cross-product pe1)
          (not (-outer-prefix-not-valid-p :full-word pe1))
          (-prefix-condition-matches-p pe1 word)
          (let ((root-p (-prefix-to-root pe1 word)))
            (-do-suffixes-of (se1 dict root-p)
              (and (-has-cont-flag-p (-dict-suffix-cont-flags dict) (-suffix-flag se1))
                   (-suffix-cross-product se1)
                   (not (-suffix-not-valid-p :full-word se1))
                   (eq (-prefix-circumfix-p pe1)
                       (-suffix-circumfix-p se1))
                   (-suffix-condition-matches-p se1 root-p)
                   (let ((root1 (-suffix-to-root se1 root-p)))
                     (-strip-pfx-2-sfx-3 dict pe1 se1 root1 skip-hidden)))))))))

(defun -strip-pfx-2-sfx-3 (dict pe1 se1 word skip-hidden)
;; Last step of `-strip-prefix-then-2-suffixes': WORD is the original
;; word with prefix PE1 and outer suffix SE1 already stripped.  Find
;; an inner suffix SE2 allowing SE1, such that WORD is ROOT+SE2 for a
;; dictionary entry ROOT allowing SE2, and with PE1 allowed by either
;; SE1's continuation flags or ROOT.
  (-do-suffixes-of (se2 dict word)
    (and (-cross-valid-p (-suffix-cont-flags se2) (-suffix-flag se1))
         (not (or (-suffix-not-valid-p :full-word se2)
                  (-suffix-circumfix-p se2)))
         (-suffix-condition-matches-p se2 word)
         (let ((root (-suffix-to-root se2 word)))
           (named-let recur ((tail (-homonyms dict root)))
             (when tail
               (or (let ((flags (car tail)))
                     (and (or (-cross-valid-p (-suffix-cont-flags se1) (-prefix-flag pe1))
                              (-cross-valid-p flags (-prefix-flag pe1)))
                          (-cross-valid-p flags (-suffix-flag se2))
                          (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                          (not (and skip-hidden (-hidden-homonym-p flags)))
                          (cons root flags)))
                   (recur (cdr tail)))))))))

(defun -strip-suffix-prefix-suffix (dict word skip-hidden)
;; Analyze WORD as PE1+ROOT+SE2+SE1, where SE1 is outermost, but
;; unlike `-strip-prefix-then-2-suffixes', SE1 may have been added on
;; top of PE1 instead of SE2.  This strips SE1 and then PE1, and lets
;; `-strip-s-p-s-3' do the rest.
  (and
   (or (-dict-suffix-cont-flags dict) (-dict-prefix-cont-flags dict))
   (-do-suffixes-of (se1 dict word)
     (and (or (-has-cont-flag-p (-dict-suffix-cont-flags dict) (-suffix-flag se1))
              (-has-cont-flag-p (-dict-prefix-cont-flags dict) (-suffix-flag se1)))
          (-suffix-cross-product se1)
          (not (-outer-suffix-not-valid-p :full-word se1))
          (-suffix-condition-matches-p se1 word)
          (let ((root-s (-suffix-to-root se1 word)))
            (-do-prefixes-of (pe1 dict root-s)
              (and (-prefix-cross-product pe1)
                   (not (-prefix-not-valid-p :full-word pe1))
                   (-prefix-condition-matches-p pe1 root-s)
                   (let ((root1 (-prefix-to-root pe1 root-s)))
                     (-strip-s-p-s-3 dict se1 pe1 root1 skip-hidden)))))))))

(defun -strip-s-p-s-3 (dict se1 pe1 word skip-hidden)
;; Last step of `-strip-suffix-prefix-suffix': WORD is the original
;; word with outer suffix SE1 and prefix PE1 already stripped.  Find a
;; suffix SE2 such that WORD is ROOT+SE2 for a dictionary entry ROOT
;; allowing SE2, with SE1 allowed by either SE2's or PE1's
;; continuation flags, and PE1 by either SE2's continuation flags or
;; ROOT.  For CIRCUMFIX, PE1 must agree with one of the suffixes and
;; the other one must not have it.
  (-do-suffixes-of (se2 dict word)
    (and (-suffix-cross-product se2)
         (or (-cross-valid-p (-suffix-cont-flags se2) (-suffix-flag se1))
             (-cross-valid-p (-prefix-cont-flags pe1) (-suffix-flag se1)))
         (not (-suffix-not-valid-p :full-word se2))
         (let* ((circ-pe1 (-prefix-circumfix-p pe1))
                (circ-se1 (-suffix-circumfix-p se1))
                (circ-se2 (-suffix-circumfix-p se2))
                (ok1 (and (eq circ-pe1 circ-se1) (not circ-se2)))
                (ok2 (and (eq circ-pe1 circ-se2) (not circ-se1))))
           (and (or ok1 ok2)
                (-suffix-condition-matches-p se2 word)
                (let ((root (-suffix-to-root se2 word)))
                  (named-let recur ((tail (-homonyms dict root)))
                    (when tail
                      (or (let ((flags (car tail)))
                            (and (or (-cross-valid-p (-suffix-cont-flags se2) (-prefix-flag pe1))
                                     (-cross-valid-p flags (-prefix-flag pe1)))
                                 (-cross-valid-p flags (-suffix-flag se2))
                                 (not (-flags-contain flags (-dict-compound-onlyin-flag dict)))
                                 (not (and skip-hidden (-hidden-homonym-p flags)))
                                 (cons root flags)))
                          (recur (cdr tail)))))))))))

;;;; Simple (non-compound) word checking

(defun -check-simple-word (dict word skip-hidden)
;; Check WORD as a single (possibly affixed) dictionary entry.  Return
;; (ROOT-WORD . FLAGS) or nil; see the affix-stripping functions above
;; for the shape of the result.
  (or (named-let recur ((tail (-homonyms dict word)))
        (when tail
          (or (let ((flags (car tail)))
                (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                              (-flags-contain flags (-dict-compound-onlyin-flag dict))
                              (and skip-hidden (-hidden-homonym-p flags))))
                     (cons word flags)))
              (recur (cdr tail)))))
      (-strip-suffix-only dict :full-word word skip-hidden)
      (-strip-prefix-only dict :full-word word skip-hidden)
      (-strip-prefix-then-suffix-commutative dict :full-word word skip-hidden)
      (-strip-suffix-then-suffix dict word skip-hidden)
      (-strip-prefix-then-2-suffixes dict word skip-hidden)
      (-strip-suffix-prefix-suffix dict word skip-hidden)))

;;;; Compound checking

(defun -check-word-in-compound (dict mode word)
;; Check WORD as one part of a compound, in position MODE.  MODE is
;; :compound-begin, :compound-middle or :compound-end.
  (let ((cpd-flag (cond ((eq mode :compound-begin) (-dict-compound-begin-flag dict))
                        ((eq mode :compound-middle) (-dict-compound-middle-flag dict))
                        ((eq mode :compound-end) (-dict-compound-last-flag dict))
                        (t 0))))
    (or (named-let recur ((tail (-homonyms dict word)))
          (when tail
            (or (let ((flags (car tail)))
                  (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                                (not (or (-flags-contain flags (-dict-compound-flag dict))
                                         (-flags-contain flags cpd-flag)))
                                (-hidden-homonym-p flags)))
                       (cons word flags)))
                (recur (cdr tail)))))
        (-strip-suffix-only dict mode word t)
        (-strip-prefix-only dict mode word t)
        (-strip-prefix-then-suffix-commutative dict mode word t))))

(defun -char-alpha-p (ch) (memq (get-char-code-property ch 'general-category)
                                '(Ll Lu Lo Lm Lt)))
(defun -char-upper-p (ch) (/= ch (downcase ch)))
(defun -char-lower-p (ch) (/= ch (upcase ch)))

(defun -uppercase-at-compound-boundary-p (word i)
;; Return non-nil if WORD has a letter case change straddling index I.
  (let ((before (and (> i 0) (aref word (1- i))))
        (after (and (< i (length word)) (aref word i))))
    (or (and after (-char-upper-p after) before (-char-alpha-p before))
        (and before (-char-upper-p before) after (-char-alpha-p after)))))

(defun -three-equal-at-boundary-p (word i)
;; Return non-nil if three identical characters straddle index I in
;; WORD.
  (when (and (> i 0) (< i (length word)))
    (let ((b (aref word (1- i))) (c (aref word i)))
      (when (eq b c)
        (or (and (< (1+ i) (length word)) (eq c (aref word (1+ i))))
            (and (> (1- i) 0) (eq (aref word (- i 2)) b)))))))

(defun -rep-similar-p (dict word)
;; Return non-nil if some REP replacement turns WORD into a correct
;; simple word, i.e. WORD is likely a misspelling of that word rather
;; than a compound.  Mirrors Nuspell's `Checker::is_rep_similar'.
  (or (named-let recur ((tail (-dict-rep-whole dict)))
        (when tail
          (or (let ((r (car tail)))
                (and (string= word (car r))
                     (-check-simple-word dict (cdr r) t)))
              (recur (cdr tail)))))
      (named-let recur ((tail (-dict-rep-start dict)))
        (when tail
          (or (let ((r (car tail)))
                (and (string-prefix-p (car r) word)
                     (-check-simple-word
                      dict (concat (cdr r) (substring word (length (car r)))) t)))
              (recur (cdr tail)))))
      (named-let recur ((tail (-dict-rep-end dict)))
        (when tail
          (or (let ((r (car tail)))
                (and (string-suffix-p (car r) word)
                     (-check-simple-word
                      dict (concat (substring word 0 (- (length word) (length (car r)))) (cdr r)) t)))
              (recur (cdr tail)))))
      (named-let recur ((tail (-dict-rep-any dict)))
        (when tail
          (or (let ((r (car tail)))
                (let* ((from (car r))
                       (i (and (not (string-empty-p from)) (string-search from word)))
                       (found nil))
                  (while (and i (not found))
                    (setq found (-check-simple-word
                                 dict (concat (substring word 0 i) (cdr r)
                                              (substring word (+ i (length from))))
                                 t))
                    (setq i (string-search from word (1+ i))))
                  found))
              (recur (cdr tail)))))))

(defun -check-compound-classic (dict mode word start-pos i num-part allow-bad-forceucase)
;; Try to split WORD[START-POS:] as a compound with its first part
;; ending at I.  Returns the dictionary entry (ROOT-WORD . FLAGS) for
;; the first part on success, or nil.  MODE is :compound-begin or
;; :compound-middle, matching the position of the first part.
  (-check-time-limit)
  (let* ((part1-text (substring word start-pos i))
         (part1-entry (-check-word-in-compound dict mode part1-text)))
    (and part1-entry
         (not (-flags-contain (cdr part1-entry) (-dict-forbiddenword-flag dict)))
         (not (and (-dict-compound-check-triple dict) (-three-equal-at-boundary-p word i)))
         (not (and (-dict-compound-check-case dict) (-uppercase-at-compound-boundary-p word i)))
         (let* ((root-flag (-dict-compound-root-flag dict))
                (num-part1 (+ num-part (if (-flags-contain (cdr part1-entry) root-flag) 1 0)))
                (part2-text (substring word i))
                (part2-entry (-check-word-in-compound dict :compound-end part2-text))
                (success2
                 (and part2-entry
                      (not (-flags-contain (cdr part2-entry) (-dict-forbiddenword-flag dict)))
                      (not (-compound-pattern-forbids-p dict word i part1-text part1-entry part2-entry))
                      (not (and (-dict-compound-check-duplicate dict)
                                (eq (cdr part1-entry) (cdr part2-entry))))
                      (not (and (-dict-compound-check-rep dict)
                                (-rep-similar-p dict (substring word start-pos))))
                      (not (and (/= 0 (-dict-compound-force-uppercase dict))
                                (not allow-bad-forceucase)
                                (-flags-contain (cdr part2-entry) (-dict-compound-force-uppercase dict)))))))
           (if success2
               (let ((num-part2 (+ num-part1 (if (-flags-contain (cdr part2-entry) root-flag) 1 0))))
                 (and (not (and (/= 0 (-dict-compound-max-word-count dict))
                                (>= (1+ num-part2) (-dict-compound-max-word-count dict))))
                      part1-entry))
             (let ((part2-entry (-check-compound-split dict :compound-middle word i
                                                       (1+ num-part1) allow-bad-forceucase)))
               (and part2-entry
                    (not (-compound-pattern-forbids-p dict word i part1-text part1-entry part2-entry))
                    ;; Also try the first two parts alone, when the
                    ;; second is an unaffixed dictionary word.
                    (not (and (-dict-compound-check-rep dict)
                              (or (-rep-similar-p dict (substring word start-pos))
                                  (let ((end (+ i (length (car part2-entry)))))
                                    (and (<= end (length word))
                                         (string= (car part2-entry) (substring word i end))
                                         (-rep-similar-p dict (substring word start-pos end)))))))
                    part1-entry)))))))

(defun -check-compound-split (dict mode word start-pos num-part allow-bad-forceucase)
;; Try every split point of WORD[START-POS:] respecting COMPOUNDMIN.
  (let* ((min-len (if (/= 0 (-dict-compound-min-length dict))
                      (-dict-compound-min-length dict)
                    3))
         (len (length word)))
    (when (>= (- len start-pos) (* 2 min-len))
      (cl-loop for i from (+ start-pos min-len) to (- len min-len)
               thereis (or (-check-compound-classic dict mode word start-pos i num-part
                                                    allow-bad-forceucase)
                           (and (-dict-compound-patterns dict)
                                (-check-compound-with-pattern-replacements
                                 dict mode word start-pos i num-part allow-bad-forceucase)))))))

(defun -string-at-p (s word i)
;; Return non-nil if string S occurs in WORD at index I.
  (let ((end (+ i (length s))))
    (and (<= end (length word))
         (eq t (compare-strings s nil nil word i end)))))

(defun -compound-pattern-flag-ok-p (flag flags)
;; Return non-nil if a pattern's condition FLAG, 0 for none, is in FLAGS.
  (or (zerop flag) (-flags-contain flags flag)))

(defun -compound-pattern-forbids-p (dict word i part1-text part1-entry part2-entry)
;; Return non-nil if one of DICT's CHECKCOMPOUNDPATTERNs forbids the
;; join at index I of WORD between PART1-ENTRY, found for PART1-TEXT,
;; and PART2-ENTRY.  Mirrors Nuspell's
;; `is_compound_forbidden_by_patterns'.
  (and (-dict-compound-patterns dict)
       (any (lambda (p)
              (let ((idx (-compound-pattern-idx p)))
                (and (>= i idx)
                     (-string-at-p (-compound-pattern-text p) word (- i idx))
                     (-compound-pattern-flag-ok-p (-compound-pattern-first-flag p)
                                                  (cdr part1-entry))
                     (-compound-pattern-flag-ok-p (-compound-pattern-second-flag p)
                                                  (cdr part2-entry))
                     ;; Nuspell asks whether the first word's affix
                     ;; has a non-empty strip or append; here, that is
                     ;; whether its root differs from its text.
                     (not (and (-compound-pattern-first-unmodified p)
                               (not (string= (car part1-entry) part1-text)))))))
            (-dict-compound-patterns dict))))

(defun -check-compound-with-pattern-replacements (dict mode word start-pos i num-part
                                                       allow-bad-forceucase)
;; Like `-check-compound-classic', but for a join at index I of WORD
;; written with a CHECKCOMPOUNDPATTERN replacement, which is split as
;; if it were the pattern's text instead.  Mirrors Nuspell's
;; `check_compound_with_pattern_replacements', minus its
;; SIMPLIFIEDTRIPLE fallback.
  (named-let recur ((tail (-dict-compound-patterns dict)))
    (when tail
      (let* ((p (car tail))
             (repl (-compound-pattern-replacement p)))
        (if (not (and repl (-string-at-p repl word i)))
            (recur (cdr tail))
          (let* ((word2 (concat (substring word 0 i) (-compound-pattern-text p)
                                (substring word (+ i (length repl)))))
                 (i2 (+ i (-compound-pattern-idx p)))
                 (part1-entry (-check-word-in-compound dict mode (substring word2 start-pos i2))))
            (if (not (and part1-entry
                          (not (-flags-contain (cdr part1-entry) (-dict-forbiddenword-flag dict)))
                          (-compound-pattern-flag-ok-p (-compound-pattern-first-flag p)
                                                       (cdr part1-entry))
                          (not (and (-dict-compound-check-triple dict)
                                    (-three-equal-at-boundary-p word2 i2)))))
                (recur (cdr tail))
              (let ((part2-entry (-check-word-in-compound dict :compound-end (substring word2 i2))))
                (if (and part2-entry
                         (not (-flags-contain (cdr part2-entry) (-dict-forbiddenword-flag dict)))
                         (-compound-pattern-flag-ok-p (-compound-pattern-second-flag p)
                                                      (cdr part2-entry))
                         (not (and (-dict-compound-check-duplicate dict)
                                   (eq (cdr part1-entry) (cdr part2-entry))))
                         (not (and (-dict-compound-check-rep dict)
                                   (-rep-similar-p dict (substring word start-pos))))
                         (not (and (/= 0 (-dict-compound-force-uppercase dict))
                                   (not allow-bad-forceucase)
                                   (-flags-contain (cdr part2-entry)
                                                   (-dict-compound-force-uppercase dict)))))
                    ;; As in Nuspell, too many parts ends the search
                    ;; at this join, without trying further patterns.
                    (and (not (and (/= 0 (-dict-compound-max-word-count dict))
                                   (>= (1+ num-part) (-dict-compound-max-word-count dict))))
                         part1-entry)
                  (let ((part2-entry (-check-compound-split dict :compound-middle word2 i2
                                                            (1+ num-part) allow-bad-forceucase)))
                    (if (and part2-entry
                             (-compound-pattern-flag-ok-p (-compound-pattern-second-flag p)
                                                          (cdr part2-entry))
                             (not (and (-dict-compound-check-rep dict)
                                       (or (-rep-similar-p dict (substring word start-pos))
                                           (and (-string-at-p (car part2-entry) word2 i2)
                                                (-rep-similar-p
                                                 dict (substring word2 start-pos
                                                                 (+ i2 (length (car part2-entry))))))))))
                        part1-entry
                      (recur (cdr tail)))))))))))))

(defun -compound-rules-has-any-flag-p (dict flags)
  (any (lambda (f) (-flags-contain flags f)) (-dict-compound-rules-all-flags dict)))

(defun -find-compound-rule-word (dict word)
  (named-let recur ((tail (-homonyms dict word)))
    (when tail
      (or (let ((flags (car tail)))
            (and (not (or (-flags-contain flags (-dict-need-affix-flag dict))
                          (not (-compound-rules-has-any-flag-p dict flags))))
                 (cons word flags)))
          (recur (cdr tail))))))

(defun -match-compound-rule-1 (data pattern)
;; Match flag lists DATA (one per word part) against a compound-rule
;; PATTERN.  PATTERN is a list of (FLAG . QUANT), QUANT one of nil, ??
;; or ?*.
  (pcase pattern
    ('nil (null data))
    (`((,flag . ,??) . ,rest-pat)
     (or (-match-compound-rule-1 data rest-pat)
         (and data (-flags-contain (car data) flag)
              (-match-compound-rule-1 (cdr data) rest-pat))))
    (`((,flag . ,?*) . ,rest-pat)
     (or (-match-compound-rule-1 data rest-pat)
         (and data (-flags-contain (car data) flag)
              (-match-compound-rule-1 (cdr data) pattern))))
    (`((,flag . ,_) . ,rest-pat)
     (and data (-flags-contain (car data) flag)
          (-match-compound-rule-1 (cdr data) rest-pat)))))

(defun -match-compound-rules (dict words-data)
  (any (lambda (pattern) (-match-compound-rule-1 words-data pattern))
       (-dict-compound-rules dict)))

(defun -check-compound-with-rules (dict word words-data start-pos allow-bad-forceucase)
;; Try to split WORD[START-POS:] according to COMPOUNDRULE patterns.
;; WORDS-DATA is the flag lists of the ancestor parts already matched,
;; most recent first.
  (let* ((min-len (if (/= 0 (-dict-compound-min-length dict))
                      (-dict-compound-min-length dict)
                    3))
         (len (length word)))
    (when (>= (- len start-pos) (* 2 min-len))
      (cl-loop
       for i from (+ start-pos min-len) to (- len min-len)
       thereis
       (progn
         (-check-time-limit)
         (let* ((part1-text (substring word start-pos i))
                (part1-entry (-find-compound-rule-word dict part1-text)))
           (and part1-entry
                (let* ((words-data1 (cons (cdr part1-entry) words-data))
                       (part2-text (substring word i))
                       (part2-entry (-find-compound-rule-word dict part2-text)))
                  (or (and part2-entry
                           (let ((words-data2 (cons (cdr part2-entry) words-data1)))
                             (and (-match-compound-rules dict (reverse words-data2))
                                  (not (and (/= 0 (-dict-compound-force-uppercase dict))
                                            (not allow-bad-forceucase)
                                            (-flags-contain (cdr part2-entry)
                                                            (-dict-compound-force-uppercase dict))))
                                  part1-entry)))
                      (-check-compound-with-rules dict word words-data1 i allow-bad-forceucase))))))))))

(defun -check-compound (dict word allow-bad-forceucase)
  (or (and (or (/= 0 (-dict-compound-flag dict))
               (/= 0 (-dict-compound-begin-flag dict))
               (/= 0 (-dict-compound-middle-flag dict))
               (/= 0 (-dict-compound-last-flag dict)))
           (-check-compound-split dict :compound-begin word 0 0 allow-bad-forceucase))
      (and (-dict-compound-rules dict)
           (-check-compound-with-rules dict word nil 0 allow-bad-forceucase))))

(defun -check-word (dict word &optional allow-bad-forceucase skip-hidden)
;; Check WORD as a simple word or as a compound.  Return (MATCHED-WORD
;; . FLAGS) or nil; FLAGS may itself be nil for a dictionary entry
;; with no flags at all, so nil here means WORD was not found -- never
;; confuse this with a found entry's (possibly nil) FLAGS, as `cdr' of
;; the result would.
  (or (-check-simple-word dict word skip-hidden)
      (-check-compound dict word allow-bad-forceucase)))

;;;; Casing

(defun -to-title (s)
;; Return S with its first character title-cased and the rest
;; downcased, e.g. "o'NEIL" -> "O'neil".  S is treated as a single word, unlike
;; `capitalize' and `upcase-initials', which would give "O'Neil".
;; This mirrors Nuspell's `to_title', i.e. ICU's toTitle with
;; U_TITLECASE_WHOLE_STRING, used to try the dictionary's own
;; capitalization of a word seen in all caps or with an initial
;; capital.  Title-casing the first character via `capitalize',
;; rather than `upcase', matters for some characters: ǆ gives ǅ (not
;; Ǆ) and ß gives Ss (not SS).
  (if (string-empty-p s)
      s
    (concat (capitalize (substring s 0 1)) (downcase (substring s 1)))))

(defun -classify-casing (s)
;; Classify S as `small', `init-capital', `all-capital', `pascal' or
;; `camel'.
  (let ((case-fold-search nil)
        (upper-rx "[[:upper:]]+")
        (lower-rx "[[:lower:]]"))
    (if (not (string-match upper-rx s))
        'small
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (if (> beg 0)
            ;; First character isn't upper-case, so `init-capital' and
            ;; `pascal' (which both require that) are out; it's
            ;; `all-capital' if there's no lower-case letter anywhere,
            ;; `camel' otherwise.
            (if (string-match-p lower-rx s) 'camel 'all-capital)
          ;; First character is upper-case.  There's exactly one
          ;; upper-case character in all of S iff this leading run is
          ;; a single character and no further one follows it --
          ;; checking the latter can start right after the run instead
          ;; of rescanning it.
          (if (and (= end 1) (not (string-match-p upper-rx s end)))
              'init-capital
            (if (string-match-p lower-rx s) 'pascal 'all-capital)))))))

(defun -spell-casing (dict s)
;; Dispatch S to the right casing-aware check for DICT.  A leading
;; lower-case letter already rules out `all-capital' and
;; `init-capital' (both require an upper-case first character),
;; leaving only `small'/`camel'/`pascal', which all go to
;; `-check-word' anyway -- so most words (ordinary running text) skip
;; `-classify-casing' entirely.
  (if (or (string-empty-p s) (-char-lower-p (aref s 0)))
      (-check-word dict s)
    (pcase (-classify-casing s)
      ((or 'small 'camel 'pascal) (-check-word dict s))
      ('all-capital (-spell-casing-upper dict s))
      ('init-capital (-spell-casing-title dict s)))))

(defun -spell-casing-upper (dict s)
  (or (-check-word dict s t)
      ;; Apostrophe-separated prefix, e.g. Catalan/French/Italian
      ;; SANT'ELIA -> Sant'+Elia.
      (let ((apos (cl-position ?' s)))
        (and apos (/= apos (1- (length s)))
             (let* ((part1 (downcase (substring s 0 (1+ apos))))
                    (part2 (-to-title (substring s (1+ apos)))))
               (or (-check-word dict (concat part1 part2) t)
                   (progn
                     (setq part1 (-to-title part1))
                     (-check-word dict (concat part1 part2) t))))))
      ;; German sharp s.
      (and (-dict-checksharps dict) (string-search "SS" s)
           (or (-spell-sharps dict (downcase s))
               (-spell-sharps dict (-to-title s))))
      (let* ((s2 (-to-title s))
             (res (-check-word dict s2 t)))
        (unless (-flags-contain (cdr res) (-dict-keepcase-flag dict)) res))
      (let* ((s2 (downcase s))
             (res (-check-word dict s2 t)))
        (unless (-flags-contain (cdr res) (-dict-keepcase-flag dict)) res))))

(defun -spell-casing-title (dict s)
  (or (-check-word dict s t t)
      (let* ((s2 (downcase s))
             (res (-check-word dict s2 t)))
        (unless (and (-flags-contain (cdr res) (-dict-keepcase-flag dict))
                     (not (and (-dict-checksharps dict) (string-search "ß" s2))))
          res))))

(defun -spell-sharps (dict base &optional pos n rep)
;; Recursively try replacing "ss" with "ß" in BASE, German-style.
  (setq pos (or pos 0) n (or n 0) rep (or rep 0))
  (let ((max-sharps 5)
        (found (string-search "ss" base pos)))
    (if (and found (< n max-sharps))
        (or (let ((replaced (concat (substring base 0 found) "ß"
                                    (substring base (+ found 2)))))
              (-spell-sharps dict replaced (1+ found) (1+ n) (1+ rep)))
            (-spell-sharps dict base (+ found 2) (1+ n) rep))
      (and (> rep 0)
           (-check-word dict base t)))))

;;;; Word breaking (BREAK) and top-level checking

(defun -spell-break (dict s depth)
  (let ((res (-spell-casing dict s)))
    (if res
        (let ((flags (cdr res)))
          (not (or (-flags-contain flags (-dict-forbiddenword-flag dict))
                   (and (-dict-forbid-warn dict)
                        (-flags-contain flags (-dict-warn-flag dict))))))
      ;; Give up after 9 nested BREAK splits.  This is the same limit
      ;; as Nuspell's `Checker::spell_break', where it is a bare
      ;; literal; Hunspell's MAXBREAKDEPTH is 10.  It bounds the
      ;; recursion, which may otherwise branch for every pattern.
      (and (/= depth 9)
           (or (named-let recur ((tail (-dict-break-start dict)))
                 (when tail
                   (or (let ((pat (car tail)))
                         (and (string-prefix-p pat s)
                              (-spell-break dict (substring s (length pat)) (1+ depth))))
                       (recur (cdr tail)))))
               (named-let recur ((tail (-dict-break-end dict)))
                 (when tail
                   (or (let ((pat (car tail)))
                         (and (string-suffix-p pat s)
                              (-spell-break dict (substring s 0 (- (length s) (length pat)))
                                            (1+ depth))))
                       (recur (cdr tail)))))
               (named-let recur ((tail (-dict-break-middle dict)))
                 (when tail
                   (or (let ((pat (car tail)))
                         (and (not (string-empty-p pat))
                              (let ((i (string-search pat s)))
                                (and i (> i 0) (< i (- (length s) (length pat)))
                                     (-spell-break dict (substring s 0 i) (1+ depth))
                                     (-spell-break dict (substring s (+ i (length pat)))
                                                   (1+ depth))))))
                       (recur (cdr tail))))))))))

(defun -erase-chars (s chars)
  (if (string-empty-p chars)
      s
    (let (result)
      (dotimes (i (length s))
        (let ((c (aref s i)))
          (unless (cl-position c chars) (push c result))))
      (concat (nreverse result)))))

(defun -string-last-non-dot (s)
;; Return 1+ the index of the last non-dot character of S (0 if none).
  (let ((i (length s)))
    (while (and (> i 0) (eq (aref s (1- i)) ?.))
      (setq i (1- i)))
    i))

(defun -spell (dict word)
  (setq word (-substr-replace (-dict-input-iconv dict) word))
  (or (string-empty-p word)
      (let ((abbreviation (eq (aref word (1- (length word))) ?.)))
        (when abbreviation
          (setq word (substring word 0 (-string-last-non-dot word))))
        (or (string-empty-p word)
            ;; A number, possibly with . , - separators, is correct.
            (and (string-match-p (rx bos (? "-") (+ digit) (* (any ".,-") (+ digit)) eos)
                                 word)
                 t)
            (progn
              (setq word (-erase-chars word (-dict-ignored-chars dict)))
              (let ((ret (-spell-break dict word 0)))
                (if (and (not ret) abbreviation)
                    (-spell-break dict (concat word ".") 0)
                  ret)))))))

;;;; Suggestions
;;
;; This is a port of Nuspell's `Suggester', which builds a list of
;; candidate corrections by running many small, independent "one
;; typo" heuristics (`-suggest-low' and its callees) plus a slower
;; dictionary-wide n-gram similarity search (`-ngram-suggest'), then
;; cleans up and reorders the result.  Nuspell's version operates on
;; UTF-8 byte offsets because C++ strings are byte sequences; Emacs
;; strings are already sequences of characters, so this port indexes
;; by character throughout and has no equivalent of Nuspell's
;; UTF-8-boundary bookkeeping.  Likewise, affixing/stripping here
;; builds fresh strings with `substring'/`concat' instead of mutating
;; a buffer in place and reverting it afterwards, since Emacs strings
;; are not mutated in length the way C++ strings are; this is simpler
;; at the cost of some allocation, which is acceptable since
;; suggestions, unlike checking, are not expected to run in a tight
;; loop.

;;;; Suggestion accumulator

(cl-defstruct (-sugs-box (:constructor -sugs-box-create))
;; Mutable suggestion list, built up by the various `-*-suggest'
;; functions.  Mirrors passing a `List_Strings&' around in Nuspell;
;; HEAD/TAIL support O(1) `-sugs-push' (appending, the common case),
;; and COUNT is kept in sync so callers do not need to walk the list
;; to learn its length.
  (head nil :type list)
  (tail nil :type list)                ; last cons of HEAD
  (count 0 :type natnum))

(defun -sugs-push (box word)
;; Append WORD to the end of BOX's suggestion list.
  (let ((cell (cons word nil)))
    (if (-sugs-box-tail box)
        (setcdr (-sugs-box-tail box) cell)
      (setf (-sugs-box-head box) cell))
    (setf (-sugs-box-tail box) cell))
  (incf (-sugs-box-count box)))

(defun -sugs-push-first (box word)
;; Prepend WORD to BOX's suggestion list.
  (let ((cell (cons word (-sugs-box-head box))))
    (setf (-sugs-box-head box) cell)
    (unless (-sugs-box-tail box)
      (setf (-sugs-box-tail box) cell)))
  (incf (-sugs-box-count box)))

(defun -sugs-list (box)
;; Return BOX's current suggestion list, in order.
  (-sugs-box-head box))

(defun -sugs-box-set (box list)
;; Replace BOX's suggestion list wholesale with LIST.
  (setf (-sugs-box-head box) list)
  (setf (-sugs-box-tail box) (last list))
  (setf (-sugs-box-count box) (length list)))

(defun -add-sug-if-correct (dict word box)
;; Push WORD onto BOX if it is a correct, suggestible word of DICT.
;; Return non-nil iff it was pushed.  Mirrors Nuspell's
;; `Suggester::add_sug_if_correct'.
  (-check-time-limit)
  (let ((res (-check-word dict word nil t)))
    (and res
         (not (-flags-contain (cdr res) (-dict-forbiddenword-flag dict)))
         (not (and (-dict-forbid-warn dict) (-flags-contain (cdr res) (-dict-warn-flag dict))))
         (progn (-sugs-push box word) t))))

;;;; Attempt budget, for the algorithms below that are otherwise O(word
;;;; length * affix table size) or worse

;; Count of remaining suggestion attempts.  Each algorithm below that
;; is capped by it let-binds it to `-max-attempts-for-long-algos'; it
;; has no global value, so a use outside such a binding is an error.
(defvar -budget)

(defun -budget-consume ()
;; Consume one unit of `-budget'; return non-nil if one was available.
  (when (plusp -budget)
    (decf -budget)
    t))

(defun -max-attempts-for-long-algos (dict word)
;; Return DICT's suggestion-attempt budget for WORD.  A heuristic cap,
;; based on the size of DICT's affix tables, on how many candidates
;; the pricier per-position suggestion heuristics below may try;
;; without it, a dictionary with unusually many affixes could make
;; suggesting take an impractically long time.  Mirrors Nuspell's
;; `Suggester::max_attempts_for_long_alogs'.  Unlike Nuspell, there is
;; no COMPLEXPREFIXES branch, since `elspell-create' already
;; rejects such dictionaries; see the Commentary at the top of this
;; file.
  (let* ((p (/ (length (-dict-prefix-table dict)) 20))
         (s (/ (length (-dict-suffix-table dict)) 20))
         (cost (+ 1 p s (* p s) (* s s) (* 2 p s s)))
         (cost (max 250000 (min cost 25000000000)))
         (ret (/ 25000000000 cost)))
    (if (and (or (/= 0 (-dict-compound-flag dict))
                 (/= 0 (-dict-compound-begin-flag dict))
                 (/= 0 (-dict-compound-last-flag dict))
                 (/= 0 (-dict-compound-middle-flag dict)))
             (> (length word) 0))
        (/ ret (length word))
      ret)))

;;;; Small string-surgery helpers shared by the heuristics below

(defun -replace-char-at (word i rep)
;; Return WORD with its character at index I replaced by string REP.
  (concat (substring word 0 i) rep (substring word (1+ i))))

(defun -swap-adjacent (word i)
;; Return WORD with the characters at indices I and I+1 swapped.
  (concat (substring word 0 i)
          (substring word (1+ i) (+ i 2))
          (substring word i (1+ i))
          (substring word (+ i 2))))

(defun -swap-chars (word i j)
;; Return WORD with the characters at indices I and J (I < J) swapped.
  (concat (substring word 0 i)
          (substring word j (1+ j))
          (substring word (1+ i) j)
          (substring word i (1+ i))
          (substring word (1+ j))))

(defun -swap-two-pairs (word i j)
;; Return WORD with (I . I+1) and (J . J+1) each swapped (I+2 <= J).
  (concat (substring word 0 i)
          (substring word (1+ i) (+ i 2))
          (substring word i (1+ i))
          (substring word (+ i 2) j)
          (substring word (1+ j) (+ j 2))
          (substring word j (1+ j))
          (substring word (+ j 2))))

(defun -move-char-forward (word i j)
;; Return WORD with the character at index I moved to just after J (J
;; > I).
  (concat (substring word 0 i)
          (substring word (1+ i) (1+ j))
          (substring word i (1+ i))
          (substring word (1+ j))))

(defun -move-char-backward (word i j)
;; Return WORD with the character at index I moved to index J (J < I).
  (concat (substring word 0 j)
          (substring word i (1+ i))
          (substring word j i)
          (substring word (1+ i))))

(defun -to-title-char-at (s i)
;; Return S with its character at index I upcased; the rest is
;; unchanged.
  (concat (substring s 0 i) (string (upcase (aref s i))) (substring s (1+ i))))

;;;; Single-typo suggestion heuristics
;;
;; Each of these mirrors one of Nuspell's `Suggester::*_suggest'
;; member functions: given WORD (assumed already a correctly-cased
;; input, or lower-cased by the caller) and BOX, it tries variations
;; of WORD that correct one commonly-made kind of typing mistake, and
;; pushes each one that turns out to be a correctly spelled word of
;; DICT onto BOX via `-add-sug-if-correct'.

(defun -uppercase-suggest (dict word box)
  (-add-sug-if-correct dict (upcase word) box))

(defun -try-rep-suggestion (dict candidate box)
;; Push CANDIDATE onto BOX if correct, or if every space-separated
;; part is.  Mirrors Nuspell's `Suggester::try_rep_suggestion'.
  (unless (-add-sug-if-correct dict candidate box)
    (when (and (string-search " " candidate)
               (all (lambda (part) (-check-word dict part nil t))
                    (split-string candidate " " t)))
      (-sugs-push box candidate))))

(defun -rep-suggest (dict word box)
;; Try DICT's REP-table replacements (whole-word, prefix, suffix,
;; any-place).  Mirrors Nuspell's `Suggester::rep_suggest'.
  (dolist (r (-dict-rep-whole dict))
    (when (string= word (car r))
      (-try-rep-suggestion dict (cdr r) box)))
  (dolist (r (-dict-rep-start dict))
    (when (string-prefix-p (car r) word)
      (-try-rep-suggestion dict (concat (cdr r) (substring word (length (car r)))) box)))
  (dolist (r (-dict-rep-end dict))
    (when (string-suffix-p (car r) word)
      (-try-rep-suggestion
       dict (concat (substring word 0 (- (length word) (length (car r)))) (cdr r)) box)))
  (dolist (r (-dict-rep-any dict))
    (let* ((from (car r))
           (to (cdr r))
           (i (and (not (string-empty-p from)) (string-search from word))))
      (while i
        (-try-rep-suggestion
         dict (concat (substring word 0 i) to (substring word (+ i (length from)))) box)
        (setq i (string-search from word (1+ i)))))))

(defun -map-suggest-try (dict word box i old-len rep)
;; Replace WORD[I, I+OLD-LEN) with REP, as one `-map-suggest' attempt.
  (when (-budget-consume)
    (let ((candidate (concat (substring word 0 i) rep (substring word (+ i old-len)))))
      (-add-sug-if-correct dict candidate box)
      (-map-suggest-1 dict candidate box (+ i (length rep))))))

(defun -map-suggest-try-group (dict word box i grp)
;; Try GRP's character/string equivalences at WORD's position I.  See
;; `-map-suggest-1'.
  (let* ((chars (-similarity-group-chars grp))
         (strings (-similarity-group-strings grp))
         (cp (aref word i))
         (j (cl-position cp chars)))
    (when j
      (dotimes (k (length chars))
        (unless (= k j)
          (-map-suggest-try dict word box i 1 (substring chars k (1+ k)))))
      (dolist (r strings)
        (-map-suggest-try dict word box i 1 r)))
    (dolist (f strings)
      (when (and (<= (+ i (length f)) (length word))
                 (string= f (substring word i (+ i (length f)))))
        (dotimes (k (length chars))
          (-map-suggest-try dict word box i (length f) (substring chars k (1+ k))))
        (dolist (r strings)
          (unless (string= r f)
            (-map-suggest-try dict word box i (length f) r)))))))

(defun -map-suggest-1 (dict word box i)
;; Try MAP (similarity-group) substitutions in WORD from index I
;; onward.
  (when (and (< i (length word)) (plusp -budget))
    (dolist (grp (-dict-similarities dict))
      (-map-suggest-try-group dict word box i grp))
    (-map-suggest-1 dict word box (1+ i))))

(defun -map-suggest (dict word box)
;; Try DICT's MAP character/string equivalences throughout WORD.
;; Mirrors Nuspell's `Suggester::map_suggest'.
  (let ((-budget (-max-attempts-for-long-algos dict word)))
    (-map-suggest-1 dict word box 0)))

(defun -adjacent-swap-suggest (dict word box)
;; Try swapping each pair of adjacent characters of WORD.  For 4- and
;; 5-character words, also try the two-simultaneous-swap combinations
;; Nuspell additionally tries.  Mirrors Nuspell's
;; `Suggester::adjacent_swap_suggest'.
  (let ((len (length word)))
    (dotimes (i (1- len))
      (-add-sug-if-correct dict (-swap-adjacent word i) box))
    (pcase len
      (4 (-add-sug-if-correct dict (-swap-two-pairs word 0 2) box))
      (5 (-add-sug-if-correct dict (-swap-two-pairs word 0 3) box)
         (-add-sug-if-correct dict (-swap-two-pairs word 1 3) box)))))

(defun -distant-swap-suggest (dict word box)
;; Try swapping each pair of non-adjacent characters of WORD.  Mirrors
;; Nuspell's `Suggester::distant_swap_suggest'.
  (let ((len (length word)))
    (when (> len 0)
      (let ((-budget (-max-attempts-for-long-algos dict word)))
        (dotimes (i len)
          (dotimes (k (max 0 (- len i 2)))
            (when (-budget-consume)
              (-add-sug-if-correct dict (-swap-chars word i (+ i 2 k)) box))))))))

(defun -keyboard-suggest (dict word box)
;; Try case-toggling and keyboard-adjacent-key replacements in WORD.
;; DICT's KEY (keyboard closeness) groups are "|"-separated runs of
;; characters that are physically close together; this tries, for each
;; character of WORD, replacing it with its upper case, and with its
;; neighbors (if any) in every KEY group it belongs to.  Mirrors
;; Nuspell's `Suggester::keyboard_suggest'.
  (let ((kb (-dict-keyboard-closeness dict)))
    (dotimes (j (length word))
      (let* ((c (aref word j)) (upp (upcase c)))
        (when (/= upp c)
          (-add-sug-if-correct dict (-replace-char-at word j (string upp)) box))
        (dotimes (i (length kb))
          (when (eq (aref kb i) c)
            (when (and (> i 0) (/= (aref kb (1- i)) ?|))
              (-add-sug-if-correct dict (-replace-char-at word j (substring kb (1- i) i)) box))
            (when (and (< (1+ i) (length kb)) (/= (aref kb (1+ i)) ?|))
              (-add-sug-if-correct
               dict (-replace-char-at word j (substring kb (1+ i) (+ i 2))) box))))))))

(defun -extra-char-suggest (dict word box)
;; Try removing each single character of WORD.  Mirrors Nuspell's
;; `Suggester::extra_char_suggest'.
  (dotimes (i (length word))
    (-add-sug-if-correct dict (concat (substring word 0 i) (substring word (1+ i))) box)))

(defun -forgotten-char-suggest (dict word box)
;; Try inserting each of DICT's TRY characters at each position of
;; WORD.  Mirrors Nuspell's `Suggester::forgotten_char_suggest'.
  (let ((-budget (-max-attempts-for-long-algos dict word))
        (try-chars (-dict-try-chars dict)))
    (dotimes (t-idx (length try-chars))
      (let ((cp (substring try-chars t-idx (1+ t-idx))))
        (dotimes (i (1+ (length word)))
          (when (-budget-consume)
            (-add-sug-if-correct
             dict (concat (substring word 0 i) cp (substring word i)) box)))))))

(defun -move-char-suggest (dict word box)
;; Try moving each character of WORD to each other, non-adjacent
;; position.  Mirrors Nuspell's `Suggester::move_char_suggest'.
  (let* ((len (length word))
         (-budget (-max-attempts-for-long-algos dict word)))
    (dotimes (i1 len)
      (dotimes (k (- len i1 2))
        (when (-budget-consume)
          (-add-sug-if-correct dict (-move-char-forward word i1 (+ i1 2 k)) box))))
    (dotimes (i1 len)
      (dotimes (k (max 0 (- i1 2)))
        (when (-budget-consume)
          (-add-sug-if-correct dict (-move-char-backward word i1 (- i1 2 k)) box))))))

(defun -bad-char-suggest (dict word box)
;; Try replacing each character of WORD with each of DICT's TRY
;; characters.  Mirrors Nuspell's `Suggester::bad_char_suggest'.
  (let ((-budget (-max-attempts-for-long-algos dict word))
        (try-chars (-dict-try-chars dict)))
    (dotimes (t-idx (length try-chars))
      (let ((tc (aref try-chars t-idx)))
        (dotimes (i (length word))
          (unless (eq (aref word i) tc)
            (when (-budget-consume)
              (-add-sug-if-correct dict (-replace-char-at word i (string tc)) box))))))))

(defun -doubled-two-chars-suggest (dict word box)
;; Try collapsing an "XYXYX" run in WORD down to "XYX".  Mirrors
;; Nuspell's `Suggester::doubled_two_chars_suggest'.
  (let ((len (length word)))
    (dotimes (i (max 0 (- len 4)))
      (when (and (eq (aref word i) (aref word (+ i 2)))
                 (eq (aref word (1+ i)) (aref word (+ i 3)))
                 (eq (aref word i) (aref word (+ i 4))))
        (-add-sug-if-correct
         dict (concat (substring word 0 (+ i 3)) (substring word (+ i 5))) box)))))

(defun -two-words-suggest (dict word box)
;; Try splitting WORD into two dictionary words, space- or
;; hyphen-joined.  Mirrors Nuspell's `Suggester::two_words_suggest'.
  (let ((len (length word)) (try-chars (-dict-try-chars dict)))
    (dotimes (i (max 0 (1- len)))
      (let ((w1 (substring word 0 (1+ i))) (w2 (substring word (1+ i))))
        (when (and (-check-simple-word dict w1 t) (-check-simple-word dict w2 t))
          (let ((candidate (concat w1 " " w2)))
            (unless (member candidate (-sugs-list box))
              (-sugs-push box candidate)))
          (when (and (>= i 1) (length> w2 1)
                     (or (cl-position ?a try-chars) (cl-position ?- try-chars)))
            (let ((candidate (concat w1 "-" w2)))
              (unless (member candidate (-sugs-list box))
                (-sugs-push box candidate)))))))))

(defun -suggest-low (dict word box)
;; Try every single-typo heuristic above for WORD, pushing hits onto
;; BOX.  Return non-nil if a "high quality" suggestion was found this
;; way, in which case the caller may skip the much more expensive
;; `-ngram-suggest'.  Mirrors Nuspell's `Suggester::suggest_low'.
  (let ((old-count (-sugs-box-count box)))
    (-uppercase-suggest dict word box)
    (-rep-suggest dict word box)
    (-map-suggest dict word box)
    (let ((hq (or (/= old-count (-sugs-box-count box))
                  (and (-dict-similarities dict) (-check-word dict word nil t) t))))
      (-adjacent-swap-suggest dict word box)
      (-distant-swap-suggest dict word box)
      (-keyboard-suggest dict word box)
      (-extra-char-suggest dict word box)
      (-forgotten-char-suggest dict word box)
      (-move-char-suggest dict word box)
      (-bad-char-suggest dict word box)
      (-doubled-two-chars-suggest dict word box)
      (-two-words-suggest dict word box)
      hq)))

;;;; N-gram-based suggestion search
;;
;; When none of the heuristics above finds a "high quality" match,
;; `-ngram-suggest' instead scores every word of the dictionary by how
;; similar it looks to the misspelled word (by shared substrings and
;; letters in matching positions, roughly), keeps the 100 best-scoring
;; roots, expands each of those through its possible affixes, scores
;; those expansions too, and returns the best-scoring ones.  This is
;; much more expensive than the heuristics above (it visits every
;; dictionary word) but catches corrections that are not just one
;; small edit away from the misspelling.  Mirrors Nuspell's
;; `Suggester::ngram_suggest' and the free functions above it.

;; Hard cap on the number of suggestions `-ngram-suggest' may add.
;; Mirrors Nuspell's `Aff_Data::MAX_SUGGESTIONS'.
(defvar -max-suggestions 16)

(defun -kgrams (n a)
;; Return the k-grams of A, for k=1..N, as a vector of vectors.
;; Element K-1 is the vector of A's K-grams, in order.  The n-gram
;; similarity functions below accept this as an optional precomputed
;; A-GRAMS argument: `-ngram-suggest' compares the same misspelled
;; word against every dictionary word, so extracting its k-grams once
;; per call, rather than once per comparison, avoids redoing that work
;; (and its allocations) tens of thousands of times.
  (let* ((len (length a))
         (n (min n len))
         (result (make-vector n nil)))
    (dotimes (k0 n)
      (let ((grams (make-vector (- len k0) nil)))
        (dotimes (i (length grams))
          (aset grams i (substring a i (+ i k0 1))))
        (aset result k0 grams)))
    result))

(defun -ngram-similarity-low-level (n a b &optional a-grams)
;; Return a raw k-gram-overlap similarity score of A against B,
;; k=1..N.  A-GRAMS, if non-nil, is `-kgrams' of A for some N' >= N.
;; Mirrors Nuspell's `ngram_similarity_low_level'.
  (let ((grams (or a-grams (-kgrams n a)))
        (n (min n (length a)))
        (score 0)
        (k0 0))
    (while (< k0 n)
      (let ((v (aref grams k0)) (k-score 0))
        (dotimes (i (length v))
          (when (string-search (aref v i) b)
            (setq k-score (1+ k-score))))
        (setq score (+ score k-score))
        (setq k0 (if (< k-score 2) n (1+ k0)))))
    score))

(defun -ngram-similarity-weighted-low-level (n a b &optional a-grams)
;; Like `-ngram-similarity-low-level', but penalizing non-matching
;; k-grams.  A-GRAMS is as in `-ngram-similarity-low-level'.  Mirrors
;; Nuspell's `ngram_similarity_weighted_low_level'.
  (let ((grams (or a-grams (-kgrams n a)))
        (score 0))
    (dotimes (k0 (min n (length a)))
      (let* ((v (aref grams k0)) (last (1- (length v))) (k-score 0))
        (dotimes (i (length v))
          (if (string-search (aref v i) b)
              (setq k-score (1+ k-score))
            (setq k-score (1- k-score))
            (when (or (= i 0) (= i last))
              (setq k-score (1- k-score)))))
        (setq score (+ score k-score))))
    score))

(defun -ngram-similarity-longer-worse (n a b &optional a-grams)
;; `-ngram-similarity-low-level', penalized when B is much longer than
;; A.  Mirrors Nuspell's `ngram_similarity_longer_worse'.
  (if (string-empty-p b)
      0
    (let ((score (-ngram-similarity-low-level n a b a-grams))
          (d (- (- (length b) (length a)) 2)))
      (if (> d 0) (- score d) score))))

(defun -ngram-similarity-any-mismatch (n a b &optional a-grams)
;; `-ngram-similarity-low-level', penalized when A and B differ much
;; in length.  Mirrors Nuspell's `ngram_similarity_any_mismatch'.
  (if (string-empty-p b)
      0
    (let ((score (-ngram-similarity-low-level n a b a-grams))
          (d (- (abs (- (length b) (length a))) 2)))
      (if (> d 0) (- score d) score))))

(defun -ngram-similarity-any-mismatch-weighted (n a b &optional a-grams)
;; Weighted version of `-ngram-similarity-any-mismatch'.  Mirrors
;; Nuspell's `ngram_similarity_any_mismatch_weighted'.
  (if (string-empty-p b)
      0
    (let ((score (-ngram-similarity-weighted-low-level n a b a-grams))
          (d (- (abs (- (length b) (length a))) 2)))
      (if (> d 0) (- score d) score))))

(defun -left-common-substring-length (a b)
;; Return the length of A and B's common prefix.  The first character
;; is matched case-insensitively; the rest are not.  Mirrors Nuspell's
;; `left_common_substring_length'.
  (if (or (string-empty-p a) (string-empty-p b)
          (and (/= (aref a 0) (aref b 0)) (/= (aref a 0) (downcase (aref b 0)))))
      0
    (let ((n (min (length a) (length b))) (i 1))
      (while (and (< i n) (eq (aref a i) (aref b i)))
        (setq i (1+ i)))
      i)))

(defun -longest-common-subsequence-length (a b)
;; Return the length of the longest common subsequence of A and B.
;; Mirrors Nuspell's `longest_common_subsequence_length'.
  (let ((state (make-vector (max (length b) 1) 0)))
    (dotimes (i (length a))
      (let ((row1-prev 0) (row2-prev 0))
        (dotimes (j (length b))
          (let ((row1-current (aref state j)))
            (if (eq (aref a i) (aref b j))
                (aset state j (1+ row1-prev))
              (aset state j (max row1-current row2-prev)))
            (setq row1-prev row1-current)
            (setq row2-prev (aref state j))))))
    (if (zerop (length b)) 0 (aref state (1- (length b))))))

(defun -count-eq-chars-at-same-pos (a b)
;; Return (NUM . IS-SWAP) for A and B; see Nuspell's
;; `count_eq_chars_at_same_pos'.  NUM is how many characters are equal
;; at the same position in both; when A and B are the same length and
;; exactly 2 positions differ, IS-SWAP says whether those two
;; differing characters are a transposition of each other.
  (let ((n (min (length a) (length b))) (count 0))
    (dotimes (i n)
      (when (eq (aref a i) (aref b i)) (setq count (1+ count))))
    (let (is-swap m1 m2)
      (when (and (= (length a) (length b)) (= (- n count) 2))
        (let ((i 0))
          (while (not m2)
            (unless (eq (aref a i) (aref b i))
              (if m1 (setq m2 i) (setq m1 i)))
            (incf i)))
        (setq is-swap (and (eq (aref a m1) (aref b m2)) (eq (aref a m2) (aref b m1)))))
      (cons count is-swap))))

(defun -expand-root-word-for-ngram (dict root flags wrong)
;; Return every word form derivable from ROOT/FLAGS, for
;; `-ngram-suggest'.  WRONG is the original (lower-cased) misspelled
;; word; affixes whose appending text is absent from it are skipped,
;; as a pruning optimization only.  Mirrors Nuspell's
;; `Suggester::expand_root_word_for_ngram'.
  (let ((base (unless (-flags-contain flags (-dict-need-affix-flag dict)) (list root)))
        suffixed cross-suffixed combo prefixed)
    (when flags
      (dolist (sfx (-all-affix-table-entries (-dict-suffix-table dict)))
        (when (and (-cross-valid-p flags (-suffix-flag sfx))
                   (not (-outer-suffix-not-valid-p :full-word sfx))
                   (not (-suffix-circumfix-p sfx))
                   (string-suffix-p (-suffix-stripping sfx) root)
                   (-suffix-check-condition sfx root)
                   (or (string-empty-p (-suffix-appending sfx))
                       (string-suffix-p (-suffix-appending sfx) wrong)))
          (push (-suffix-from-root sfx root) suffixed)
          (when (-suffix-cross-product sfx)
            (push (car suffixed) cross-suffixed))))
      (setq suffixed (nreverse suffixed))
      (dolist (root-sfx (nreverse cross-suffixed))
        (dolist (pfx (-all-affix-table-entries (-dict-prefix-table dict)))
          (when (and (-cross-valid-p flags (-prefix-flag pfx))
                     (not (-outer-prefix-not-valid-p :full-word pfx))
                     (not (-prefix-circumfix-p pfx))
                     (string-prefix-p (-prefix-stripping pfx) root-sfx)
                     (-prefix-check-condition pfx root-sfx)
                     (or (string-empty-p (-prefix-appending pfx))
                         (string-prefix-p (-prefix-appending pfx) wrong)))
            (push (-prefix-from-root pfx root-sfx) combo))))
      (dolist (pfx (-all-affix-table-entries (-dict-prefix-table dict)))
        (when (and (-cross-valid-p flags (-prefix-flag pfx))
                   (not (-outer-prefix-not-valid-p :full-word pfx))
                   (not (-prefix-circumfix-p pfx))
                   (string-prefix-p (-prefix-stripping pfx) root)
                   (-prefix-check-condition pfx root)
                   (or (string-empty-p (-prefix-appending pfx))
                       (string-prefix-p (-prefix-appending pfx) wrong)))
          (push (-prefix-from-root pfx root) prefixed))))
    (append base suffixed combo prefixed)))

(defun -heap-replace (heap entry)
;; Replace the root of HEAP, a `value<'-ordered min-heap vector, with
;; ENTRY.  The root, HEAP's worst entry, is discarded and ENTRY sifted
;; down to its place.  Like Python's `heapq.heapreplace'.
  (let ((n (length heap)) (i 0) (done nil))
    (while (not done)
      (let ((child (1+ (* 2 i))))
        (when (and (< (1+ child) n)
                   (value< (aref heap (1+ child)) (aref heap child)))
          (setq child (1+ child)))
        (if (and (< child n) (value< (aref heap child) entry))
            (progn (aset heap i (aref heap child))
                   (setq i child))
          (setq done t))))
    (aset heap i entry)))

(defun -ngram-candidates (dict)
;; Return the dictionary entries `-ngram-suggest' may suggest from.
;; This is a vector of (WORD LOWER FLAGS), one per homonym entry of
;; DICT's words table that is not forbidden, hidden, NOSUGGEST or
;; ONLYINCOMPOUND, in the table's order; LOWER is WORD's lowercased
;; form.  Which entries qualify depends only on DICT, so this is
;; computed on first use and cached, rather than re-filtered on every
;; suggestion; `-dict-add-word' and `-dict-remove-word' clear the
;; cache.
  (or (-dict-ngram-candidates dict)
      (let (acc)
        (maphash
         (lambda (word entry)
           (dolist (flags (cdr entry))
             (unless (or (-flags-contain flags (-dict-forbiddenword-flag dict))
                         (-hidden-homonym-p flags)
                         (-flags-contain flags (-dict-nosuggest-flag dict))
                         (-flags-contain flags (-dict-compound-onlyin-flag dict)))
               (push (list word (car entry) flags) acc))))
         (-dict-words dict))
        (setf (-dict-ngram-candidates dict) (vconcat (nreverse acc))))))

(defun -ngram-suggest (dict wrong-word box)
;; Add n-gram-similarity-based suggestions for WRONG-WORD to BOX.
;; WRONG-WORD is assumed already lower-cased.  Mirrors Nuspell's
;; `Suggester::ngram_suggest'.
  (let ((wrong-grams (-kgrams (length wrong-word) wrong-word))
        (candidates (-ngram-candidates dict))
        ;; Keep the best 100 roots in a `-heap-replace' heap of (SCORE
        ;; IDX . VALUE).  IDX increases, so it breaks ties on SCORE in
        ;; favor of the later offer, as a stable sort of a `push'ed list
        ;; would, and a newcomer beats the root iff its SCORE is at least
        ;; as high.  The filler entries have a nil VALUE.
        (top-roots (make-vector 100 (list most-negative-fixnum 0)))
        roots)
    (dotimes (i (length candidates))
      (when (zerop (logand i 1023))
        (-check-time-limit))
      (let* ((c (aref candidates i))
             (score (+ (-left-common-substring-length wrong-word (car c))
                       (-ngram-similarity-longer-worse 3 wrong-word (cadr c) wrong-grams))))
        (when (>= score (car (aref top-roots 0)))
          (-heap-replace top-roots (cons score (cons i c))))))
    ;; Each root is (SCORE WORD LOWER FLAGS), sharing its tail with the
    ;; `-ngram-candidates' entry.
    (setq roots (mapcan (lambda (e) (and (cddr e) (list (cons (car e) (cddr e)))))
                        ;; For Emacs ≤29
                        (compat-call sort top-roots :reverse t :in-place t)))

    (let ((threshold 0))
      (dolist (k '(1 2 3))
        (let ((mangled wrong-word) (i k))
          (while (< i (length mangled))
            ;; Can't `aset' an arbitrary character in place: Emacs
            ;; strings are not necessarily uniform-width internally,
            ;; so replacing e.g. a 2-byte-wide character with a
            ;; 1-byte-wide "*" can fail; rebuild instead, see (elisp)
            ;; Modifying Strings.
            (setq mangled (concat (substring mangled 0 i) "*" (substring mangled (1+ i))))
            (setq i (+ i 4)))
          (setq threshold (+ threshold
                             (-ngram-similarity-any-mismatch
                              (length wrong-word) wrong-word mangled wrong-grams)))))
      (setq threshold (/ threshold 3))

      (let ((top-guesses (make-vector 200 (list most-negative-fixnum 0)))
            (guess-idx 0)
            guesses)
        (dolist (r roots)
          (-check-time-limit)
          (pcase-let ((`(,_ ,word ,word-lower ,flags) r))
            (dolist (expanded (-expand-root-word-for-ngram dict word flags wrong-word))
              (let* ((lower (if (eq expanded word) word-lower (downcase expanded)))
                     (score (+ (-left-common-substring-length wrong-word expanded)
                               (-ngram-similarity-any-mismatch
                                (length wrong-word) wrong-word lower wrong-grams))))
                (when (>= score (max (car (aref top-guesses 0)) threshold))
                  (-heap-replace top-guesses (cons score (cons (incf guess-idx) expanded))))))))
        (setq guesses (mapcan (lambda (e) (and (cddr e) (list (cons (car e) (cddr e)))))
                              ;; For Emacs ≤29
                              (compat-call sort top-guesses :reverse t :in-place t)))

        (named-let recur ((tail guesses))
          (when tail
            (or (let ((g (car tail)))
                  (pcase-let ((`(,score . ,word) g))
                    (let* ((lower (downcase word))
                           (lcs (-longest-common-subsequence-length wrong-word lower)))
                      (if (= (length wrong-word) (length lower) lcs)
                          (progn (setcar g (+ score 2000)) t)
                        (let* ((ngram2 (+ (-ngram-similarity-any-mismatch-weighted 2 wrong-word lower wrong-grams)
                                          (-ngram-similarity-any-mismatch-weighted 2 lower wrong-word)))
                               (ngram4 (-ngram-similarity-any-mismatch 4 wrong-word lower wrong-grams))
                               (left-common (-left-common-substring-length wrong-word lower))
                               (eq-info (-count-eq-chars-at-same-pos wrong-word lower))
                               (new-score (- (* 2 lcs) (abs (- (length wrong-word) (length lower))))))
                          (setq new-score (+ new-score left-common ngram2 ngram4))
                          (when (/= (car eq-info) 0) (setq new-score (1+ new-score)))
                          (when (cdr eq-info) (setq new-score (+ new-score 10)))
                          (when (< (* 5 ngram2)
                                   (* (+ (length wrong-word) (length lower))
                                      (- 10 (-dict-max-diff-factor dict))))
                            (setq new-score (- new-score 1000)))
                          (setcar g new-score)
                          nil)))))
                (recur (cdr tail)))))
        (setq guesses (sort guesses (lambda (a b) (> (car a) (car b)))))

        (let* ((more-selective (and guesses (> (car (car guesses)) 1000)))
               (old-num-sugs (-sugs-box-count box))
               (max-sug (min -max-suggestions (+ old-num-sugs (-dict-max-ngram-suggestions dict)))))
          (named-let recur ((tail guesses))
            (when tail
              (or (let ((g (car tail)))
                    (pcase-let ((`(,score . ,word) g))
                      (cond
                       ((= (-sugs-box-count box) max-sug) t)
                       ((and more-selective (<= score 1000)) t)
                       ((and (< score -100)
                             (or (/= old-num-sugs (-sugs-box-count box)) (-dict-only-max-diff dict)))
                        t)
                       ((any (lambda (sug) (string-search sug word)) (-sugs-list box))
                        (< score -100))
                       (t (-sugs-push box word) nil))))
                  (recur (cdr tail))))))))))

;;;; Top-level suggestion entry point

(defun -fix-space-casing-in-sugs (box word)
;; Title-case the second word of BOX's space-containing suggestions.
;; Any suggestion of BOX that contains a space and whose part after
;; that space does not already end in the same case as WORD has that
;; part's first character upcased, and is moved to the front of BOX.
;; Used for CAMEL/PASCAL-cased input.  Loosely mirrors the casing-fix
;; loop in Nuspell's `Suggester::suggest_priv' (ties among several
;; qualifying suggestions are broken differently; see the LLM notes to
;; self near the top of this file).
  (let (fixed rest)
    (dolist (sug (-sugs-list box))
      (if-let* ((space (string-search " " sug))
                (i (1+ space))
                (len (- (length sug) i)))
          (if (and (<= len (length word))
                   (eq t (compare-strings sug i nil word (- (length word) len) nil)))
              (push sug rest)
            (push (concat (substring sug 0 i) (-to-title-char-at (substring sug i) 0))
                  fixed))
        (push sug rest)))
    (-sugs-box-set box (nreverse (nconc rest fixed)))))

(defun -fix-keepcase-sug (dict s)
;; Return S, possibly lower- or title-cased, or nil to drop it.  Used
;; for INIT_CAPITAL/ALL_CAPITAL input to a KEEPCASE/FORBIDDENWORD
;; dictionary.  Mirrors the inline `is_ok' lambda in Nuspell's
;; `Suggester::suggest_priv'.
  (cond
   ((string-search " " s) s)
   ((-spell dict s) s)
   ((-spell dict (downcase s)) (downcase s))
   (t (let ((title (-to-title s))) (and (-spell dict title) title)))))

(defun -suggest-dash-split (dict box word)
;; If WORD contains a hyphen, also try correcting it piece by piece.
;; For each "-"-separated piece of WORD that is not itself a correctly
;; spelled word, recursively suggest corrections for that piece alone
;; and splice each one into WORD in its place, pushing the result onto
;; BOX unless it is a forbidden word.  Skipped entirely if BOX already
;; has a suggestion containing a hyphen.  Mirrors the dash-splitting
;; block in Nuspell's `Suggester::suggest_priv'.
  (when (and (string-search "-" word)
             (not (any (lambda (s) (string-search "-" s)) (-sugs-list box))))
    (let ((i 0) (len (length word)))
      (while (<= i len)
        (let* ((dash (string-search "-" word i))
               (j (or dash len))
               (part (substring word i j)))
          (unless (-spell dict part)
            (dolist (sub (-suggest-priv dict part))
              (let* ((candidate (concat (substring word 0 i) sub (substring word j)))
                     (flg (-check-word dict candidate)))
                (when (or (not flg) (not (-flags-contain (cdr flg) (-dict-forbiddenword-flag dict))))
                  (-sugs-push box candidate)))))
          (setq i (if dash (1+ j) (1+ len))))))))

(defun -suggest-for-casing (dict word casing)
  ;; Return suggestions for WORD, dispatching on its CASING.  Called
  ;; from `-suggest-priv' once WORD has had any trailing abbreviation
  ;; dots trimmed and its early-exit special case ruled out; CASING is
  ;; `-classify-casing' of WORD.
  (let* ((box (-sugs-box-create)) (hq-sugs nil)
         (timed-out
          (catch '-time-limit
            (pcase casing
              ('small
               (setq hq-sugs (-suggest-low dict word box)))
              ('init-capital
               (setq hq-sugs (-suggest-low dict word box))
               (setq hq-sugs (or (-suggest-low dict (downcase word) box) hq-sugs)))
              ((or 'camel 'pascal)
               (setq hq-sugs (-suggest-low dict word box))
               (let ((dot (string-search "." word)))
                 (when dot
                   (let ((after (substring word (1+ dot))))
                     (when (eq (-classify-casing after) 'init-capital)
                       (-sugs-push-first box (concat (substring word 0 (1+ dot)) " " after))))))
               (when (eq casing 'pascal)
                 (let ((buffer (concat (downcase (substring word 0 1)) (substring word 1))))
                   (when (-spell dict buffer) (-sugs-push-first box buffer))
                   (setq hq-sugs (or (-suggest-low dict buffer box) hq-sugs))))
               (let ((lower (downcase word)))
                 (when (-spell dict lower) (-sugs-push-first box lower))
                 (setq hq-sugs (or (-suggest-low dict lower box) hq-sugs)))
               (when (eq casing 'pascal)
                 (let ((title (-to-title word)))
                   (when (-spell dict title) (-sugs-push-first box title))
                   (setq hq-sugs (or (-suggest-low dict title box) hq-sugs))))
               (-fix-space-casing-in-sugs box word))
              ('all-capital
               (let ((lower (downcase word)))
                 (when (and (/= 0 (-dict-keepcase-flag dict)) (-spell dict lower))
                   (-sugs-push-first box lower))
                 (setq hq-sugs (-suggest-low dict lower box)))
               (let ((title (-to-title word)))
                 (setq hq-sugs (or (-suggest-low dict title box) hq-sugs)))
               (-sugs-box-set box (mapcar #'upcase (-sugs-list box)))))

            (when (and (not hq-sugs) (/= 0 (-dict-max-ngram-suggestions dict)))
              (let* ((buffer (if (eq casing 'small) word (downcase word)))
                     (old-list (-sugs-list box))
                     (old-len (length old-list)))
                (-ngram-suggest dict buffer box)
                (when (eq casing 'all-capital)
                  (-sugs-box-set
                   box (append old-list (mapcar #'upcase (nthcdr old-len (-sugs-list box))))))))
            nil)))

    (if (not timed-out)
        (-suggest-dash-split dict box word)
      ;; Out of time: keep what was found, finishing the casing fix-up
      ;; the throw may have skipped, and lift the limit for the
      ;; clean-ups below.
      (when (eq casing 'all-capital)
        (-sugs-box-set box (mapcar #'upcase (-sugs-list box))))
      (setq -time-limit nil))

    (when (memq casing '(init-capital pascal))
      (-sugs-box-set box (mapcar (lambda (s) (-to-title-char-at s 0)) (-sugs-list box))))

    (when (and (memq casing '(init-capital all-capital))
               (or (/= 0 (-dict-keepcase-flag dict)) (/= 0 (-dict-forbiddenword-flag dict))))
      (-sugs-box-set box (mapcan (lambda (s)
                                   (and-let* ((x (-fix-keepcase-sug dict s))) (list x)))
                                 (-sugs-list box))))

    (-sugs-box-set box (delete-dups (-sugs-list box)))

    ;; OCONV can map a suggestion back to WORD itself (e.g. "románórum"
    ;; to "romanórum"); drop it then, as Hunspell does (its gh#1002).
    (delete word (mapcar (lambda (s) (-substr-replace (-dict-output-oconv dict) s))
                         (-sugs-list box)))))

(defun -suggest-priv (dict word)
;; Return suggestions for WORD, as a plain list.  Applies DICT's input
;; ICONV to WORD and trims its trailing abbreviation dots, if any,
;; then handles Nuspell's `compound_force_uppercase' early-exit
;; special case before dispatching to `-suggest-for-casing'.  Mirrors
;; Nuspell's `Suggester::suggest_priv'.
  (unless (string-empty-p word)
    (let* ((word (-substr-replace (-dict-input-iconv dict) word))
           (abbreviation (eq (aref word (1- (length word))) ?.))
           (word (if abbreviation (substring word 0 (-string-last-non-dot word)) word)))
      (unless (string-empty-p word)
        (let ((casing (-classify-casing word)))
          (if (and (eq casing 'small)
                   (/= 0 (-dict-compound-force-uppercase dict))
                   (-check-compound dict word t))
              (list (-to-title word))
            (-suggest-for-casing dict word casing)))))))

;;;; Dictionary (.dic) parsing

(defun -parse-dic-line ()
;; Parse the .dic line starting at point into (WORD
;; . FLAGS-STRING-OR-NIL), or nil if it has no word; point is left
;; somewhere on that line.  As in Nuspell, the entry ends at a tab, or
;; at a space followed by a morphological field or by nothing else;
;; any other space is part of a multi-word entry, like "ad hoc".
;; Within the entry, the first "/" other than a leading one starts the
;; flags, and a backslash-escaped slash is kept as a literal "/".
  (let ((case-fold-search nil)
        (start (point)))
    (if (re-search-forward (rx (or "\t" (: " " (* (any " \t")) (= 2 (any "a-z")) ":")))
                           (pos-eol) t)
        (goto-char (match-beginning 0))
      (end-of-line)
      (skip-chars-backward " \r" start))
    (let ((end (point)) (slash nil))
      (unless (= start end)
        ;; Skip the first character, since a leading "/" is literal.
        (goto-char (1+ start))
        (while (and (setq slash (search-forward "/" end t))
                    (eq (char-before (1- slash)) ?\\)))
        (cons (string-replace "\\/" "/" (buffer-substring-no-properties
                                         start (if slash (1- slash) end)))
              (and slash
                   (buffer-substring-no-properties
                    slash (progn (skip-chars-forward "^ " end) (point)))))))))

(defun -add-homonym (table word flags)
;; Append a homonym entry with FLAGS to WORD's entry in words TABLE.
;; A new entry also records WORD's lowercased form, computed once here
;; rather than on every `-ngram-suggest' call, which needs it for
;; every dictionary word; it is WORD itself, with no allocation, when
;; WORD is already all lowercase.
  (let ((entry (gethash word table)))
    ;; Appended, not pushed: homonyms must stay in insertion order, since
    ;; a later one can mask an earlier one for a flag (e.g. FORBIDDENWORD)
    ;; that only some of them carry -- see `-check-simple-word'.
    (if entry
        (setcdr entry (append (cdr entry) (list flags)))
      (puthash word
               (list (if (let ((case-fold-search nil))
                           (string-match-p "[[:upper:]]" word))
                         (downcase word)
                       word)
                     flags)
               table))))

(defun -dict-add-word (dict word flags)
;; Add dictionary entry WORD/FLAGS to DICT.  If WORD is ALL-CAPITAL
;; (with non-empty FLAGS), PASCAL or CAMEL cased, also add a
;; title-cased duplicate flagged as a hidden homonym, so that checking
;; the title-cased form of a word whose canonical form has different
;; casing (e.g. an acronym) falls through to other checks rather than
;; accepting it outright; see `-hidden-homonym-p'.
  (let* ((table (-dict-words dict))
         (casing (-classify-casing word)))
    (setf (-dict-ngram-candidates dict) nil)
    (-add-homonym table word flags)
    (when (or (and (eq casing 'all-capital) flags)
              (memq casing '(pascal camel)))
      (unless (-flags-contain flags (-dict-forbiddenword-flag dict))
        (let ((title (-to-title word)))
          ;; -1 marks this entry as a hidden homonym; see `-hidden-homonym-p'.
          (-add-homonym table title (cons -1 flags)))))))

(defun -dict-remove-word (dict word)
;; Remove one nil-flags entry of WORD from DICT's words table.  This
;; is the counterpart of `-dict-add-word' called with nil FLAGS, which
;; is how `elspell-add-word' always calls it; an entry with non-nil
;; flags, e.g. one loaded from the dictionary's own .dic file, is left
;; alone even if WORD also has a nil-flags entry alongside it.  Any
;; hidden-homonym duplicate `-dict-add-word' may have added for WORD
;; elsewhere is left in place; it has no effect on checking, only on
;; whether that title-cased spelling is skipped when found directly
;; (see `-hidden-homonym-p'), so an unused leftover one is harmless.
;;
;; Return non-nil if such an entry was found and removed.
  (let* ((table (-dict-words dict))
         (entry (gethash word table)))
    (when (memq nil (cdr entry))
      (setf (-dict-ngram-candidates dict) nil)
      (let ((new-list (cl-remove nil (cdr entry) :count 1)))
        (if new-list
            (setcdr entry new-list)
          (remhash word table)))
      t)))

(defun -set-coding-system (name)
;; Return the coding system for a SET directive's encoding NAME, or
;; `undecided' if Emacs has none by that name.
  (let ((name (downcase name)))
    (pcase name
      ("utf-8" 'utf-8)
      ((rx bos "iso8859-" (let n (+ digit)) eos)
       (-set-coding-system (concat "iso-8859-" n)))
      ("microsoft-cp1251" 'windows-1251)
      ("tis620-2533" 'tis-620)
      (_ (if (coding-system-p (intern name)) (intern name) 'undecided)))))

(defun -parse-dic (dict file)
;; Parse .dic FILE into DICT.
  (with-temp-buffer
    (let ((coding-system-for-read (-dict-coding dict)))
      (insert-file-contents file))
    (goto-char (point-min))
    (when (eq (char-after) #xFEFF)
      (delete-char 1))
    ;; The first line is an approximate word count; use it to presize the
    ;; words table so it isn't repeatedly grown and rehashed as entries
    ;; are added below.
    (let ((count (string-to-number (buffer-substring-no-properties
                                    (pos-bol) (pos-eol)))))
      (setf (-dict-words dict) (make-hash-table :test 'equal :size (max count 65))))
    (forward-line 1)
    (while (not (eobp))
      (when-let* ((parsed (-parse-dic-line)))
        (let* ((word (-erase-chars (car parsed) (-dict-ignored-chars dict)))
               (flags-str (cdr parsed))
               (flags (and flags-str (-decode-flags-possible-alias dict flags-str))))
          (unless (string-empty-p word)
            (-dict-add-word dict word flags))))
      (forward-line 1))))

;;;; Affix (.aff) parsing

(defun -current-line ()
;; Return the text of the current line, without text properties and
;; without a trailing CR (as in files with DOS line endings that were
;; not decoded as such).
  (let ((bol (pos-bol)) (eol (pos-eol)))
    (when (and (> eol bol) (eq (char-before eol) ?\r))
      (setq eol (1- eol)))
    (buffer-substring-no-properties bol eol)))

(defun -array-command-step (cmd-counts command count-token)
;; Advance array-COMMAND state in CMD-COUNTS.  Return `header' when
;; this line declared the array's element count, `data' when this is a
;; data line to be handled by the caller, or nil for a data line
;; beyond the declared count (ignored).
  (let ((state (gethash command cmd-counts 'unset)))
    (cond
     ((eq state 'unset)
      (puthash command (string-to-number count-token) cmd-counts)
      'header)
     ((> state 0)
      (puthash command (1- state) cmd-counts)
      'data)
     (t nil))))

(defun -parse-compound-rule-charwise (str)
;; Parse single-char/UTF-8 COMPOUNDRULE text STR into (FLAG . QUANT)
;; pairs.
  (let ((i 0) (n (length str)) (result nil))
    (while (< i n)
      (let ((flag (aref str i)))
        (setq i (1+ i))
        (let (quant)
          (when (and (< i n) (memq (aref str i) '(?? ?*)))
            (setq quant (aref str i))
            (setq i (1+ i)))
          (push (cons flag quant) result))))
    (nreverse result)))

(defun -parse-compound-rule-grouped (str group-len)
;; Parse long/num COMPOUNDRULE text STR, flags given as "(...)"
;; groups.
  (let ((i 0) (n (length str)) (result nil))
    (while (< i n)
      (unless (eq (aref str i) ?\()
        (error "elspell: invalid COMPOUNDRULE %S" str))
      (setq i (1+ i))
      (let ((start i))
        (while (and (< i n) (not (eq (aref str i) ?\))))
          (setq i (1+ i)))
        (let ((flag (if (eq group-len 2)
                        (logior (ash (aref str start) 8) (aref str (1+ start)))
                      (string-to-number (substring str start i)))))
          (setq i (1+ i))                ; skip ")"
          (let (quant)
            (when (and (< i n) (memq (aref str i) '(?? ?*)))
              (setq quant (aref str i))
              (setq i (1+ i)))
            (push (cons flag quant) result)))))
    (nreverse result)))

(defun -parse-compound-rule-line (dict str)
  (pcase (-dict-flag-type dict)
    ('single-char (-parse-compound-rule-charwise (-flag-bytes dict str)))
    ('utf-8 (-parse-compound-rule-charwise str))
    ('long (-parse-compound-rule-grouped (-flag-bytes dict str) 2))
    ('num (-parse-compound-rule-grouped str nil))))

(defun -parse-compound-pattern (dict args)
;; Parse the fields ARGS of a CHECKCOMPOUNDPATTERN line into a
;; `-compound-pattern', or nil if there are fewer than two.  The first
;; two fields are each TEXT or TEXT/FLAG; a first TEXT of "0" stands
;; for empty text matching only a first word that no affix modified.
;; As in Hunspell and Nuspell, the third field, if any, is the
;; replacement, even when it is the start of a trailing comment.
  (when (cdr args)
    (pcase-let* ((`((,end . ,first-flag) (,begin . ,second-flag))
                  (mapcar (lambda (field)
                            (let ((i (string-search "/" field)))
                              (if i
                                  (cons (substring field 0 i)
                                        (or (car (-decode-flags dict (substring field (1+ i)))) 0))
                                (cons field 0))))
                          (take 2 args)))
                 (unmodified (string= end "0"))
                 (end (if unmodified "" end)))
      (-compound-pattern-create :text (concat end begin) :idx (length end)
                                :replacement (nth 2 args)
                                :first-flag first-flag :second-flag second-flag
                                :first-unmodified unmodified))))

(defun -partition-break-patterns (patterns)
;; Partition raw BREAK pattern strings into (START END MIDDLE) lists.
;; A pattern beginning with "^" is a start-of-word pattern (with the
;; "^" stripped); a remaining pattern ending with "$" is an
;; end-of-word pattern (with the "$" stripped); everything else is a
;; middle-of-word pattern.  Empty patterns, and the bare patterns "^"
;; and "$", are dropped, matching Nuspell's
;; `Break_Table::order_entries'.
  (let (start end middle)
    (dolist (p patterns)
      (cond
       ((or (string-empty-p p) (string= p "^") (string= p "$")))
       ;; A pattern with both anchors, like "^-$", counts as a start
       ;; pattern, keeping the "$" as a literal character; as in
       ;; Nuspell, which checks for "^" first.  Such a pattern is
       ;; meaningless anyway (Hunspell doesn't make sense of it either).
       ((eq (aref p 0) ?^) (push (substring p 1) start))
       ((eq (aref p (1- (length p))) ?$) (push (substring p 0 -1) end))
       (t (push p middle))))
    (list (nreverse start) (nreverse end) (nreverse middle))))

(defun -compute-compound-rules-all-flags (rules)
  (let (flags)
    (dolist (pattern rules)
      (dolist (node pattern)
        (cl-pushnew (car node) flags)))
    flags))

;;;; Suggestion-related affix tables (REP, MAP, ICONV/OCONV)

(defun -parse-similarity-group (str)
;; Parse one MAP line STR into a `-similarity-group'.  Text outside
;; parentheses becomes equivalent single characters; a single
;; character inside parentheses is added the same way, but a
;; multi-character parenthesized run is instead added as a whole
;; equivalent string.  Mirrors Nuspell's `Similarity_Group::parse'.
  (let ((i 0) (n (length str)) chars strings (done nil))
    (while (and (< i n) (not done))
      (let ((open (cl-position ?\( str :start i)))
        (if (not open)
            (progn (setq chars (concat chars (substring str i))) (setq done t))
          (setq chars (concat chars (substring str i open)))
          (let ((close (cl-position ?\) str :start (1+ open))))
            (if (not close)
                (setq done t)
              (let ((len (- close open 1)))
                (cond
                 ((= len 1) (setq chars (concat chars (substring str (1+ open) close))))
                 ((> len 1) (push (substring str (1+ open) close) strings)))
                (setq i (1+ close))))))))
    (-similarity-group-create :chars chars :strings (nreverse strings))))

(defun -build-replacement-table (pairs)
;; Partition REP PAIRS, each (FROM . TO), by anchor into 4 lists.
;; Return (WHOLE-WORD START-WORD END-WORD ANY-PLACE), each a list of
;; (FROM . TO) with any "^"/"$" anchor stripped from FROM.  Entries
;; whose FROM is empty or a bare "^"/"$" are dropped.  Mirrors
;; Nuspell's `Replacement_Table::order_entries'.
  (let (whole start end any)
    (dolist (p pairs)
      (let ((from (car p)) (to (cdr p)))
        (unless (or (string-empty-p from) (member from '("^" "$")))
          (let* ((starts (eq (aref from 0) ?^))
                 (ends (eq (aref from (1- (length from))) ?$))
                 (from (cond ((and starts ends) (substring from 1 -1))
                             (starts (substring from 1))
                             (ends (substring from 0 -1))
                             (t from))))
            (cond
             ((and starts ends) (push (cons from to) whole))
             (starts (push (cons from to) start))
             (ends (push (cons from to) end))
             (t (push (cons from to) any)))))))
    (list (nreverse whole) (nreverse start) (nreverse end) (nreverse any))))

(defun -build-substr-replacer (pairs)
;; Build an ICONV/OCONV substitution table from (FROM . TO) PAIRS.
;; Return nil if there are no pairs, otherwise a hash table mapping
;; each character to the pairs whose FROM starts with it, longest FROM
;; first; pairs with equal FROM keep their order in PAIRS, so the
;; first one wins.  Entries with an empty FROM are dropped; see
;; `-substr-replace'.
  (let ((table (make-hash-table)))
    (dolist (p (reverse pairs))
      (unless (string-empty-p (car p))
        (push p (gethash (aref (car p) 0) table))))
    (when (> (hash-table-count table) 0)
      (maphash (lambda (c ps)
                 (puthash c (compat-call ;; For Emacs ≤29
                             sort ps :key (lambda (p) (length (car p))) :reverse t)
                          table))
               table)
      table)))

(defun -substr-replacer-find (table s i)
;; Return the (FROM . TO) entry of TABLE whose FROM is the longest
;; match at position I of S, or nil if none matches there.
  (let ((len (length s)))
    (named-let recur ((tail (gethash (aref s i) table)))
      (when tail
        (or (let ((p (car tail)))
              (let ((end (+ i (length (car p)))))
                (and (<= end len)
                     (eq t (compare-strings (car p) nil nil s i end))
                     p)))
            (recur (cdr tail)))))))

(defun -substr-replace (table s)
;; Apply TABLE's longest-match-at-each-position substitutions
;; throughout S.  Return S itself if nothing matches.  Mirrors
;; Nuspell's `Substr_Replacer', used for ICONV/OCONV.
  (if (null table)
      s
    (let ((i 0) (start 0) (len (length s)) parts)
      (while (< i len)
        (let ((m (-substr-replacer-find table s i)))
          (if (not m)
              (incf i)
            (when (< start i) (push (substring s start i) parts))
            (push (cdr m) parts)
            (setq i (+ i (length (car m)))
                  start i))))
      (if (null parts)
          s
        (when (< start len) (push (substring s start) parts))
        (apply #'concat (nreverse parts))))))

(defun -parse-aff-affix-line (dict command args cmd-affix)
;; Process one PFX/SFX line.  Return a new affix struct, or nil.
  (let* ((flag (-decode-first-flag dict (car args)))
         (key (cons command flag))
         (state (gethash key cmd-affix)))
    (if (null state)
        (progn
          (puthash key (cons (string= (nth 1 args) "Y") (string-to-number (nth 2 args)))
                   cmd-affix)
          nil)
      (let ((cross (car state)) (remaining (cdr state)))
        (when (> remaining 0)
          (puthash key (cons cross (1- remaining)) cmd-affix)
          (let* ((stripping0 (nth 1 args))
                 (stripping (if (string= stripping0 "0") "" stripping0))
                 (appending-raw (nth 2 args))
                 (slash (cl-position ?/ appending-raw))
                 (appending0 (if slash (substring appending-raw 0 slash) appending-raw))
                 (appending (if (string= appending0 "0") "" appending0))
                 (cont-flags (and slash (-decode-flags-possible-alias
                                         dict (substring appending-raw (1+ slash)))))
                 (condition (-compile-condition (or (nth 3 args) "."))))
            (if (string= command "PFX")
                (-prefix-create :flag flag :cross-product cross
                                :stripping stripping :appending appending
                                :cont-flags cont-flags :condition condition)
              (-suffix-create :flag flag :cross-product cross
                              :stripping stripping :appending appending
                              :cont-flags cont-flags :condition condition))))))))

(defun -make-word-regexp (wordchars)
;; Return the regexp described in `elspell-word-regexp'.  WORDCHARS
;; is the value of the dictionary's WORDCHARS directive.
  (let (edge inner)
    (dotimes (i (length wordchars))
      (let ((c (aref wordchars i)))
        (cond ((memq c '(?' ?\’)) (setq inner (append '(?' ?\’) inner)))
              ((eq (get-char-code-property c 'general-category) 'Pd) (push c inner))
              (t (push c edge)))))
    (rx-to-string `(seq (any alpha ,@edge)
                        (* (any alpha ,@edge ,@inner))
                        (any alpha ,@edge))
                  t)))

(defun -parse-aff (dict file)
;; Parse .aff FILE into DICT.
  (with-temp-buffer
    ;; The SET directive names the encoding of both the .aff and .dic
    ;; files, so find it in the raw bytes, then decode the buffer.
    (let ((coding-system-for-read 'raw-text))
      (insert-file-contents file))
    (goto-char (point-min))
    (decode-coding-region (point-min) (point-max)
                          (if (re-search-forward "^SET[ \t]+\\([^ \t\r\n]+\\)" nil t)
                              (-set-coding-system (match-string 1))
                            'undecided))
    (setf (-dict-coding dict) last-coding-system-used)
    (goto-char (point-min))
    (when (eq (char-after) #xFEFF)
      (delete-char 1))
    (let ((prefixes nil) (suffixes nil)
          (break-patterns nil) (break-seen nil)
          (compound-rules nil) (compound-patterns nil) (flag-aliases nil)
          (rep-pairs nil) (similarities nil)
          (iconv-pairs nil) (oconv-pairs nil)
          (wordchars "")
          (cmd-affix (make-hash-table :test 'equal))
          (cmd-counts (make-hash-table :test 'equal)))
      (while (not (eobp))
        (let* ((line (-current-line))
               (trimmed (string-trim-left line)))
          (unless (or (string= trimmed "") (eq (aref trimmed 0) ?#))
            (let* ((tokens (split-string line))
                   (command (upcase (car tokens)))
                   (args (cdr tokens)))
              (pcase command
                ((or "PFX" "SFX")
                 (when-let* ((affix (-parse-aff-affix-line dict command args cmd-affix)))
                   (if (string= command "PFX")
                       (push affix prefixes)
                     (push affix suffixes))))
                ("FLAG"
                 (setf (-dict-flag-type dict)
                       (pcase (upcase (car args))
                         ("LONG" 'long)
                         ("NUM" 'num)
                         ("UTF-8" 'utf-8)
                         (_ (error "elspell: invalid FLAG type %S" (car args))))))
                ("IGNORE" (setf (-dict-ignored-chars dict) (car args)))
                ("TRY" (setf (-dict-try-chars dict) (car args)))
                ("WORDCHARS" (setq wordchars (car args)))
                ("KEY" (setf (-dict-keyboard-closeness dict) (car args)))
                ("COMPLEXPREFIXES" (setf (-dict-complex-prefixes dict) t))
                ("FULLSTRIP" (setf (-dict-fullstrip dict) t))
                ("CHECKSHARPS" (setf (-dict-checksharps dict) t))
                ("FORBIDWARN" (setf (-dict-forbid-warn dict) t))
                ("WARN" (setf (-dict-warn-flag dict) (-decode-first-flag dict (car args))))
                ("CIRCUMFIX"
                 (setf (-dict-circumfix-flag dict) (-decode-first-flag dict (car args))))
                ("FORBIDDENWORD"
                 (setf (-dict-forbiddenword-flag dict) (-decode-first-flag dict (car args))))
                ("KEEPCASE" (setf (-dict-keepcase-flag dict) (-decode-first-flag dict (car args))))
                ("NEEDAFFIX"
                 (setf (-dict-need-affix-flag dict) (-decode-first-flag dict (car args))))
                ("NOSUGGEST"
                 (setf (-dict-nosuggest-flag dict) (-decode-first-flag dict (car args))))
                ("MAXNGRAMSUGS"
                 (setf (-dict-max-ngram-suggestions dict) (string-to-number (car args))))
                ("MAXDIFF"
                 (let ((v (string-to-number (car args))))
                   (setf (-dict-max-diff-factor dict) (if (> v 10) 5 v))))
                ("ONLYMAXDIFF" (setf (-dict-only-max-diff dict) t))
                ("COMPOUNDFLAG"
                 (setf (-dict-compound-flag dict) (-decode-first-flag dict (car args))))
                ("COMPOUNDBEGIN"
                 (setf (-dict-compound-begin-flag dict) (-decode-first-flag dict (car args))))
                ("COMPOUNDEND"
                 (setf (-dict-compound-last-flag dict) (-decode-first-flag dict (car args))))
                ("COMPOUNDMIDDLE"
                 (setf (-dict-compound-middle-flag dict) (-decode-first-flag dict (car args))))
                ("ONLYINCOMPOUND"
                 (setf (-dict-compound-onlyin-flag dict) (-decode-first-flag dict (car args))))
                ("COMPOUNDPERMITFLAG"
                 (setf (-dict-compound-permit-flag dict) (-decode-first-flag dict (car args))))
                ("COMPOUNDFORBIDFLAG"
                 (setf (-dict-compound-forbid-flag dict) (-decode-first-flag dict (car args))))
                ("COMPOUNDROOT"
                 (setf (-dict-compound-root-flag dict) (-decode-first-flag dict (car args))))
                ("FORCEUCASE"
                 (setf (-dict-compound-force-uppercase dict) (-decode-first-flag dict (car args))))
                ("COMPOUNDMIN"
                 (let ((v (string-to-number (car args))))
                   (setf (-dict-compound-min-length dict) (if (= v 0) 1 v))))
                ("COMPOUNDWORDMAX"
                 (setf (-dict-compound-max-word-count dict) (string-to-number (car args))))
                ("CHECKCOMPOUNDCASE" (setf (-dict-compound-check-case dict) t))
                ("CHECKCOMPOUNDDUP" (setf (-dict-compound-check-duplicate dict) t))
                ("CHECKCOMPOUNDTRIPLE" (setf (-dict-compound-check-triple dict) t))
                ("CHECKCOMPOUNDREP" (setf (-dict-compound-check-rep dict) t))
                ("AF"
                 (pcase (-array-command-step cmd-counts command (car args))
                   ('data
                    (push (-decode-flags dict (car args)) flag-aliases)
                    ;; Kept live (not just set once at the end) so that
                    ;; PFX/SFX continuation flags and COMPOUNDRULE lines
                    ;; later in this same file can resolve aliases too.
                    (setf (-dict-flag-aliases dict) (vconcat (reverse flag-aliases))))))
                ("BREAK"
                 (setq break-seen t)
                 (when (eq (-array-command-step cmd-counts command (car args)) 'data)
                   (push (car args) break-patterns)))
                ("COMPOUNDRULE"
                 (pcase (-array-command-step cmd-counts command (car args))
                   ('data (push (-parse-compound-rule-line dict (car args))
                                compound-rules))))
                ("CHECKCOMPOUNDPATTERN"
                 (pcase (-array-command-step cmd-counts command (car args))
                   ('data (when-let* ((p (-parse-compound-pattern dict args)))
                            (push p compound-patterns)))))
                ("REP"
                 (pcase (-array-command-step cmd-counts command (car args))
                   ('data (push (cons (nth 0 args) (string-replace "_" " " (nth 1 args)))
                                rep-pairs))))
                ("MAP"
                 (pcase (-array-command-step cmd-counts command (car args))
                   ('data (push (-parse-similarity-group (car args)) similarities))))
                ("ICONV"
                 (pcase (-array-command-step cmd-counts command (car args))
                   ('data (push (cons (nth 0 args) (nth 1 args)) iconv-pairs))))
                ("OCONV"
                 (pcase (-array-command-step cmd-counts command (car args))
                   ('data (push (cons (nth 0 args) (nth 1 args)) oconv-pairs))))
                ((or "PHONE" "AM")
                 (-array-command-step cmd-counts command (car args)))))))
        (forward-line 1))
      ;; As in Nuspell, ignored characters are erased from each affix's
      ;; appended text (but not from its stripped text) once parsing is
      ;; done, since IGNORE may appear anywhere relative to PFX/SFX.
      ;; Likewise for the validity bits, which depend on directives
      ;; such as NEEDAFFIX and CIRCUMFIX.
      (dolist (p prefixes)
        (setf (-prefix-appending p) (-erase-chars (-prefix-appending p) (-dict-ignored-chars dict)))
        (setf (-prefix-bits p) (-affix-bits dict (-prefix-cont-flags p) :compound-end)))
      (dolist (s suffixes)
        (setf (-suffix-appending s) (-erase-chars (-suffix-appending s) (-dict-ignored-chars dict)))
        (setf (-suffix-bits s) (-affix-bits dict (-suffix-cont-flags s) :compound-begin)))
      (setf (-dict-prefix-table dict)
            (-build-affix-table prefixes #'-prefix-appending))
      (setf (-dict-suffix-table dict)
            (-build-affix-table suffixes (lambda (s) (reverse (-suffix-appending s)))))
      (setf (-dict-prefix-cont-flags dict)
            (let ((table (make-hash-table)))
              (dolist (p prefixes)
                (dolist (flag (-prefix-cont-flags p)) (puthash flag t table)))
              (unless (zerop (hash-table-count table)) table)))
      (setf (-dict-suffix-cont-flags dict)
            (let ((table (make-hash-table)))
              (dolist (s suffixes)
                (dolist (flag (-suffix-cont-flags s)) (puthash flag t table)))
              (unless (zerop (hash-table-count table)) table)))
      (unless break-seen (setq break-patterns '("^-" "-$" "-")))
      (let ((parts (-partition-break-patterns break-patterns)))
        (setf (-dict-break-start dict) (nth 0 parts))
        (setf (-dict-break-end dict) (nth 1 parts))
        (setf (-dict-break-middle dict) (nth 2 parts)))
      (setf (-dict-compound-rules dict) (nreverse compound-rules))
      (setf (-dict-compound-patterns dict) (nreverse compound-patterns))
      (setf (-dict-compound-rules-all-flags dict)
            (-compute-compound-rules-all-flags (-dict-compound-rules dict)))
      (setf (-dict-flag-aliases dict) (vconcat (nreverse flag-aliases)))
      (pcase-let ((`(,whole ,start ,end ,any)
                   (-build-replacement-table (nreverse rep-pairs))))
        (setf (-dict-rep-whole dict) whole)
        (setf (-dict-rep-start dict) start)
        (setf (-dict-rep-end dict) end)
        (setf (-dict-rep-any dict) any))
      (setf (-dict-similarities dict) (nreverse similarities))
      (setf (-dict-input-iconv dict) (-build-substr-replacer (nreverse iconv-pairs)))
      (setf (-dict-output-oconv dict) (-build-substr-replacer (nreverse oconv-pairs)))
      (setf (-dict-word-regexp dict) (-make-word-regexp wordchars)))))

;;;; Personal dictionary

(defun -personal-dictionary-file (name)
;; Return the personal dictionary file to use for dictionary NAME.
;; See `elspell-personal-dictionary-format': this is the first file
;; named by it that exists, or, if none exists, the file named by its
;; first element.
  (let ((formats (ensure-list elspell-personal-dictionary-format)))
    (or (named-let recur ((tail formats))
          (when tail
            (or (let ((fmt (car tail)))
                  (let ((file (format fmt name)))
                    (and (file-exists-p file) file)))
                (recur (cdr tail)))))
        (format (car formats) name))))

(defun -init-personal-dictionary (dict name)
;; Set up DICT's personal dictionary, and load any words already in
;; it.  NAME is the dictionary name passed to `elspell-create'.
  (let ((file (-personal-dictionary-file name)))
    (setf (-dict-pdict dict) file)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((word (-current-line)))
            (unless (string-empty-p word)
              (-dict-add-word dict word nil)))
          (forward-line 1))))))

(defun -save-personal-word (file word)
;; Append WORD as a new line to personal dictionary FILE.  FILE, and
;; its parent directory, are created if they do not exist yet.
  (make-directory (file-name-directory file) t)
  (write-region (concat word "\n") nil file t))

(defun -remove-personal-word (file word)
;; Remove each line equal to WORD from personal dictionary FILE.
;; Return non-nil if FILE exists and had at least one such line.
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (removed)
        (while (not (eobp))
          (if (string= (-current-line) word)
              (progn (delete-region (pos-bol) (min (point-max) (1+ (pos-eol))))
                     (setq removed t))
            (forward-line 1)))
        (when removed
          (write-region (point-min) (point-max) file))
        removed))))

;; Local Variables:
;; read-symbol-shorthands: (("-" . "elspell--"))
;; End:

(provide 'elspell)
;;; elspell.el ends here
