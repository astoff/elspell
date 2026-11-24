;;; elspell-tests.el --- Tests for elspell.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Augusto Stoffel

;; Author: Augusto Stoffel <arstoffel@gmail.com>
;; Keywords: text

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

;; Runs Hunspell's own test dictionaries, found in hunspell/tests/,
;; against `elspell-check'.  For every NAME.aff + NAME.dic pair in
;; that directory that comes with a NAME.good and/or NAME.wrong word
;; list, this defines an ERT test `elspell-test-hunspell-NAME' that
;; loads the dictionary and checks that every word in NAME.good is
;; accepted and every word in NAME.wrong is rejected.  This mirrors
;; the ".good"/".wrong" portion of Hunspell's own tests/test.sh.
;;
;; For every NAME.wrong that also comes with a NAME.sug, this also
;; defines `elspell-test-hunspell-suggest-NAME', which checks that
;; `elspell-suggest' includes at least one of NAME.sug's expected
;; alternatives for each corresponding word of NAME.wrong -- see
;; `elspell-tests--check-suggest' for how the two files are paired
;; up.  This is a
;; looser check than Hunspell's own test.sh, which requires the exact
;; suggestion list hunspell itself produces: `elspell-suggest' is a
;; Nuspell-derived port and is not expected to reproduce Hunspell's own
;; `SuggestMgr' suggestion lists verbatim, only to come up with
;; reasonable corrections, among which the expected one should appear.
;;
;; Hunspell's other kinds of expectations -- NAME.root/.morph
;; (morphological analysis) and NAME.trace (affix trace) -- are not
;; checked, since elspell does not implement morphological
;; analysis.  Dictionaries that `elspell-create' refuses outright
;; (currently, only ones using COMPLEXPREFIXES) fail; see the
;; Commentary in elspell.el for this and other limitations of this
;; port that may cause some dictionaries' good/wrong words, or
;; suggestions, to be misclassified.
;;
;; A handful of the tests this generates are known to fail outright --
;; some because of limitations already documented in elspell.el,
;; others because of genuine Hunspell/Nuspell behavioral divergences,
;; or (more rarely) a gap specific to this port.  Each such test name
;; is listed in `elspell-tests-known-failures' along with why, and
;; is declared with ERT's `:expected-result :failed' (an "xfail") so
;; that it reports as an expected failure rather than an unexpected
;; one; if it ever starts passing, ERT reports that as a failure too
;; (an "unexpected pass"), which is the prompt to remove it from that
;; list.
;;
;; Run with, e.g.:
;;   emacs -batch -L . -l ert -l elspell-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'elspell)

(defconst elspell-tests-project-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing elspell.el and elspell-tests.el.")

(defconst elspell-tests-hunspell-dir
  (expand-file-name "hunspell/tests/" elspell-tests-project-dir)
  "Directory holding Hunspell's own affix/dictionary/word-list tests.")

(unless (file-directory-p elspell-tests-hunspell-dir)
  (error "elspell-tests: no such directory %s" elspell-tests-hunspell-dir))

(defconst elspell-tests-known-failures
  '((elspell-test-hunspell-alias3 .
     "COMPLEXPREFIXES is not supported; `elspell-create' refuses the
dictionary, per the Commentary in elspell.el.")
    (elspell-test-hunspell-base_utf .
     "Turkish dotless i (İ/ı) casing; casing uses Emacs's own locale-unaware
upcase/downcase, per the Commentary in elspell.el.")
    (elspell-test-hunspell-break .
     "Tests recursive multi-dash BREAK splitting (\"e-mail-foo\"); Nuspell's
BREAK handling, which this ports, only retries the first occurrence of
each pattern per split attempt, so a word with more than one break
point fails here too.")
    (elspell-test-hunspell-checkcompoundpattern5 .
     "The pattern's condition flag arrives through an affix's continuation
class; like Nuspell, this port only looks at the root's own flags.")
    (elspell-test-hunspell-checkcompoundpattern6 .
     "The pattern's condition flag arrives through an affix's continuation
class; like Nuspell, this port only looks at the root's own flags.")
    (elspell-test-hunspell-checkcompoundpattern7 .
     "The pattern's condition flag arrives through an affix's continuation
class; like Nuspell, this port only looks at the root's own flags.")
    (elspell-test-hunspell-complexprefixes .
     "COMPLEXPREFIXES is not supported; `elspell-create' refuses the
dictionary, per the Commentary in elspell.el.")
    (elspell-test-hunspell-complexprefixes2 .
     "COMPLEXPREFIXES is not supported; `elspell-create' refuses the
dictionary, per the Commentary in elspell.el.")
    (elspell-test-hunspell-complexprefixesutf .
     "COMPLEXPREFIXES is not supported; `elspell-create' refuses the
dictionary, per the Commentary in elspell.el.")
    (elspell-test-hunspell-compoundforbid .
     "COMPOUNDFORBIDFLAG on a direct .dic entry (\"foobaz/Z\") has no effect
on an unrelated derivation of the same string from a different root
+ suffix (\"foo\"+\"baz\"), since that derivation never looks at the
other entry's flags at all; `-check-word-in-compound' mirrors
Nuspell's own `check_word_in_compound' exactly here, so this appears
to be a genuine gap in Nuspell/Hunspell's COMPOUNDFORBIDFLAG
semantics, not something specific to this port.")
    (elspell-test-hunspell-dotless_i .
     "Turkish dotless i (İ/ı) casing (LANG tr); casing uses Emacs's own
locale-unaware upcase/downcase, per the Commentary in elspell.el.")
    (elspell-test-hunspell-gh106 .
     "Tests a Hunspell-specific fix for case-insensitive affix conditions on
PASCAL-cased words; Nuspell's checker, which this ports, never retries
case variants for CAMEL/PASCAL words, so this diverges from Hunspell
upstream too.")
    (elspell-test-hunspell-gh151 .
     "SIMPLIFIEDTRIPLE's extra \"insert the elided letter back and retry\"
fallback is not implemented, per the LLM notes to self in elspell.el.")
    (elspell-test-hunspell-gh353 .
     "Hunspell treats a word made only of WORDCHARS-listed non-ASCII digits
as a number; Nuspell's own `is_number' (which `elspell-check'
mirrors) only recognizes ASCII digits, so this diverges from Hunspell
upstream too.")
    (elspell-test-hunspell-hu .
     "Hungarian COMPOUNDSYLLABLE/SYLLABLENUM (the exception letting a
compound exceed COMPOUNDWORDMAX) and the LANG_hu \"moving rule\" for
COMPOUNDFORBIDFLAG are not implemented, per the LLM notes to self in
elspell.el.")
    (elspell-test-hunspell-limit-multiple-compounding .
     "Tests a Hunspell-specific heuristic rejecting a 3-or-more-part compound
that is a near-typo of an unrelated, longer dictionary word; Nuspell's
checker, which this ports, has no such heuristic.")
    (elspell-test-hunspell-nepali .
     "Tests ICONV/IGNORE normalization of ZWJ/ZWNJ in Devanagari conjuncts;
this port's substring replacer does not reproduce Hunspell's exact
normalization here.")
    (elspell-test-hunspell-ph2 .
     "A dictionary entry with an embedded space (e.g. \"forbidden root\") is
meant to block the squished-together compound from also being
accepted; this multi-word .dic entry mechanism is not implemented
\(same gap as the wordpair test).")
    (elspell-test-hunspell-simplifiedtriple .
     "SIMPLIFIEDTRIPLE's extra \"insert the elided letter back and retry\"
fallback is not implemented, per the LLM notes to self in elspell.el.")
    (elspell-test-hunspell-wordpair .
     "A dictionary entry with an embedded space (e.g. \"compound word\") is
meant to block the squished-together compound (\"compoundword\") from
also being accepted; `-parse-dic-line' stops at the first whitespace,
so this multi-word .dic entry mechanism is not implemented.")
    (elspell-test-hunspell-suggest-base_utf .
     "Turkish dotless i (İ) casing; casing uses Emacs's own locale-unaware
upcase/downcase, per the Commentary in elspell.el.")
    (elspell-test-hunspell-suggest-checksharps .
     "CHECKSHARPS (German sharp s) suggestion casing variants are narrower
than Hunspell's own; Nuspell's test suite marks this one as failing
too.")
    (elspell-test-hunspell-suggest-checksharpsutf .
     "CHECKSHARPS (German sharp s) suggestion casing variants are narrower
than Hunspell's own; Hunspell drops such a suggestion, Nuspell does not.")
    (elspell-test-hunspell-suggest-nosuggest .
     "NOSUGGEST only filters n-gram suggestions, as in Nuspell, whose
`add_sug_if_correct' ignores it; Nuspell's test suite marks this one
as failing too.")
    (elspell-test-hunspell-suggest-opentaal_keepcase .
     "KEEPCASE casing restoration for a multi-dash PASCAL-cased compound
\(\"TV-word-TV\") only fixes one dash-part at a time, not both together.")
    (elspell-test-hunspell-suggest-ph .
     "Hunspell turns ph: fields of .dic entries (known misspellings of a
word) into suggestions; like Nuspell, this port ignores morphological
fields, and PHONE is not implemented either.")
    (elspell-test-hunspell-suggest-ph2 .
     "Hunspell turns ph: fields of .dic entries (known misspellings of a
word) into suggestions; like Nuspell, this port ignores morphological
fields."))
  "Alist of ERT test name (a symbol) to why it is a known failure.
Tests named here are declared with `:expected-result :failed'; see
`elspell-tests--maybe-xfail'.  A test listed here that starts
passing is itself reported as a failure (an \"unexpected pass\"),
which is ERT's way of prompting this list to be updated.")

(defun elspell-tests--maybe-xfail (test-name)
  "Return the `:expected-result' spec for TEST-NAME (a symbol), or nil.
Looks TEST-NAME up in `elspell-tests-known-failures'; spliced into
a generated `ert-deftest' form when non-nil."
  (when (assq test-name elspell-tests-known-failures)
    '(:expected-result :failed)))

(defun elspell-tests--read-words (file)
  "Return the list of whitespace-separated words of FILE.
Like Hunspell's and Nuspell's own test runners, a line with several
words yields each of them separately.  Return nil if FILE does not
exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string)))))

(defun elspell-tests--check (name)
  "Check hunspell test NAME's .good/.wrong word lists against elspell.
NAME is looked up as NAME.aff/NAME.dic/NAME.good/NAME.wrong under
`elspell-tests-hunspell-dir'.  Return nil if everything checks out,
or (fail . MESSAGE) if `elspell-create' itself failed or some
word was misclassified."
  (cl-block nil
    (let ((elspell-dictionary-path (list elspell-tests-hunspell-dir))
          (good-file (expand-file-name (concat name ".good") elspell-tests-hunspell-dir))
          (wrong-file (expand-file-name (concat name ".wrong") elspell-tests-hunspell-dir))
          (dict nil))
      (condition-case err
          (setq dict (elspell-create name))
        (error
         (cl-return-from nil
           (cons 'fail (format "elspell-create failed: %s" (error-message-string err))))))
      ;; For Hunspell, a timeout is a bad word; we return `timeout'.
      (let ((bad-good (seq-remove (lambda (w) (eq t (elspell-check dict w)))
                                   (elspell-tests--read-words good-file)))
            (bad-wrong (seq-filter (lambda (w) (eq t (elspell-check dict w)))
                                    (elspell-tests--read-words wrong-file))))
        (if (or bad-good bad-wrong)
            (cons 'fail
                  (concat
                   (and bad-good
                        (format "Good words rejected as incorrect: %S" bad-good))
                   (and bad-good bad-wrong "\n")
                   (and bad-wrong
                        (format "Wrong words accepted as correct: %S" bad-wrong))))
          nil)))))

(defun elspell-tests--run-case (checker name)
  "Run CHECKER for hunspell test NAME.
CHECKER is `elspell-tests--check' or
`elspell-tests--check-suggest'."
  (pcase (funcall checker name)
    ('nil t)
    (`(fail . ,msg) (ert-fail msg))))

(defun elspell-tests--define-hunspell-tests ()
  "Define one `ert-deftest' per Hunspell test dictionary with word lists."
  (dolist (aff-file (directory-files elspell-tests-hunspell-dir t "\\.aff\\'"))
    (let* ((name (file-name-sans-extension (file-name-nondirectory aff-file)))
           (dic-file (expand-file-name (concat name ".dic") elspell-tests-hunspell-dir))
           (good-file (expand-file-name (concat name ".good") elspell-tests-hunspell-dir))
           (wrong-file (expand-file-name (concat name ".wrong") elspell-tests-hunspell-dir)))
      (when (and (file-exists-p dic-file)
                 (or (file-exists-p good-file) (file-exists-p wrong-file)))
        (let ((test-name (intern (format "elspell-test-hunspell-%s" name))))
          (eval
           `(ert-deftest ,test-name ()
              ,(format "Check hunspell/tests/%s.{good,wrong} with elspell." name)
              ,@(elspell-tests--maybe-xfail test-name)
              (elspell-tests--run-case #'elspell-tests--check ,name))
           t))))))

(elspell-tests--define-hunspell-tests)

;;;; Suggestions

(defun elspell-tests--read-lines (file)
  "Return FILE's lines as a list of strings, or nil if FILE does not exist.
Unlike `elspell-tests--read-words', this keeps each line's
whitespace intact and preserves blank lines, except for trailing
ones, which Hunspell's own test script ignores too."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((lines (mapcar (lambda (l) (string-trim-right l "[ \t\r]+"))
                            (split-string (buffer-string) "\n"))))
        (nreverse (drop-while #'string-empty-p (nreverse lines)))))))

(defun elspell-tests--check-suggest (name)
  "Check hunspell test NAME's .wrong/.sug word lists against elspell.
NAME is looked up as NAME.aff/NAME.dic/NAME.wrong/NAME.sug under
`elspell-tests-hunspell-dir'.  Like Hunspell's own test script,
which keeps only the \"&\" lines of `hunspell -a', this pairs the
.sug lines, in order, with the .wrong words that are misspelled and
get at least one suggestion; a different number of such words is a
failure.  A .sug line may list several Hunspell-acceptable
alternatives, comma-separated; this only requires `elspell-suggest'
to include at least one of them, not to match the whole list or its
order -- `elspell-suggest' is a Nuspell-derived port and is not
expected to reproduce Hunspell's own suggestions verbatim.  Return nil
if everything checks out, or (fail . MESSAGE) if `elspell-create'
itself failed or the suggestions did not match."
  (cl-block nil
    (let ((elspell-dictionary-path (list elspell-tests-hunspell-dir))
          (wrong-file (expand-file-name (concat name ".wrong") elspell-tests-hunspell-dir))
          (sug-file (expand-file-name (concat name ".sug") elspell-tests-hunspell-dir))
          (dict nil))
      (condition-case err
          (setq dict (elspell-create name))
        (error
         (cl-return-from nil
           (cons 'fail (format "elspell-create failed: %s" (error-message-string err))))))
      (let ((sug-lines (elspell-tests--read-lines sug-file))
            (suggested nil))
        (dolist (word (elspell-tests--read-lines wrong-file))
          (unless (eq t (elspell-check dict word))
            (let ((got (elspell-suggest dict word)))
              (when got
                (push (cons word got) suggested)))))
        (setq suggested (nreverse suggested))
        (unless (= (length suggested) (length sug-lines))
          (cl-return-from nil
            (cons 'fail (format "%d words got suggestions, but %s.sug has %d lines: %S"
                                 (length suggested) name (length sug-lines) suggested))))
        (let (bad)
          (cl-mapc
           (lambda (entry expected)
             (let ((alternatives (split-string expected ", " t)))
               (unless (cl-intersection alternatives (cdr entry) :test #'string=)
                 (push (format "%S: expected one of %S, got %S"
                               (car entry) alternatives (cdr entry))
                       bad))))
           suggested sug-lines)
          (if bad
              (cons 'fail (string-join (nreverse bad) "\n"))
            nil))))))

(defun elspell-tests--define-hunspell-suggest-tests ()
  "Define one `ert-deftest' per Hunspell test dictionary with a .sug file."
  (dolist (aff-file (directory-files elspell-tests-hunspell-dir t "\\.aff\\'"))
    (let* ((name (file-name-sans-extension (file-name-nondirectory aff-file)))
           (dic-file (expand-file-name (concat name ".dic") elspell-tests-hunspell-dir))
           (wrong-file (expand-file-name (concat name ".wrong") elspell-tests-hunspell-dir))
           (sug-file (expand-file-name (concat name ".sug") elspell-tests-hunspell-dir)))
      (when (and (file-exists-p dic-file) (file-exists-p wrong-file) (file-exists-p sug-file))
        (let ((test-name (intern (format "elspell-test-hunspell-suggest-%s" name))))
          (eval
           `(ert-deftest ,test-name ()
              ,(format "Check hunspell/tests/%s.{wrong,sug} with elspell-suggest." name)
              ,@(elspell-tests--maybe-xfail test-name)
              (elspell-tests--run-case #'elspell-tests--check-suggest ,name))
           t))))))

(elspell-tests--define-hunspell-suggest-tests)

;;;; Personal dictionary

(ert-deftest elspell-test-personal-dictionary ()
  "Check `elspell-add-word' and its interaction with `elspell-create'.
A word added without saving is usable right away but forgotten by a
later `elspell-create'; one added with SAVE is both usable right
away and, because it ends up in the personal dictionary file, still
known to a later `elspell-create'."
  (let* ((tmpdir (make-temp-file "elspell-test-pdict-" t))
         (elspell-dictionary-path (list elspell-tests-hunspell-dir))
         (elspell-personal-dictionary-format (expand-file-name "personal-%s" tmpdir))
         (pdict-file (format elspell-personal-dictionary-format "base")))
    (unwind-protect
        (progn
          (let ((dict (elspell-create "base")))
            (should-not (elspell-check dict "loooked"))
            (should-not (elspell-check dict "zyzzyva"))
            (elspell-add-word dict "loooked")
            (should (elspell-check dict "loooked"))
            (should-not (file-exists-p pdict-file))
            (elspell-add-word dict "zyzzyva" t)
            (should (elspell-check dict "zyzzyva"))
            (should (file-exists-p pdict-file)))
          (let ((dict (elspell-create "base")))
            (should-not (elspell-check dict "loooked"))
            (should (elspell-check dict "zyzzyva"))
            (should (elspell-check dict "created"))
            (should-not (elspell-check dict "texxt"))))
      (delete-directory tmpdir t))))

(ert-deftest elspell-test-remove-word ()
  "Check `elspell-remove-word' as the inverse of `elspell-add-word'.
Removing a word that was never added reports nil; removing one added
without SAVE forgets it right away; removing one added with SAVE both
forgets it right away and, because it is scrubbed from the personal
dictionary file, keeps it forgotten by a later `elspell-create'.
Removing a word a second time reports nil.  A real dictionary word is
left alone, since it has no personal (nil-flags) entry to remove."
  (let* ((tmpdir (make-temp-file "elspell-test-pdict-" t))
         (elspell-dictionary-path (list elspell-tests-hunspell-dir))
         (elspell-personal-dictionary-format (expand-file-name "personal-%s" tmpdir))
         (pdict-file (format elspell-personal-dictionary-format "base")))
    (unwind-protect
        (let ((dict (elspell-create "base")))
          (should-not (elspell-remove-word dict "zyzzyva"))

          (elspell-add-word dict "loooked")
          (should (elspell-remove-word dict "loooked"))
          (should-not (elspell-check dict "loooked"))

          (elspell-add-word dict "zyzzyva" t)
          (should (elspell-check dict "zyzzyva"))
          (should (elspell-remove-word dict "zyzzyva" t))
          (should-not (elspell-check dict "zyzzyva"))
          (should-not (elspell-remove-word dict "zyzzyva" t))

          (should (elspell-check dict "created"))
          (should-not (elspell-remove-word dict "created"))
          (should (elspell-check dict "created"))

          (should-not (elspell-check dict "zyzzyva"))
          (should (with-temp-buffer
                    (insert-file-contents pdict-file)
                    (not (string-search "zyzzyva" (buffer-string))))))
      (delete-directory tmpdir t))))

;;;; Word regexp

(defun elspell-tests--words (dict text)
  "Return the list of words of TEXT, according to `elspell-word-regexp'."
  (let ((re (elspell-word-regexp dict))
        (start 0) words)
    (while (string-match re text start)
      (push (match-string 0 text) words)
      (setq start (match-end 0)))
    (nreverse words)))

(ert-deftest elspell-test-word-regexp ()
  "Check `elspell-word-regexp' against a few WORDCHARS settings."
  (let ((elspell-dictionary-path (list elspell-tests-hunspell-dir)))
    ;; WORDCHARS .'’
    (let ((dict (elspell-create "base_utf")))
      (should (eq (elspell-word-regexp dict) (elspell-word-regexp dict)))
      (should (equal (elspell-tests--words
                      dict "'Quoted' don’t ‘it’ e.g. a Ünïcode-x2 _a_")
                     '("Quoted" "don’t" "it" "e.g." "Ünïcode"))))
    ;; WORDCHARS -–
    (let ((dict (elspell-create "break")))
      (should (equal (elspell-tests--words dict "-foo-bar– don't --")
                     '("foo-bar" "don"))))))

(provide 'elspell-tests)
;;; elspell-tests.el ends here
