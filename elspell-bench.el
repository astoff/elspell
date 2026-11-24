;;; elspell-bench.el --- Benchmarks for elspell.el  -*- lexical-binding: t; -*-

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

;; Measures how long `elspell-create' takes to load a dictionary,
;; and how long `elspell-check' then takes to check every word of a
;; text file -- the dictionary name and the file are given as
;; command-line arguments (see `elspell-bench').  The file is
;; read into a temporary buffer once, up front; checking time is then
;; measured over a single forward scan of that buffer that finds each
;; word and checks it immediately, rather than first collecting the
;; words into a list and only then checking them.
;;
;; With the --suggest switch, it also measures how long
;; `elspell-suggest' takes, called once for each misspelled word
;; occurrence the check found, in a separately timed pass after it.
;; With --lines N, only the first N lines of the file are used.
;;
;; hunspell-check.c is the C counterpart, measuring Hunspell the
;; same way on the same files (see the Makefile's bench target).
;; Dictionaries are loaded only from the project's data/ directory,
;; where the Makefile puts them, and no personal dictionary is used.
;;
;; This is a batch-only, noninteractive script.  Run with, e.g.:
;;   emacs -batch -L . -l elspell-bench.el -f elspell-bench \
;;         [--suggest] [--lines N] en_US data/alice.txt
;;
;; Unless `native-comp-eln-load-path' is nil (e.g. because the caller
;; cleared it to force a pure byte-compiled measurement), this makes
;; sure elspell.el is actually running native-compiled code first,
;; synchronously native-compiling (and reloading) it if it is not --
;; useful since plain `emacs -batch -l elspell-bench.el' only ever
;; runs byte-compiled code unless a matching .eln happens to already
;; sit in the native-comp cache.  Either way, which one it ran with is
;; reported in the first line of the output.

;;; Code:

(require 'benchmark)
(require 'elspell)

;; Set it to the default interactive value (batch mode raises it to
;; 1.0).
(setq gc-cons-percentage 0.1)

(defconst elspell-bench--project-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing elspell.el and elspell-bench.el.")

(setq elspell-dictionary-path
      (list (expand-file-name "data/" elspell-bench--project-dir)))

(setq elspell-personal-dictionary-format "/dev/null/%s")

(defun elspell-bench--ensure-native ()
  "Make sure elspell.el runs native-compiled, unless the caller
disabled native compilation by setting `native-comp-eln-load-path' to
nil, in which case this does nothing."
  (when (and native-comp-eln-load-path
             (not (native-comp-function-p (symbol-function 'elspell-check))))
    (if (native-comp-available-p)
        (load (native-compile (expand-file-name "elspell.el" elspell-bench--project-dir)))
      (error "elspell-bench: native compilation is not available"))))

(defun elspell-bench (&optional dict-name file suggest lines)
  "Benchmark loading dictionary DICT-NAME and checking FILE's words.
If SUGGEST is non-nil, also benchmark computing suggestions for each
misspelled word occurrence found, in a separately timed pass.  If LINES
is non-nil, use only the first LINES lines of FILE.

When DICT-NAME is nil, all four arguments are taken from the leftover
command-line arguments instead, for use as `emacs -batch -l
elspell-bench.el -f elspell-bench [--suggest] [--lines N]
DICT-NAME FILE'.  This function is noninteractive -- it is meant to be
run this way, not via \\[execute-extended-command].

Unless `native-comp-eln-load-path' is nil, this ensures elspell.el
runs native-compiled before benchmarking it; see the Commentary above.

Return and (via `message') report (WORD-COUNT CORRECT-COUNT
LOAD-SECONDS LOAD-GCS LOAD-GC-SECONDS CHECK-SECONDS CHECK-GCS
CHECK-GC-SECONDS), followed, if SUGGEST, by (SUGGESTION-COUNT
SUGGEST-SECONDS SUGGEST-GCS SUGGEST-GC-SECONDS); the
*-SECONDS/-GCS/-GC-SECONDS triples are as produced by `benchmark-run'."
  (unless dict-name
    (while (string-prefix-p "--" (car command-line-args-left))
      (pcase (pop command-line-args-left)
        ("--suggest" (setq suggest t))
        ("--lines" (setq lines (string-to-number (pop command-line-args-left))))
        (opt (error "elspell-bench: unknown option %s" opt))))
    (setq dict-name (pop command-line-args-left)
          file (pop command-line-args-left)))
  (unless dict-name (error "elspell-bench: no dictionary name given"))
  (unless file (error "elspell-bench: no word-list file given"))
  (let* ((dict nil)
         (word-count 0)
         (correct-count 0)
         (misspelled nil)
         (suggestion-count 0))
    (elspell-bench--ensure-native)
    (pcase-let*
        ((`(,load-seconds ,load-gcs ,load-gc-seconds)
          (benchmark-run (setq dict (elspell-create dict-name))))
         (`(,check-seconds ,check-gcs ,check-gc-seconds)
          (with-temp-buffer
            (insert-file-contents file)
            (when lines
              (goto-char (point-min))
              (forward-line lines)
              (delete-region (point) (point-max)))
            (goto-char (point-min))
            (garbage-collect)
            (benchmark-run
             (while (re-search-forward (rx alpha (? (* (any alpha ?' ?-)) alpha)) nil t)
               (let ((word (match-string-no-properties 0)))
                 (incf word-count)
                 (cond ((elspell-check dict word) (incf correct-count))
                       (suggest (push word misspelled))))))))
         (`(,suggest-seconds ,suggest-gcs ,suggest-gc-seconds)
          (when suggest
            (setq misspelled (nreverse misspelled))
            (garbage-collect)
            (benchmark-run
             (dolist (word misspelled)
               (incf suggestion-count (length (elspell-suggest dict word))))))))
      (message "\
benchmark:  %s %s %s
load dict:  %.3fs, %d GCs taking %.3fs
check:      %d words, %d correct
            %.3fs total, %d GCs taking %.3fs
            %.2f µs/word, %.2f µs/word excluding GC"
               (if (native-comp-function-p (symbol-function 'elspell-check))
                   "native"
                 "bytecomp")
               dict-name file
               load-seconds load-gcs load-gc-seconds
               word-count correct-count
               check-seconds check-gcs check-gc-seconds
               (/ (* 1e6 check-seconds) (max 1 word-count))
               (/ (* 1e6 (- check-seconds check-gc-seconds)) (max 1 word-count)))
      (when suggest
        (message "\
suggest:    %d words, %d suggestions
            %.3fs total, %d GCs taking %.3fs
            %.2f ms/word, %.2f ms/word excluding GC"
                 (length misspelled) suggestion-count
                 suggest-seconds suggest-gcs suggest-gc-seconds
                 (/ (* 1e3 suggest-seconds) (max 1 (length misspelled)))
                 (/ (* 1e3 (- suggest-seconds suggest-gc-seconds))
                    (max 1 (length misspelled)))))
      (append (list word-count correct-count load-seconds load-gcs load-gc-seconds
                    check-seconds check-gcs check-gc-seconds)
              (and suggest
                   (list suggestion-count
                         suggest-seconds suggest-gcs suggest-gc-seconds))))))

(provide 'elspell-bench)
;;; elspell-bench.el ends here
