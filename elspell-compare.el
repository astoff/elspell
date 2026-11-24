;;; elspell-compare.el --- Compare elspell.el with Hunspell  -*- lexical-binding: t; -*-

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

;; Checks, for every dictionary in the project's data/ directory,
;; whether elspell and Hunspell agree on which words of a text are
;; misspelled -- by default Emacs's HELLO file, which has words in many
;; scripts, as shown by `view-hello-file' (so with its text/enriched
;; markup decoded), or else a given text file.
;;
;; hunspell-check --print (see hunspell-check.c and the Makefile)
;; splits the text into words and checks each with Hunspell_spell();
;; the distinct words it found are then also checked with
;; `elspell-check'.  A word for which `elspell-check' times out
;; counts as correct.
;;
;; Some words are left out of the comparison, since Hunspell cannot
;; check them: those with characters outside the Basic Multilingual
;; Plane, which Hunspell's 16-bit `w_char' cannot represent, and, for a
;; dictionary in an 8-bit encoding, those the encoding cannot represent.
;;
;; This is a batch-only, noninteractive script.  Run with, e.g.:
;;   emacs -batch -L . -l elspell-compare.el -f elspell-compare [FILE]
;;
;; It reports the words the two disagree on for each dictionary, and
;; exits with a nonzero status if there are any.

;;; Code:

(require 'elspell)

(defconst elspell-compare--project-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing elspell.el and elspell-compare.el.")

(defconst elspell-compare--data-dir
  (expand-file-name "data/" elspell-compare--project-dir)
  "Directory containing the dictionaries to compare.")

(defvar elspell-compare-hunspell-check
  (expand-file-name ".tmp/hunspell-check" elspell-compare--project-dir)
  "The hunspell-check executable, built from hunspell-check.c.")

(setq elspell-dictionary-path (list elspell-compare--data-dir))

(setq elspell-personal-dictionary-format "/dev/null/%s")

(defun elspell-compare--write-hello-file (file)
  "Write the text of the HELLO file, as `view-hello-file' shows it, to FILE."
  (save-window-excursion
    (view-hello-file)
    (let ((coding-system-for-write 'utf-8))
      (write-region nil nil file nil 'silent))
    (kill-buffer)))

(defun elspell-compare--checkable-p (dict word)
  "Return non-nil if Hunspell can check WORD with DICT; see Commentary."
  (and (not (string-match-p (rx (any (#x10000 . #x10ffff))) word))
       (not (unencodable-char-position 0 nil (elspell--dict-coding dict)
                                       nil word))))

(defun elspell-compare--hunspell (name file)
  "Check FILE's words with Hunspell's dictionary NAME.
Return an alist of (WORD . CORRECT) for the distinct words, in order."
  (let ((coding-system-for-read 'utf-8)
        ;; hunspell-check looks for the dictionary in data/.
        (default-directory elspell-compare--project-dir)
        (seen (make-hash-table :test #'equal))
        (results nil))
    (dolist (line (process-lines elspell-compare-hunspell-check
                                 "--print" name file))
      (pcase line
        ((rx bos (let verdict (or "good" "bad")) ": " (let word (* anything)) eos)
         (unless (gethash word seen)
           (puthash word t seen)
           (push (cons word (equal verdict "good")) results)))))
    (nreverse results)))

(defun elspell-compare (&optional file)
  "Compare elspell and Hunspell on FILE's words, for each dictionary.
When FILE is nil, take it from the leftover command-line arguments,
or use Emacs's HELLO file if there are none; see Commentary.  Then
exit Emacs, with status 0 if they agree on every dictionary and 1
otherwise."
  (let ((file (or file (pop command-line-args-left)))
        (hello-file nil)
        (agreed 0)
        (disagreed nil)
        (errored nil))
    (unless file
      (setq hello-file (make-temp-file "elspell-compare"))
      (elspell-compare--write-hello-file hello-file)
      (setq file hello-file))
    (unwind-protect
        (dolist (name (elspell-list-dictionaries))
          (condition-case err
              (let* ((dict (elspell-create name))
                     (all-results (elspell-compare--hunspell name file))
                     (results (seq-filter (lambda (r)
                                            (elspell-compare--checkable-p dict (car r)))
                                          all-results))
                     (only-ours nil)
                     (only-theirs nil)
                     (correct 0))
                (pcase-dolist (`(,word . ,theirs) results)
                  (let ((ours (and (elspell-check dict word) t)))
                    (when ours (incf correct))
                    (pcase (cons ours theirs)
                      ('(nil . t) (push word only-ours))
                      ('(t . nil) (push word only-theirs)))))
                (if (or only-ours only-theirs)
                    (progn
                      (push name disagreed)
                      (message "%s: MISMATCH (%d words, %d skipped, %d correct per elspell)"
                               name (length results)
                               (- (length all-results) (length results)) correct)
                      (when only-ours
                        (message "  only elspell rejects: %s"
                                 (string-join (nreverse only-ours) " ")))
                      (when only-theirs
                        (message "  only Hunspell rejects:   %s"
                                 (string-join (nreverse only-theirs) " "))))
                  (incf agreed)
                  (message "%s: ok (%d words, %d skipped, %d correct)"
                           name (length results)
                           (- (length all-results) (length results)) correct)))
            (error
             (push name errored)
             (message "%s: ERROR %s" name (error-message-string err)))))
      (when hello-file
        (delete-file hello-file)))
    (message "\nSummary:")
    (message "- %d dictionaries agree" agreed)
    (pcase-dolist (`(,names . ,what) `((,disagreed . "disagree")
                                       (,errored . "failed to load")))
      (message "- %d dictionaries %s%s" (length names) what
               (if names (format ": %s" (string-join (reverse names) " ")) "")))
    (kill-emacs (if (or disagreed errored) 1 0))))

(provide 'elspell-compare)
;;; elspell-compare.el ends here
