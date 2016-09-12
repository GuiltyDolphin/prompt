;;; prompt.el --- utilities for working with text prompts.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Copyright (C) 2016 Ben Moon
;; Author: Ben Moon <guiltydolphin@gmail.com>
;; URL: https://github.com/guiltydolphin/prompt
;; Git-Repository: git://github.com/guiltydolphin/prompt.git
;; Created: 2016-09-12
;; Version: 0.1.0
;; Keywords: input, minibuffer
;; Package-Requires: ((dash "2.13.0"))

;;; Commentary:

;; Prompt provides utilities for working with text prompts and read completion.
;;
;; For an example of prompt's enhanced completing-read,
;; see `prompt-completing-read-variable', which supports reading any variable
;; with a predicate, for example to prompt for a keymap with completion, you
;; could use:
;;
;;    (prompt-completing-read-variable "Enter keymap: " 'keymapp)

;;; Code:

(require 'dash)

(defgroup prompt nil
  "Text-input and read-completion utilities."
  :group 'minibuffer
  :prefix 'prompt-)

(defcustom prompt-default-prompt-suffixes '(":" "?")
  "Default accepted prompt suffixes."
  :group 'prompt
  :type '(repeat string))

(defun prompt--split (prompt suffixes)
  "Split PROMPT into its main body and suffix in the form (BODY SUFFIX).
SUFFIXES should be a list of possible suffixes."
  (save-match-data
    (let ((prompt-regex
           (format "^\\(.+?\\)\\(%s\\|\\)\s*$"
                   (apply 'concat (-interpose "\\|" (-map 'regexp-quote suffixes))))))
      (string-match prompt-regex prompt)
      (list (match-string 1 prompt) (match-string 2 prompt)))))

(defun prompt--make-prompt (prompt suffixes default)
  "Normalize PROMPT using SUFFIXES as the trailing suffixes.
DEFAULT should be the default value to indicate in the prompt, or NIL for no default value."
  (-let (((pbody psuffix) (prompt--split prompt suffixes)))
    (format "%s%s%s " pbody (if default (format " (default %s)" default) "") psuffix)))

(defun prompt--make-interactive-prompt (prompt default)
  "Normalize PROMPT using `prompt-default-prompt-suffixes' as the suffixes.
DEFAULT should be the default value to indicate in the prompt, or NIL for no default value."
  (prompt--make-prompt prompt prompt-default-prompt-suffixes default))

(defun prompt-completing-read-variable (prompt &optional predicate def)
  "Read the name of a variable in the minibuffer, with completion.
See `completing-read' for the meaning of PROMPT, PREDICATE, and DEF."
  (let ((v (or def (variable-at-point)))
        (enable-recursive-minibuffers t)
        (check
         (lambda (it)
           (and (symbolp it)
                (boundp it)
                (if predicate (funcall predicate (symbol-value it)) t))))
        vars val)
    (mapatoms (lambda (atom) (when (funcall check atom) (push atom vars))))
    (setq val (completing-read
               (prompt--make-interactive-prompt prompt (when (funcall check v) v))
               vars check t nil nil
               (if (symbolp v) (symbol-name v))))
    (list (if (equal val "") v (intern val)))))

(provide 'prompt)
;;; prompt.el ends here
