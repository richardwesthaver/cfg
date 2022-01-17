;;; lob-cfg.el --- LoB Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: Richard Westhaver <ellis@rwest.io>
;; Keywords: languages

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

;; 

;;; Code:
(defgroup lob nil
  "Meta-programming extensions")

(defcustom lob-file-name "~/org/lob.org"
  "Filename for an org-mode buffer containing the Library of Babel"
  :type 'string
  :group 'lob
  :safe 'stringp)

(defcustom lob-ingest-trigger 'on-save
  "Control when 'org-babel-lob-ingest` will be executed."
  :type '(choice (const :tag "Ingest on `lob-file` save." on-save)
		 (const :tag "Trigger lob-ingest manually." nil))
  :group 'lob)

(defvar lob-file (expand-file-name lob-file-name)
  "library-of-babel file, usually 'lob.org'")

  ;; populate org-babel
(org-babel-do-load-languages
 ;; TODO 2021-10-24: bqn, apl, k
 'org-babel-load-languages '((shell . t)
			     (emacs-lisp . t)
			     (eshell . t)
			     (sed . t)
			     (awk . t)
			     (dot . t)
			     (js . t)
			     (C . t)
			     (python . t)
			     (lua . t)
			     (lilypond . t)))

;;;###autoload
(defun lob-refresh ()
  (interactive)
  "add contents of 'lob-file' to 'org-babel-library-of-babel'"
  (org-babel-lob-ingest lob-file))

(defun lob-file-active-p ()
  "Non-nil if the active buffer is `lob-file`"
  (string= (buffer-file-name) lob-file))

;;; Hooks 
(defun lob-ingest-hook ()
  "function to run after 'org-babel-library-of-babel' is populated")

(defun lob-after-save-hook ()
  "lob.org `after-save-hook` when lob-ingest-trigger = on-save
and `lob-file-active-p` is non-nil."
  (when (and (eq lob-ingest-trigger 'on-save)
	     (lob-file-active-p))
    (org-babel-lob-ingest lob-file)))

(defun lob--mode-prefix (mode)
  "Return MODE name or empty string in nil."
  (if mode
      (string-trim-right (symbol-name mode) (rx "mode" eos))
    ""))
(defun lob--abbrev-table (mode)
  "Get abbrev table for MODE or `global-abbrev-table' if nil."
  (if mode
      (derived-mode-abbrev-table-name mode)
    'global-abbrev-table))

(defun org-sbx-call (name header args)
  (let* ((args (mapconcat
                (lambda (x)
                  (format "%s=%S" (symbol-name (car x)) (cadr x)))
                args ", "))
         (ctx (list 'babel-call (list :call name
                                      :name name
                                      :inside-header header
                                      :arguments args
                                      :end-header ":results silent")))
         (info (org-babel-lob-get-info ctx)))
    (when info (org-babel-execute-src-block nil info))))

(defmacro org-sbx (name &rest args)
  (let* ((header (if (stringp (car args)) (car args) nil))
	 (args (if (stringp (car args)) (cdr args) args)))
    (unless (stringp name)
      (setq name (symbol-name name)))
    (let ((result (org-sbx-call name header args)))
      (org-trim (if (stringp result) result (format "%S" result))))))
 
(provide 'lob-cfg)
;;; lob-cfg.el ends here
