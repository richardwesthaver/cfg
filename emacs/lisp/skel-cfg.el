;;; skel-cfg.el --- Skel Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Richard Westhaver

;; Author: Richard Westhaver <ellis@jekyll>
;; Keywords: convenience

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
(defcustom file-template-insert-automatically nil
  "*Insert file-template automatically.
Can be one of the following values:

nil - do not insert automatically.
t   - always insert automatically.
ask - ask whether to insert or not."
  :group 'default
  :type '(choice (const :tag "No"  nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" 'ask)))

(defvar skel-available '()
  "Internal list of available default skeletons.")

(define-abbrev-table 'default-skel-abbrev-table ()
  "Abbrev table for Default skeletons."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*")

(defmacro default-skel-define (name doc &rest skel)
  "Define a default skeleton using NAME DOC and SKEL. The skeleton
will be bound to default-skel-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "default-skel-" name))))
    `(progn
       (define-abbrev default-skel-abbrev-table
         ,name "" ',function-name :system t)
       (setq skel-available
             (cons ',function-name skel-available))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert default %s skeleton." name))
         ,@skel))))

(define-abbrev-table 'default-abbrev-table ()
  "Default Abbrev table"
  :parents (list default-skel-abbrev-table))

(default-skel-define web
    "Adds a link to 'default-website' while prompting for a possible
  extension."
  "path: "
  default-website str "")

;;; Autoinsert
(with-eval-after-load 'autoinsert (add-to-list 'auto-insert-alist '(("\\.el\\'" . "Emacs Lisp header") 
				   "Short description: " ";;; "
				   (file-name-nondirectory
				    (buffer-file-name))
				   " --- " str
				   " "
				   "-*- lexical-binding: t; -*-"
				   '(setq lexical-binding t)
				   "

;; Copyright (C) "
				   (format-time-string "%Y")
				   "  "
				   (getenv "ORGANIZATION")
				   |
				   (progn user-full-name)
				   "

;; Author: "
				   (user-full-name)
				   '(if
					(search-backward "&"
							 (line-beginning-position)
							 t)
					(replace-match
					 (capitalize
					  (user-login-name))
					 t t))
				   '(end-of-line 1)
				   " <"
				   (progn user-mail-address)
				   ">
;; Keywords: "
				   '(require 'finder)
				   '(setq v1
					  (mapcar
					   (lambda
					     (x)
					     (list
					      (symbol-name
					       (car x))))
					   finder-known-keywords)
					  v2
					  (mapconcat
					   (lambda
					     (x)
					     (format "%12s:  %s"
						     (car x)
						     (cdr x)))
					   finder-known-keywords "
"))
				   ((let
					((minibuffer-help-form v2))
				      (completing-read "Keyword, C-h: " v1 nil t))
				    str ", ")
				   & -2 "

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

;; " _ "

;;; Code:



(provide '"
				   (file-name-base
				    (buffer-file-name))
				   ")
;;; "
				   (file-name-nondirectory
				    (buffer-file-name))
				   " ends here
")))

(provide 'skel-cfg)
;;; skel-cfg.el ends here
