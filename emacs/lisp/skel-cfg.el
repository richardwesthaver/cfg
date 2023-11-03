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
(with-eval-after-load 'autoinsert 
  (add-to-list
   'auto-insert-alist
   '(("\\.el\\'" . "Emacs Lisp header") 
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
"))
  (add-to-list 
   'auto-insert-alist 
   '("\\.dir-locals[-2]*\.el" nil ";;; Directory Local Variables
" ";;; For more information see (info \"(emacs) Directory Variables\")

" "(("
'(setq v1
       (let
	   (modes)
	 (mapatoms
	  (lambda
	    (mode)
	    (let
		((name
		  (symbol-name mode)))
	      (when
		  (string-match "-mode$" name)
		(push name modes)))))
	 (sort modes 'string<)))
(completing-read "Local variables for mode: " v1 nil t)
" . (("
(let
    ((all-variables
      (apropos-internal ".*"
			(lambda
			  (symbol)
			  (and
			   (boundp symbol)
			   (get symbol 'variable-documentation))))))
  (completing-read "Variable to set: " all-variables))
" . "
(completing-read "Value to set it to: " nil)
"))))
")))

(defcustom fu-magic nil
  "Non-nil means template skeletons will be inserted automagically using abbrevs."
  :type 'boolean
  :group 'babel
  :safe 'booleanp)

(defvar fu-alist '()
  "Internal list of available skeletons.")

(define-abbrev-table 'fu-abbrev-table ()
  "Abbrev table for Babel skeletons"
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*")

(defmacro fu-define-skeleton (name doc &rest skel)
  "Define a fu skeleton using NAME DOC and SKEL.
The skeleton will be bound to fu-NAME and added to
`fu-abbrev-table`"
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "fu-" name))))
    `(progn
       (define-abbrev fu-abbrev-table
         ,name "" ',function-name :system t)
       (setq fu-alist
             (cons ',function-name fu-alist))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert %s statement." name))
         ,@skel))))

(defmacro fu-define-aux-skeleton (name &optional doc &rest skel)
  "Define a fu auxiliary skeleton using NAME DOC and SKEL.
The skeleton will be bound to fu-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "fu--" name)))
         (msg (funcall (if (fboundp 'format-message) #'format-message #'format)
                       "Add `%s' clause? " name)))
    (when (not skel)
      (setq skel
            `(< ,(format "%s:" name) \n \n
                > _ \n)))
    `(define-skeleton ,function-name
       ,(or doc
            (format "Auxiliary skeleton for %s statement." name))
       nil
       (unless (y-or-n-p ,msg)
         (signal 'quit t))
       ,@skel)))

(fu-define-aux-skeleton else)
(fu-define-aux-skeleton except)
(fu-define-aux-skeleton then)
(fu-define-aux-skeleton while)

(fu-define-skeleton makefile "insert a makefile"
  "name: " "# makefile --- " str \n ".PHONY: c" \n \n "c:;rm -rf o")

(fu-define-skeleton local-variables
    "Insert a local variables section.  Use current comment syntax if any."  
  (completing-read "mode: " obarray                                        
                   (lambda (symbol)
		     (if (commandp symbol)
			 (string-match
			  "-mode$" (symbol-name symbol)))) t)                                                        
  '(save-excursion                                                         
     (if (re-search-forward page-delimiter nil t)                          
	 (error "Not on last page")))                                        
  comment-start comment-start " local-vars:" comment-end \n                          
  comment-start comment-start " - mode: " str                                               
  & -5 | '(kill-line 0) & -1 | comment-end \n                              
  ( (completing-read (format "var, %s: " skeleton-subprompt)          
                     obarray                                                 
                     (lambda (symbol)                                        
                       (or (eq symbol 'eval)                                 
                           (custom-variable-p symbol)))                      
                     t)                                                      
    comment-start comment-start " - " str ": "                                                 
    (read-from-minibuffer "expr: " nil read-expression-map nil       
			  'read-expression-history) | _                      
    comment-end \n)                                                        
  resume:                                                                  
  \n)                                     

(fu-define-skeleton rust-fn
    "Insert a Rust function."
  nil > "fn " > _ "() {" \n \n "}")

(define-abbrev-table 'fu-abbrev-table ()
  "Abbrev table for fu."
  :parents (list fu-abbrev-table))

;;;###autoload
(define-derived-mode fu-keys keys "fu-keys"
  "fu keybinds")

;;;###autoload
(define-minor-mode fu-mode "fu-mode") 

(provide 'skel-cfg)
;;; skel-cfg.el ends here
