;;; lisp-cfg
;; HACK 2023-02-08: the great movement refactor
;; NOTE 2023-02-08: check out lisp-mnt.el for ideas
;; (add-to-list 'package-selected-packages 'lispy)
(add-to-list 'package-selected-packages 'slime)

(add-to-list 'interpreter-mode-alist '("scsh" . scheme-mode))

(set-default 'auto-mode-alist
             (append '(("\\.scm$" . scheme-mode)
		       ("\\.scsh$" . scheme-mode)
                       ("\\.ss$" . scheme-mode))
                     auto-mode-alist))

(dolist (hook '(emacs-lisp-mode-hook
		ielm-mode-hook
		lisp-interaction-mode-hook
		lisp-data-mode-hook
		eval-expression-minibuffer-setup-hook))
  (add-hook hook #'(lambda () (lispy-mode 1))))

(setf slime-lisp-implementations
      `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
        (roswell ("ros" "-Q" "run"))))
(setf slime-default-lisp 'sbcl)

(defun common-lisp-quickdoc (package)
  "Search for PACKAGE in QuickDocs."
  (interactive "sCommon Lisp package: ")
  (let ((name (common-lisp-hyperspec--strip-cl-package
	       (downcase package))))
    (eww-browse-url (concat "https://quickdocs.org/" name))))

(defun common-lisp-hyperspec-read-symbol-name (&optional symbol-at-point)
  (let* ((symbol-at-point (or symbol-at-point (thing-at-point 'symbol)))
	 (stripped-symbol (and symbol-at-point
			       (common-lisp-hyperspec--strip-cl-package
				(downcase symbol-at-point)))))
    (cond ((and stripped-symbol
		(common-lisp-hyperspec--find stripped-symbol))
	   stripped-symbol)
	  (t
	   (completing-read "Look up symbol in Common Lisp HyperSpec: "
			    common-lisp-hyperspec--symbols nil t
			    stripped-symbol
			    'common-lisp-hyperspec-history)))))

(defun common-lisp-hyperspec (symbol-name)
  "View the documentation on SYMBOL-NAME from the Common Lisp HyperSpec.
If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions.

The Common Lisp HyperSpec is the full ANSI Standard Common Lisp, provided
by Kent Pitman and Xanalys Inc.  By default, the Xanalys Web site is
visited to retrieve the information.  Xanalys Inc. allows you to transfer
the entire Common Lisp HyperSpec to your own site under certain conditions.
Visit http://www.lispworks.com/reference/HyperSpec/ for more information.
If you copy the HyperSpec to another location, customize the variable
`common-lisp-hyperspec-root' to point to that location."
  (interactive (list (common-lisp-hyperspec-read-symbol-name)))
  (let ((name (common-lisp-hyperspec--strip-cl-package
	       (downcase symbol-name))))
    (cl-maplist (lambda (entry)
		  (browse-url (concat common-lisp-hyperspec-root "Body/"
				      (car entry)))
		  (when (cdr entry)
		    ;; ???
		    (sleep-for 1.5)))
		(or (common-lisp-hyperspec--find name)
		    (error "The symbol `%s' is not defined in Common Lisp"
			   symbol-name)))))

(setq inferior-lisp-program "ros -L sbcl -Q -l ~/.sbclrc run")
(setq scheme-program-name "gsi")
(setq guile-program "guile")
(setq cmulisp-program "lisp")
(setq scsh-program "scsh")

(load (expand-file-name "~/.roswell/helper.el") t)

;;; Structural Editing
(repeat-mode 1)
(defvar edit-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,k . ,f)
                   '(("u" . backward-up-list)
                     ("f" . forward-sexp)
                     ("b" . backward-sexp)
                     ("d" . down-list)
                     ("k" . kill-sexp)
                     ("n" . paredit-forward)
                     ("p" . paredit-backward)
                     ("K" . paredit-kill)
                     ("]" . paredit-forward-slurp-sexp)
                     ("[" . paredit-backward-slurp-sexp)
                     ("}" . paredit-forward-barf-sexp)
                     ("{" . paredit-backward-barf-sexp)
                     ("C" . paredit-convolute-sexp)
                     ("J" . paredit-join-sexps)
                     ("S" . paredit-split-sexp)
                     ("R" . paredit-raise-sexp)
                     ("\\" . indent-region)
                     ("/" . undo)
                     ("t" . transpose-sexps)
                     ("x" . eval-defun)))
      (define-key map (kbd k) f))
    map))

(map-keymap
 (lambda (_ cmd)
   (put cmd 'repeat-map 'edit-map))
 edit-map)

;;; Utils

;; TODO 2023-02-08
(defun reduce-setq-at-pt ()
  "Reduce setq statements at point.

In other words, with point represented by '|':
(setq a nil)
(setq |b t)

is reduced to:
(setq a nil
      b t)|"
  (let ((sp (list-at-point))
	pt syms res)
    (backward-list)
    (cond
     ;; inside a setq form
     ((eq 'setq (car sp)) 		
      (let ((prev 			; previous list
	     (save-excursion
	       (condition-case err
		   (backward-list)
		 (err nil)
		 (:success (list-at-point)))))
	    (next 			; next list
	     (save-excursion
	       (condition-case err
		   (forward-list)
		 (err nil)
		 (:success (list-at-point))))))
	;; reduce previous and next setq forms, appending result to syms
	(cl-reduce (lambda (a b)
		     (cond
		      ((and (null a) (null b)) nil)
		      ((and (eq 'setq (car a)) (null b)) (cdr a))
		      ((and (null a) (eq 'setq (car b))) (cdr b))
		      ((cl-reduce #'eq `(setq ,(car a) ,(car b)))
		       (append syms (cdr a) (cdr b)))))
		   `(,prev ,next)))))))

(defun slime-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))

(provide 'lisp-cfg)
