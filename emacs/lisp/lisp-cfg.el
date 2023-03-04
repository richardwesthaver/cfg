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
(setf slime-default-lisp 'roswell)

(defun common-lisp-quickdoc (package)
  "Search for PACKAGE in QuickDocs."
  (interactive "sCommon Lisp package: ")
  (let ((name (common-lisp-hyperspec--strip-cl-package
	       (downcase package))))
    (eww-browse-url (concat "https://quickdocs.org/" name))))

(setq inferior-lisp-program "ros -L sbcl -Q -l ~/.sbclrc run")
(setq scheme-program-name "gsi")
(setq guile-program "guile")
(setq cmulisp-program "lisp")
(setq scsh-program "scsh")

(load (expand-file-name "~/.roswell/helper.el"))

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


(provide 'lisp-cfg)
