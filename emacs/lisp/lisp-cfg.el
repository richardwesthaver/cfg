(add-to-list 'package-selected-packages 'lispy)
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

;; (add-hook 'slime-load-hook (lambda () (require 'slime-scheme)))

(setq inferior-lisp-program "sbcl")

(setq scheme-program-name "gsi")
(setq guile-program "guile")
(setq cmulisp-program "lisp")
(setq scsh-program "scsh")


(provide 'lisp-cfg)
