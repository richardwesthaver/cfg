;;; init.el --- Emacs init -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.config/emacs/lisp/")
(let ((default-directory  "~/shed/src/contrib/el/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;; install
(require 'default (concat user-emacs-directory "lisp/default.el"))
(require 'hyde (concat user-emacs-directory "lisp/hyde.el"))
(require 'babel (concat (file-name-as-directory (getenv "SHED")) "src/babel/babel.el"))
(require 'shed (concat (file-name-as-directory (getenv "SHED")) "src/shed/lisp/shed.el"))
;;;; config
(setq org-directory "~/shed/org")

(default-setup)

(hd-prog-setup)
