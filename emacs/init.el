;;; init.el --- Emacs init -*- lexical-binding: t -*-
(defvar lisp-data "~/shed/data/emacs/lisp/")
(add-to-list 'load-path lisp-data)
(let ((default-directory (concat  lisp-data "contrib/")))
  (normal-top-level-add-subdirs-to-load-path))
;;;; install
(require 'default)
(require 'hyde)
(require 'shed)
(require 'babel)
(setq package-user-dir (expand-file-name "~/shed/data/emacs/elpa"))
;;;; config
(default-setup)
(setq org-directory "~/shed/stash/org")
(hd-prog-setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(default hyde shed babel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
