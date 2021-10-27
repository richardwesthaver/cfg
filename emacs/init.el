;;; init.el --- Emacs init -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.config/emacs/lisp/")
(let ((default-directory  "~/shed/src/contrib/el/"))
  (normal-top-level-add-subdirs-to-load-path))
;;;; default configuration
(require 'default)
(default-setup)
(setq org-directory "~/shed/org")
;;;; hyde config
(require 'hyde)
(hd-prog-setup)
