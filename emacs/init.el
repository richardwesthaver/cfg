;;; init.el --- Emacs init -*- lexical-binding: t -*-
(defvar lisp-data "~/.emacs.d/lisp")
(add-to-list 'load-path lisp-data)
(let ((default-directory (concat  lisp-data "/elpa/")))
  (normal-top-level-add-subdirs-to-load-path))
;;;; install
(require 'default)
(require 'hyde)
(require 'shed)
(require 'babel)
;;;; config
(push '(org-directory . "~/org") user-settings)
(setq package-native-compile t)
(default-setup)
(hd-prog-setup)
