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
(progn
  (push '(org-directory . "~/shed/stash/org") user-settings)
  (default-setup)
  (hd-prog-setup))
