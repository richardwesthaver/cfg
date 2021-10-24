;;; hyde.el --- Hyde Emacs Development Environment   -*- lexical-binding: t; -*-
;; 
;; Copyright (C) 2021  ellis
;; 
;; Author: ellis <ellis@rwest.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0.50"))
;; Keywords: languages, mail, multimedia, news, terminals, tools, wp
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;; Commentary:
;; 
;; This package contains a complete and configurable Emacs ecosystem
;; suitable for your daily driver. It depends on many external
;; packages from MELPA as well as some system programs (ripgrep,
;; mercurial, etc).
;; 
;;; Code:
;;;; Packages
(require 'package-x)
;;;; Themes 
(require 'abyss-theme)
(require 'color-theme-sanityinc-tomorrow)
(setq custom-theme-directory "~/shed/data/emacs/themes")
(defvar light-theme 'leuven)
(defvar dark-theme 'wheatgrass)
(defvar demon-theme 'abyss)
(defvar ps-theme 'sanityinc-tomorrow-blue)
(defvar luv-theme 'sanityinc-tomorrow-bright)
(defvar current-theme luv-theme)
(defun next-theme (theme)
  (disable-theme current-theme)
  (load-theme theme t)
  (setq current-theme theme))

;;;###autoload
(defun toggle-theme ()
  (interactive)
  (cond
   ((eq current-theme light-theme) (next-theme dark-theme))
   ((eq current-theme dark-theme) (next-theme demon-theme))
   ((eq current-theme demon-theme) (next-theme luv-theme))
   ((eq current-theme luv-theme) (next-theme light-theme))))

(add-hook 'after-init-hook (lambda () (load-theme current-theme t)))
;;;; Completion 
(require 'marginalia)
(require 'orderless)
(require 'embark)
(marginalia-mode 1)
(setq completion-styles '(orderless))

;; Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

;;;###autoload
(defun embark-act-noquit ()
  "Run action but don't quit the minibuffer afterwards."
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

;;;; Search 
(require 'avy)
(require 'swiper)
(require 'rg)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
(global-set-key (kbd "C-c s l") 'avy-goto-line)
(global-set-key (kbd "C-c s a") 'avy-goto-word-1)
(global-set-key (kbd "C-c s e") 'avy-goto-word-0)
(global-set-key (kbd "\C-s") 'swiper)

;;;; Mail 
(require 'notmuch)
(require 'ol-notmuch)

(defgroup hyde-mail ()
  "Hyde email system.
requires: notmuch, offlineimap
env: USER_EMAIL"
  :group 'hyde)

(defcustom hyde-mail-dir ()
  "Root path for mail-related data"
  :group 'hyde-mail)

(defcustom hyde-mailbox-alist ()
  "list of initialized mailboxes to query"
  :group 'hyde-mail)

;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)

(setq user-mail-address "ellis@rwest.io"
      user-full-name "ellis")

;; smtp config
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it
      smtpmail-debug-info t
      message-default-mail-headers "Cc: \nBcc: \n"
      message-auto-save-directory "~/shed/data/mail/draft"
      message-directory "~/shed/data/mail/sent"
      message-kill-buffer-on-exit t)

;;;###autoload
(defun notmuch-exec-offlineimap ()
  "execute offlineimap command and tag new mail with notmuch"
  (interactive)
  (start-process-shell-command "offlineimap"
                               "*offlineimap*"
                               "offlineimap -o")
  (notmuch-refresh-all-buffers))

;;;; News 
(require 'elfeed)

(defvar elfeed-feeds-alist
  '(("http://threesixty360.wordpress.com/feed/" blog math)
    ("http://www.50ply.com/atom.xml" blog dev)
    ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
    ("http://abstrusegoose.com/feed.xml" comic)
    ("http://accidental-art.tumblr.com/rss" image math)
    ("http://english.bouletcorp.com/feed/" comic)
    ("http://curiousprogrammer.wordpress.com/feed/" blog dev)
    ("http://feeds.feedburner.com/amazingsuperpowers" comic)
    ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
    ("http://pages.cs.wisc.edu/~psilord/blog/rssfeed.rss" blog)
    ("http://www.anticscomic.com/?feed=rss2" comic)
    ("http://feeds.feedburner.com/blogspot/TPQSS" blog dev)))

(setq elfeed-db-directory "~/shed/data/emacs/elfeed")

;;;; Music 
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/shed/stash/music")
;;;; Programming 
(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-ui-doc)
(require 'lsp-ui-util)
(require 'lsp-ui-sideline)
(require 'lsp-headerline)

(require 'ron-mode)
(require 'toml-mode)
(require 'yaml-mode)

(require 'dyalog-mode)
(require 'bqn-mode)
(require 'k-mode)

(defgroup hd-prog ()
  "hyde programming extensions"
  :group 'hyde)

(defcustom hd-prog-modes (list (prog-mode))
  "the list of programming modes we care about"
  :group 'hd-prog)

(defcustom hd-prog-auto-mode-alist
  '(("\\.toml\\'" . toml-mode)
   ("\\.ron\\'" . ron-mode)
   ("\\.yaml\\.yml\\'" . yaml-mode)
   ("\\.apl\\'" . dyalog-mode)
   ("\\.bqn$" . bqn-mode)
   ("\\.rs$" . rustic-mode))
  "list of (REGEX . MODE) to append to auto-mode-alist"
  :group 'hd-prog)

;;;###autoload
(defun hd-prog-setup ()
  "Initialize settings and packages for hd-prog modes"
  ;; betta bites
  (setq hexl-bits 16)
  ;; auto-indent newlines
  (electric-indent-mode)
  ;; lsp config
  (setq lsp-ui-doc-position "bottom"
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-doc-show-with-mouse t)

  (setq python-indent-offset 2
	ron-indent-offset 2)
  ;; auto-mode-alist setup
  (dolist (m hd-prog-auto-mode-alist)
    (push m auto-mode-alist))

  ;; hooks
  (hook-modes hd-prog-auto-mode-alist)

  ;; custom
  (hd-elisp-setup))

;;;;; Shells 
(require 'esh-opt)
(require 'vterm)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;;;;; Elisp
(require 'lispy)

;; setup
(defun hd-elisp-setup ()
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'(lambda () (lispy-mode 1)))))


;;;###autoload
(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

;;;;; Rust 
(require 'rustic)
(require 'lsp-rust)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
(setq rustic-indent-offset 2
      rustic-babel-format-src-block t
      rustic-babel-display-compilation-buffer t)

;;;; Demos 
(require 'frameshot)
(require 'keycast)

;;;; pkg 
(provide 'hyde)
;;; hyde.el ends here
