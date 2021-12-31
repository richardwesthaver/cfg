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
;;;; Themes 
(defvar current-theme 'modus-operandi "the current theme")

(defun next-theme (theme)
  (load-theme theme)
  (setq current-theme theme))

;;;###autoload
(defun toggle-theme ()
  (interactive)
  (cond ((eq current-theme 'modus-operandi) (next-theme 'modus-vivendi))
	((eq current-theme 'modus-vivendi) (next-theme 'modus-operandi))))

(add-hook 'after-init-hook (lambda () (load-theme current-theme)))

;;;; Async 
(require 'async)
(require 'ob-async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
;;;; Completion 
(require 'marginalia)
(require 'orderless)
(require 'embark)
(require 'corfu)
(corfu-global-mode 1)
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

(define-key keys-map (kbd "C-c e m") #'notmuch)
;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)

(setq user-mail-address "ellis@rwest.io"
      user-full-name "ellis")

;; smtp config
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it
      smtpmail-debug-info t
      message-default-mail-headers "Cc: \nBcc: \n"
      message-kill-buffer-on-exit t)

;;;###autoload
(defun notmuch-exec-offlineimap ()
  "execute offlineimap command and tag new mail with notmuch"
  (interactive)
  (start-process-shell-command "offlineimap"
                               "*offlineimap*"
                               "offlineimap -o")
  (notmuch-refresh-all-buffers))

;;;; Org 
(require 'org-web-tools)
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

;;;; EMMS
(require 'emms-setup)
(require 'emms-mark)
(require 'emms-player-mpd)

(emms-all)
(emms-default-players)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq emms-source-file-default-directory "~/shed/stash/music/lib")
;; (setq emms-playlist-default-major-mode 'emms-mark-mode)

(defun track-title-from-file-name (file)
  "For using with EMMS description functions. Extracts the track
title from the file name FILE, which just means a) taking only
the file component at the end of the path, and b) removing any
file extension."
  (with-temp-buffer
    (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
    (ignore-error 'search-failed
      (search-forward-regexp (rx "." (+ alnum) eol))
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun hd-emms-track-description (track)
  "Return a description of TRACK, for EMMS, but try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and artist title)
           (concat artist " - " title))
          (title title)
          ((eq (emms-track-type track) 'file)
           (track-title-from-file-name (emms-track-name track)))
          (t (emms-track-simple-description track)))))

(setq emms-track-description-function #'hd-emms-track-description)

;;;; Programming 
(require 'lsp-mode)
(require 'lsp-lens)
(require 'lsp-headerline)
(require 'lsp-completion)
(require 'lsp-diagnostics)
(require 'lsp-protocol)
(require 'lsp-modeline)
(require 'lsp-iedit)
(require 'lsp-semantic-tokens)
(require 'lsp-ido)
(require 'lsp-dired)
(require 'lsp-icons)
(require 'lsp-ui)
(require 'lsp-treemacs)
(require 'company)
(require 'ron-mode)
(require 'toml-mode)
(require 'yaml-mode)
(require 'meson-mode)

(require 'dyalog-mode)
(require 'bqn-mode)
(require 'k-mode)
(require 'jq-mode)
(require 'ob-jq)

(require 'csound-mode)
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
   ("\\.k$" . k-mode)
   ("\\.rs$" . rustic-mode))
  "list of (REGEX . MODE) to append to auto-mode-alist"
  :group 'hd-prog)

;;;###autoload
(defun hd-prog-setup ()
  "Initialize settings and packages for hd-prog modes"
  (setq hexl-bits 8)
  ;; auto-indent newlines
  (electric-indent-mode)
  ;; lsp config
  (setq lsp-ui-doc-position "bottom"
	lsp-ui-doc-show-with-cursor nil
	lsp-ui-doc-show-with-mouse t
	lsp-keymap-prefix "C-c `")
  (setq bqn-mode-map-prefix "C-M-")
  (setq python-indent-offset 2
	python-guess-indent nil
	ron-indent-offset 2)

  ;; populate org-babel
  (org-babel-do-load-languages
   ;; TODO 2021-10-24: bqn, apl, k
   'org-babel-load-languages '((rust . t)
			       (shell . t)
			       (emacs-lisp . t)
			       (eshell . t)
			       (sed . t)
			       (awk . t)
			       (jq . t)
			       (dot . t)
			       (js . t)
			       (C . t)
			       (python . t)
			       (lua . t)
			       (lilypond . t)))
  ;; (add-hook 'c-mode-hook 'lsp-mode)
  ;; (add-hook 'c++-mode-hook 'lsp-mode)
  ;; auto-mode-alist setup
  (dolist (m hd-prog-auto-mode-alist)
    (push m auto-mode-alist))

  ;; hooks
  (hook-modes hd-prog-auto-mode-alist)
  ;; custom
  (hd-elisp-setup))

;;;;; Shells 
(require 'esh-opt)
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;;;;; Elisp
(require 'lispy)

;;;###autoload
(defun hd-elisp-setup ()
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook lisp-interaction-mode-hook))
    (add-hook hook #'(lambda () (lispy-mode 1)))))


;;;;; Rust 
(require 'rustic)
(setq rustic-indent-offset 2
      rustic-babel-format-src-block t
      rustic-babel-display-compilation-buffer t)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(setq lsp-rust-server 'rust-analyzer)
;; (remove-hook 'rustic-mode-hook 'flycheck-mode)

;;;; Demos 
(require 'frameshot)
(require 'keycast)

;;;; provide
(provide 'hyde)
;;; hyde.el ends here
