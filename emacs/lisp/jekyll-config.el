;;; jekyll-custom.el --- Jekyll Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: Richard Westhaver <ellis@rwest.io>
;; Keywords: convenience, internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))

(require 'default)
(add-packages '(async))

(defun set-exec-path-from-shell ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(add-to-list 'exec-path  "/usr/local/bin/")
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(set-exec-path-from-shell)
;;; UI
(require 'theme-cfg)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;; Org
(require 'org-cfg)
(with-eval-after-load 'org
  (setq org-agenda-files `(,(expand-file-name org-directory) "/Volumes/stash/org/")))
(require 'lob-cfg)
;;; Prog
(require 'prog-cfg)
(require 'rust-cfg)
(require 'bqn-cfg)
(require 'elisp-cfg)
(require 'python-cfg)
(require 'nim-cfg)
;;; Term
(require 'shell-cfg)
(require 'eshell-cfg)
;;; Tools
(require 'search-cfg)
(require 'completion-cfg)
(require 'scratch-cfg)
(require 'register-cfg)
(require 'macro-cfg)
(require 'skel-cfg)
(require 'vc-cfg)
;;; Apps
(require 'emms-cfg)
(require 'elfeed-cfg)
(require 'mail-cfg)
(require 'sclang-cfg)

(provide 'jekyll-config)
;;; jekyll-custom.el ends here
