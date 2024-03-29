;;; zor-config.el --- Zor Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023  anticorp

;; Author: Richard Westhaver <ellis@rwest.io>
;; Keywords: extensions, tools

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

;; Archlinux BTW

;;; Code:

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))

; (setq package-selected-packages '(csound-mode ob-jq bqn-mode dyalog-mode k-mode))

(require 'default)

(add-to-list 'load-path (expand-file-name "lisp/rw" user-emacs-directory))
(require 'rw-fu "fu")
; (require 'rw "rw")

;;; UI
(require 'theme-cfg)
(set-face-attribute 'default nil :font "CommitMono")
;;; Org
(require 'org-cfg)
(require 'lob-cfg)

;;; Prog
(require 'outline-cfg)
(require 'prog-cfg)
(require 'rust-cfg)
(require 'python-cfg)
(require 'nim-cfg)

(require 'lisp-cfg)

;;; Tools
(require 'which-key)
(require 'repeat-cfg)
(require 'dired-cfg)
(require 'completion-cfg)
(require 'fmt-cfg)
(require 'search-cfg)

;;; Misc
(require 'elfeed-cfg)
(require 'erc-cfg)
(require 'zig-cfg)
;; (require 'sclang-cfg)
(require 'register-cfg)
(require 'macro-cfg)
(require 'mail-cfg)
(require 'shell-cfg)
(require 'skel-cfg)
(require 'scratch-cfg)
(require 'vc-cfg)
;; (require 'emms-cfg)
(require 'shell-cfg)
(require 'eshell-cfg)
(require 'term-cfg)
; (setq emms-source-file-default-directory "/mnt/x/audio/music")

(provide 'zor-config)
;; zor-custom.el ends here
