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

(add-to-list 'exec-path  "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;;; UI
(require 'theme-cfg)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Org
(require 'org-cfg)
(require 'lob-cfg)

;;; Prog
(require 'prog-cfg)
(require 'rust-cfg)
(require 'elisp-cfg)
(require 'python-cfg)

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

(provide 'jekyll-config)
;;; jekyll-custom.el ends here
