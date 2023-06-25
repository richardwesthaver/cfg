;;; boris-config.el --- Boris Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver

;; Author: Richard Westhaver <ellis@jekyll>
;; Keywords: hardware

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

;; This configuration is specific to boris, which is a PinePhonePro
;; running Linux.

;;; Code:
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))

(require 'default)

;;; UI
(require 'theme-cfg)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;;; Org
(require 'org-cfg)
(require 'lob-cfg)
;;; Prog
(require 'prog-cfg)
(require 'rust-cfg)
(require 'elisp-cfg)
(require 'python-cfg)
(require 'nim-cfg)
(require 'lisp-cfg)
;;; Tools
(require 'dired-cfg)
(require 'completion-cfg)
(require 'fmt-cfg)
(require 'search-cfg)
(dired-async-mode 1)
;;; Misc
(require 'elfeed-cfg)
;; (require 'sclang-cfg)
(require 'register-cfg)
(require 'macro-cfg)
(require 'mail-cfg)
(require 'shell-cfg)
(require 'skel-cfg)
(require 'scratch-cfg)
(require 'vc-cfg)
(require 'emms-cfg)
(require 'shell-cfg)
(require 'eshell-cfg)

(provide 'boris-config)
;;; boris-config.el ends here
