;;; hyde-config.el --- Hyde Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Richard Westhaver

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

;; 

;;; Code:
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))
(setq package-selected-packages nil)
(require 'default)
(require 'theme-cfg)
(add-to-list 'load-path (expand-file-name "lisp/rw" user-emacs-directory))
(require 'rw "rw")
(require 'org-cfg)
(require 'lob-cfg)
(require 'prog-cfg)
(require 'rust-cfg)
(require 'python-cfg)
(require 'lisp-cfg)
(require 'dired-cfg)
(require 'completion-cfg)
(require 'fmt-cfg)
(require 'search-cfg)
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

(provide 'hyde-config)
;;; hyde-config.el ends here
