;;; rw-cfg.el --- base config -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver

;; Author: Richard Westhaver <rwestha2@gdeb.com>
;; Keywords: convenience, local

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

;; This file is responsible for defining symbols which are required
;; throughout our custom packages. This eliminates a category of bugs
;; which emerged when I started refactoring components of 'config.org'
;; to 'lisp/' (dependency on my init.el).

;;; Code:
(require 'rw-fu "fu")
(defcustom user-dev-directory (expand-file-name "~/dev")
  "Directory containing dev projects and files."
  :type 'directory
  :safe 'file-directory-p
  :group 'environment)

(defcustom user-lisp-directory (expand-file-name "lisp" user-emacs-directory)
  "Directory containing custom lisp code."
  :type 'directory
  :safe 'file-directory-p
  :group 'environment)

(defcustom user-site-lisp-directory (expand-file-name "site-lisp" user-emacs-directory)
  "Directory containing 3rd-party lisp code. Anything in this
      directory MUST be audited in its entirety. DO NOT copy/paste from
      the internet without fully understanding the code. Whenever
      possible, do it yourself."
  :type 'directory
  :safe 'file-directory-p
  :group 'environment)

(add-to-load-path user-site-lisp-directory)
(let ((default-directory user-site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory user-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))
(define-key special-event-map [sigusr1] 'emacs-restart)           

(provide 'rw-cfg)
;;; rw-cfg.el ends here
