;;; init.el --- Global Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: Richard Westhaver <ellis@rwest.io>
;; Keywords: convenience

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
(defvar lisp-dir "~/.emacs.d/lisp")
(defvar site-lisp-dir "~/.emacs.d/site-lisp")
(add-to-list 'load-path lisp-dir site-lisp-dir)

(defvar system-names '("zor" "jekyll" "hyde" "boris")
  "Hostnames of systems available.")

(defmacro when-sys= (name body)
  "(when (string= (system-name) NAME) BODY)"
  `(when ,(string= (system-name) name) ,body))

(require 'default)
(add-hook 'after-init-hook #'default-setup)

(when-sys= "zor" (require 'zor-config))
(when-sys= "jekyll" (require 'jekyll-config))
(when-sys= "hyde" (require 'hyde-config))
(when-sys= "boris" (require 'boris-config))
(with-eval-after-load 'rw-read (reset-reader))
;;; init.el ends here
