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
(add-to-list 'load-path lisp-dir)

(require 'default)
(add-hook 'after-init-hook #'default-setup)

(cond
 ((string= (system-name) "zor") (require 'zor-config))
 ((string= (system-name) "jekyll") (require 'jekyll-config))
 ((string= (system-name) "hyde") (require 'hyde-config)))

(provide 'init)
;;; init.el ends here
