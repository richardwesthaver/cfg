;;; sclang-cfg.el --- SuperCollider Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: multimedia, languages

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
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'load-path
		   (format "/Users/%s/Library/Application Support/SuperCollider/downloaded-quarks/scel/el" (user-login-name)))
      (setq exec-path
	    (append exec-path
		    '("/Applications/SuperCollider.app/Contents/MacOS/")))))

(require 'sclang)

(provide 'sclang-cfg)
;;; sclang-cfg.el ends here
