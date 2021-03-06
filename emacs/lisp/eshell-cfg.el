;;; eshell-cfg.el --- Eshell Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: terminals

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
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'Z))


(setq eshell-highlight-prompt nil)

(provide 'eshell-cfg)
;;; eshell-cfg.el ends here
