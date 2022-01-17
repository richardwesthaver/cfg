;;; theme-cfg.el --- Theme configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Richard Westhaver

;; Author: Richard Westhaver <ellis@jekyll>
;; Keywords: faces

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
;;;; Themes 
(defvar current-theme 'modus-operandi "the current theme")

(defun next-theme (theme)
  (load-theme theme)
  (setq current-theme theme))

;;;###autoload
(defun toggle-theme ()
  (interactive)
  (cond ((eq current-theme 'modus-operandi) (next-theme 'modus-vivendi))
	((eq current-theme 'modus-vivendi) (next-theme 'modus-operandi))))

(add-hook 'after-init-hook (lambda () (load-theme current-theme)))

(provide 'theme-cfg)
;;; theme-cfg.el ends here
