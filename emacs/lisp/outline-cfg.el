;;; outline-cfg.el --- Outline mode config -*- lexical-binding: t; -*-

;; Copyright (C) 2023  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: outlines

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

;; Outlines are a very important mode in our setup and we try not to
;; make many modifications to the defaults. It's best to define
;; customizations with a derived-mode (like org-mode).

;;; Code:
(require 'default 'rw/fu)

(defun outline-hook (rx)
  "Enable `outline-minor-mode' and set `outline-regexp'."
  (setq-local outline-regexp rx)
  (outline-minor-mode t))

(defun add-outline-hook (mode rx)
    (let ((sym (symb mode "-hook")))
      (add-hook sym (lambda () (outline-hook rx)))))

(defmacro outline-hooks (&rest pairs)
  `(mapc (lambda (x) (add-outline-hook (car x) (cadr x))) ',pairs))

(outline-hooks (asm-mode ";;;+")
	       (nasm-mode ";;;+")	       
	       (rust-mode "\\(//!\\|////+\\)")
	       (sh-mode "###+")
	       (sh-script-mode "###+")
	       (makefile-mode "###+"))

(provide 'outline-cfg)
;;; outline-cfg.el ends here
