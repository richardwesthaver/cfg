;;; rw/err.el --- RW Error Handling -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver
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

;; Generic error-handling framework for Emacs Lisp.

;;; Code:
(defgroup err nil
  "Customization group for RW `err'."
  :tag "err"
  :group 'rw)

(defvar last-err nil
  "The last error signaled via `err'.")

(defun err (msg)
  "Return a generic error form with the given MSG."
  (cons 'err msg))

(defun err2 (expected found)
  (err (format "Found '%s' -> Expected '%s'" found expected)))

(provide 'rw-err)
;;; err.el ends here
