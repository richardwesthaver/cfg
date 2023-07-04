;;; test.el --- ERT extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver

;; Author: Richard Westhaver <rwestha2@gdeb.com>
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

;; Some extensions for `ert'.

;;; Code:
(require 'rw-macs "macs")
(using ert)
(defmacro deftest (name &rest body)
  "shorthand for `ert-deftest'."
  (declare (indent 1))
  `(ert-deftest ,name () ,@body))
(defmacro should= (a b &optional test)
  "generalized equality tests for `should'. TEST defaults to `equal'"
  `(should (,(or test 'equal) ,a ,b)))
(defmacro shouldp (a &optional p)
  "generalize predicates for `should' -- if P is nil, return A."
  `(should (if ,p (,p a) a)))
(provide 'rw-test)
;;; test.el ends here
