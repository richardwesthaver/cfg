;;; rw-log.el --- RW Logging and Debugging -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver

;; Author: Richard Westhaver <rwestha2@gdeb.com>
;; Keywords: tools, maint, convenience

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

;; `rw/log' provides a framework for logging and debugging in Emacs
;; Lisp.

;; - [log! dbg! trace!] macros
;; - [backend] - implement a log-stream class for 'message', 'sqlite',
;;               'list', 'hashmap', etc

;;; Code:

(provide 'rw-log)
;;; rw-log.el ends here
