;;; zor-custom.el --- Zor init                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: Richard Westhaver <ellis@Richards-MBP.frontier.com>
;; Keywords: extensions, tools

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

;; Archliinux BTW

;;; Code:
(require 'hyde)
(require 'shed)
(require 'babel)

(defvar lisp-data "~/.emacs.d/lisp")

(add-to-list 'load-path lisp-data)

(let ((default-directory (concat  lisp-data "/elpa/")))
  (normal-top-level-add-subdirs-to-load-path))

(push '(org-directory . "~/s/org") user-settings)
(setq package-native-compile t)

(hd-prog-setup)

(provide 'zor-custom)
;; zor-custom.el ends here
