;;; early-init.el --- pre-init configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021  ellis

;; Author: ellis
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; 
;; Commentary:
;; 
;; A few snippets are added here that need to be loaded ASAP on
;; startup, such as UI elements that would otherwise be loaded for a
;; second and then disappear when the `init.el` file is loaded.
;;
;; If any of the below settings are in your `init.el`, I suggest
;; migrating them to `early-init.el`.
;; 
;;; Code:
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-echo-area-message "ellis"
      inhibit-startup-screen t
      inhibit-startup-buffer-menu nil
      inhibit-splash-screen t
      use-dialog-box t
      use-file-dialog nil)

;; enable native-compilation on supported builds
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (setq package-native-compile t)))

;;; early-init.el ends here
