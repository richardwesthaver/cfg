;;; vc-cfg.el --- VC Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: Richard Westhaver <ellis@rwest.io>
;; Keywords: vc

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
(require 'default)
(require 'mercurial)
(require 'mq)
(defcustom default-vc-backend "hg"
  "the default vc-backend to use for version-control commands."
  :type '(string)
  :group 'default)

;;; Keys
(define-prefix-command 'vc-keys nil "Version Control")
(keymap-set vc-keys "n" #'vc-next-action)
(keymap-set vc-keys "d" #'vc-dir)
(keymap-set vc-keys "r" #'vc-register)
(keymap-set vc-keys "F" #'vc-pull)
(keymap-set vc-keys "P" #'vc-push )

(with-eval-after-load 'default
  (keymap-set keys-map "C-c v" #'vc-keys))

;; (setq vc-hg-global-switches "--noninteractive")

;;; mercurial.el settings

;; use rhg, fallback to hg. see hgrc
(if (file-exists-p "~/.local/bin/rhg")
    (setq hg-binary "~/.local/bin/rhg"))

(provide 'vc-cfg)
;;; vc-cfg.el ends here
