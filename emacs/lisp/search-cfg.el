;;; search-cfg.el --- Search Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: matching

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
(add-packages '(avy swiper rg))

(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)
(global-set-key (kbd "C-c s l") 'avy-goto-line)
(global-set-key (kbd "C-c s a") 'avy-goto-word-1)
(global-set-key (kbd "C-c s e") 'avy-goto-word-0)
(global-set-key (kbd "\C-s") 'swiper)

(provide 'search-cfg)
;;; search-cfg.el ends here
