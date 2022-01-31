	;;; games-cfg.el --- Games Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: games

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

(with-eval-after-load 'tetris
  (define-key tetris-mode-map (kbd "s") 'tetris-move-down)
  (define-key tetris-mode-map (kbd "a") 'tetris-move-left)
  (define-key tetris-mode-map (kbd "d") 'tetris-move-right)
  (define-key tetris-mode-map (kbd "w") 'tetris-movee-bottom)
  (substitute-key-definition 'tetris-end-game 'tetris-pause-game tetris-mode-map))

(provide 'games-cfg)
;;; games-cfg.el ends here
