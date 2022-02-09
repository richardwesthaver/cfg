;;; artist-cfg.el --- Artist-mode Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: outlines, tools

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

;; integrate ido with artist-mode
(defun artist-ido-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive (list (ido-completing-read "Drawing operation: " 
                                          (list "Pen" "Pen Line" "line" "straight line" "rectangle" 
                                                "square" "poly-line" "straight poly-line" "ellipse" 
                                                "circle" "text see-thru" "text-overwrite" "spray-can" 
                                                "erase char" "erase rectangle" "vaporize line" "vaporize lines" 
                                                "cut rectangle" "cut square" "copy rectangle" "copy square" 
                                                "paste" "flood-fill"))))
  (artist-select-operation type))

   (defun artist-ido-select-settings (type)
     "Use ido to select a setting to change in artist-mode"
     (interactive (list (ido-completing-read "Setting: " 
                                             (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars" 
                                                   "Rubber-banding" "Trimming" "Borders"))))
     (if (equal type "Spray-size") 
       (artist-select-operation "spray set size")
       (call-interactively (artist-fc-get-fn-from-symbol 
			    (cdr (assoc type '(("Set Fill" . set-fill)
					       ("Set Line" . set-line)
					       ("Set Erase" . set-erase)
					       ("Rubber-banding" . rubber-band)
					       ("Trimming" . trimming)
					       ("Borders" . borders)
					       ("Spray-chars" . spray-chars))))))))

   (add-hook 'artist-mode-init-hook 
	     (lambda ()
	       (define-key artist-mode-map (kbd "C-c C-a C-o") 'artist-ido-select-operation)
	       (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-ido-select-settings)
	       (setq indent-tabs-mode nil)))

(provide 'artist-cfg)
;;; artist-cfg.el ends here
