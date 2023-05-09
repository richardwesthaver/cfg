;;; eshell-cfg.el --- Eshell Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: terminals

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
(add-packages '(eshell-syntax-highlighting esh-help))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'Z))

(setq eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-highlight-prompt nil
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil
      eshell-destroy-buffer-when-process-dies t)

(add-hook 'eshell-mode-hook
	  (lambda () (progn
		       (eshell/alias "d" "dired $1")
		       (eshell-syntax-highlighting-global-mode +1)
		       (setup-esh-help-eldoc))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
        (progn
          (eshell-life-is-too-much) ; Why not? (eshell/exit)
          (ignore-errors
            (delete-window)))
      (delete-forward-char arg)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (bind-keys :map eshell-mode-map
                       ("C-d" . eshell-quit-or-delete-char))))

(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (re-search-forward eshell-prompt-regexp nil t n)
  (when eshell-highlight-prompt
    (while (not (get-text-property (line-beginning-position) 'read-only) )
      (re-search-forward eshell-prompt-regexp nil t n)))
  (eshell-skip-prompt))

(defun eshell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer. See `eshell-prompt-regexp'."
  (interactive "p")
  (backward-char)
  (eshell-next-prompt (- n)))

(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  (interactive)
  (insert (ido-completing-read "Eshell history: "
                               (delete-dups
                                (ring-elements eshell-history-ring)))))

(add-hook 'eshell-mode-hook (lambda ()
    (define-key eshell-mode-map (kbd "M-S-P") 'eshell-previous-prompt)
    (define-key eshell-mode-map (kbd "M-S-N") 'eshell-next-prompt)
    (define-key eshell-mode-map (kbd "M-r") 'eshell-insert-history)))

(keymap-set keys-map "C-c x x" 'eshell-new)

(provide 'eshell-cfg)
;;; eshell-cfg.el ends here
