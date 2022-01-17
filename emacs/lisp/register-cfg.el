;;; register-cfg.el --- Register Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Richard Westhaver

;; Author: Richard Westhaver <ellis@jekyll>
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

;; 

;;; Code:
(defcustom registers-save-file (expand-file-name "registers" user-emacs-directory)
  "The place where the contents of the registers should be saved."
  :type '(file)
  :group 'default)

(defun jump-to-register-action (register &optional delete)
  "Do what is the most sane thing to do for the thing stored in
   register. Either insert text (evt. a rectangle), move point to
   location stored in a register, a buffer stored in a register,
   a file stored in register, or run a macro saved in a register.
   If the register contains a file name, find that file. Or
   restore a saved window/frame configuration."
  (let ((val (get-register register)))
    (cond
     ((registerv-p val)
      (cl-assert (registerv-jump-func val) nil
                 "Don't know how to jump to register %s"
                 (single-key-description register))
      (funcall (registerv-jump-func val) (registerv-data val)))
     ((and (consp val) (frame-configuration-p (car val)))
      (set-frame-configuration (car val) (not delete))
      (goto-char (cadr val)))
     ((and (consp val) (window-configuration-p (car val)))
      (set-window-configuration (car val))
      (goto-char (cadr val)))
     ((markerp val)
      (or (marker-buffer val)
          (error "That register's buffer no longer exists"))
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     ((and (consp val) (eq (car val) 'buffer))
      (switch-to-buffer (cdr val)))
     ((and (consp val) (eq (car val) 'macro))
                                        ;appearently the only way to run a macro is by putting them in
                                        ;last-kbd-macro (named keyboard macros can only (as far as I
                                        ;know) be called interactively, but this works quite
                                        ;unproblematically).
      (let ((old-macro last-kbd-macro))
        (setq last-kbd-macro (cdr val))
        (call-last-kbd-macro)
        (setq last-kbd-macro old-macro)))
     ((and (consp val) (eq (car val) 'file-query))
      (or (find-buffer-visiting (nth 1 val))
          (y-or-n-p (format "Visit file %s again? " (nth 1 val)))
          (error "Register access aborted"))
      (find-file (nth 1 val))
      (goto-char (nth 2 val)))
     ((or (stringp val)
          (consp val)
          (numberp val)
          (and (markerp val)
               (marker-position val)))
      (insert-register register))
     (t
      (error "Register doesn't contain a buffer, buffer position, macro, file, text, rectangle or configuration")))))

;; overwrite default
(defun jump-to-register (register &optional delete)
  (interactive (list (register-read-with-preview "Jump to register: ")
                     current-prefix-arg))
  (jump-to-register-action register delete))

(defun save-registers-hook ()
  (add-hook 'kill-emacs-hook
            #'(lambda ()
               (better-registers-save-registers))))

(defun save-registers (&optional filename queryp)
  "Print the contents of all registers to a file as loadable data.
   Cannot save window/frame configuration.
   But it works with keyboard macros, text, buffernames,
   filenames and rectangles.

   If filename is non-nil and queryp is nil, use that, otherwise
   use the default filename.  If queryp is non-nil (a prefix
   argument is given), query interactively for the file-name."
  (interactive "i\nP")
  (when queryp
    (setq filename (read-file-name nil registers-save-file)))
  (let ((fn (or filename registers-save-file))
        (print-level nil) ;Let us write anything
        (print-length nil)
        (b (generate-new-buffer "*registers*")))
    (set-buffer b)
    (dolist (i register-alist)
      (let ((char (car i))
            (contents (cdr i)))
        (cond
         ((stringp contents)
          (insert (format "%S\n"
                          `(set-register
                            ,char
                            ,contents))))
         ((numberp contents) ;numbers are printed non-quotes
          (insert (format "%S\n" `(set-register ,char ,contents))))
         ((markerp contents)
          (insert (format
                   "%S\n"
                   `(set-register
                     ,char
                     '(file-query
                       ,(buffer-file-name (marker-buffer contents))
                       ,(marker-position contents))))))
         ((bufferp (cdr contents))
          (insert (format "%s\n"
                          `(set-register ,char
                                         ',(buffer-name (cdr contents))))))
         (t (when (and contents ; different from nil
                       (not (or (window-configuration-p (car contents))
                                (frame-configuration-p (car contents)))))
              (insert (format "%S\n"
                              `(set-register ,char (quote ,contents)))))))))
    (write-file fn)
    (kill-buffer b)))

(defun put-buffer-in-register (register &optional delete)
  "Put current buffername in register - this would also work for
  just buffers, as switch-to-buffer can use both, but it
  facilitates for easier saving/restoring of registers."
  (interactive "cPut current buffername in register: \nP.")
  (set-register register (cons 'buffer (buffer-name (current-buffer)))))

(defun put-buffer-filename-in-register (register &optional delete)
  "This is better than put-buffer-in-register for file-buffers, because a closed
   file can be opened again, but does not work for no-file-buffers."
  (interactive "cPut the filename of current buffer in register: \nP")
  (set-register register (cons 'file (buffer-file-name (current-buffer)))))

(defun put-keyboard-macro-in-register (register &optional delete)
  "Save the contents of the last keyboard macro to the given register.
   can be played again by jump-to-register."
  (interactive "cPut last keyboard-macro in register: \nP")
  (set-register register (cons 'macro last-kbd-macro)))

(defun decrement-register (number register)
  "Subtract NUMBER from the contents of register REGISTER.
Interactively, NUMBER is the prefix arg."
  (interactive "p\ncDecrement register: ")
  (increment-register (- number) register))

(provide 'register-cfg)
;;; register-cfg.el ends here
