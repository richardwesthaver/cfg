;;; fmt-cfg.el --- Fmt Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: Richard Westhaver <ellis@rwest.io>
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
(defvar flex-fill-paragraph-column nil
  "Last fill column used in command `flex-fill-paragraph'.")

;;;###autoload
(defun flex-fill-paragraph (&optional fewer-lines unfill)
  "Fill paragraph, incrementing fill column to cause a change when repeated.
The global value of `fill-column' is not modified; it is only
bound around calls to `fill-paragraph'.

When called for the first time in a sequence, unfill to the
default `fill-column'.

When called repeatedly, increase `fill-column' until filling
changes.

With one universal prefix, increase `fill-column' until the
number of lines is reduced.  With two, unfill completely."
  (interactive "P")
  (let* ((fewer-lines (or fewer-lines (equal current-prefix-arg '(4))))
         (unfill (or unfill (equal current-prefix-arg '(16))))
         (fill-column
          (cond (unfill (setf flex-fill-paragraph-column nil)
                        most-positive-fixnum)
                (t (setf flex-fill-paragraph-column
                         (if (equal last-command this-command)
                             (or (flex-fill-paragraph--next-fill-column fewer-lines)
                                 fill-column)
                           fill-column))))))
    (fill-paragraph)
    (message "Fill column: %s" fill-column)))

(defun flex-fill-paragraph--next-fill-column (&optional fewer-lines)
  "Return next `fill-column' value.
If FEWER-LINES is non-nil, reduce the number of lines in the
buffer, otherwise just change the current paragraph."
  ;; This works well, but because of all the temp buffers, sometimes
  ;; when called in rapid succession, it can cause GC, which can be
  ;; noticeable.  It would be nice to avoid that.  Note that this has
  ;; primarily been tested on `emacs-lisp-mode'; hopefully it works
  ;; well in other modes.
  (let* ((point (point))
         (source-buffer (current-buffer))
         (mode major-mode)
         (fill-column (or flex-fill-paragraph-column fill-column))
         (old-fill-column fill-column)
         (hash (unless fewer-lines
                 (buffer-hash)))
         (original-num-lines (when fewer-lines
                               (line-number-at-pos (point-max)))))
    (with-temp-buffer
      (delay-mode-hooks
        (funcall mode))
      (insert-buffer-substring source-buffer)
      (goto-char point)
      (cl-loop while (and (fill-paragraph)
                          (if fewer-lines
                              (= original-num-lines (line-number-at-pos (point-max)))
                            (string= hash (buffer-hash))))
               ;; If filling doesn't change after 100 iterations, abort by returning nil.
               if (> (- fill-column old-fill-column) 100)
               return nil
               else do (cl-incf fill-column)
               finally return fill-column))))

(provide 'fmt-cfg)
;;; fmt-cfg.el ends here
