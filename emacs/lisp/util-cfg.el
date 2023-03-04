;;; util-cfg.el --- Util Config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: tools, convenience

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
;;; Regex
(defvar base-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	 (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.")

;;; Macros
(defmacro alambda (parms &rest body)
  "Graham's alambda."
  (declare (indent 0))
  `(cl-labels ((self ,parms ,@body))
     #'self))

(defmacro aif (test then &optional else)
  "Graham's aif."
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

;;(setf (symbol-function 'alet-test) (alet ((acc 0))
;; (alambda (n)
;;   (if (eq n 'invert)
;;     (setq this
;;           (lambda (n)
;;             (if (eq n 'invert)
;;               (setq this #'self)
;;               (cl-decf acc n))))
;;     (cl-incf acc n)))))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro alet-fsm (&rest states)
  `(cl-macrolet ((state (s)
                `(setq this #',s)))
     (cl-labels (,@states) #',(caar states))))


(defmacro alet-hotpatch% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) ':hotpatch)
         (setq this (cadr args))
         (apply this args)))))

;; (setf (symbol-function 'hotpatch-test)
;;     (alet-hotpatch% ((acc 0))
;;       (lambda (n)
;;         (incf acc n))))

(defmacro hook-modes (modes &rest body)
  (declare (indent 1))
  `(--each ,modes
     (add-hook (intern (format "%s-hook" it))
               (lambda () ,@body))))

(defmacro define-lambda-choice (name &rest choices)
  "Define a chooser command NAME offering CHOICES.
Each of CHOICES should be a list, the first of which is the
choice's name, and the rest of which is its body forms."
  (declare (indent defun))
  ;; Avoid redefining existing, non-chooser functions.
  (cl-assert (or (not (fboundp name))
                 (get name :define-lambda-choice)))
  (let* ((choice-names (mapcar #'car choices))
         (choice-list (--map (cons (car it) #'(lambda (&rest args)
						,@(cdr it)))
                             choices))
         (prompt (format "Choose %s: " name))
         (docstring (concat "Choose between: " (s-join ", " choice-names))))
    `(progn
       (defun ,name ()
         ,docstring
         (interactive)
         (let* ((choice-name (completing-read ,prompt ',choice-names)))
           (funcall (alist-get choice-name ',choice-list nil nil #'equal))))
       (put ',name :define-lambda-choice t))))

;; (defmacro *-system (op sys &rest body)
;;   "(OP (eq system-type SYS) ,@body)"
;;   )

(defmacro with-system (sys &rest body)
  "Evaluate BODY if `system-type' equals SYS."
  (declare (indent defun))
  `(when (eq system-type ',sys)
     ,@body))

;;; Helpers
;;;###autoload
(defun random-integers (min max n)
  "Return N random integers between MIN (inclusive) and MAX (exclusive)."
  (let ((list ()))
    (dotimes (_ n)
      (push (+ (cl-random (- max min)) min) list))
    list))

;;;###autoload
(defun os-path-join (a &rest ps)
  (let ((path a))
    (while ps
      (let ((p (pop ps)))
        (cond ((string-prefix-p "/" p)
               (setq path p))
              ((or (not path) (string-suffix-p "/" p))
               (setq path (concat path p)))
              (t (setq path (concat path "/" p))))))
    path))

;;;###autoload
(defun int-to-binary-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

;;;###autoload
(defun read-elisp-data (file)
  "Read Elisp data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path"
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defun kill-ring-car-espace-quotes ()
  "Escape doublequotes in car of kill-ring "
  (interactive)
  (with-temp-buffer
    (insert (car kill-ring))
    (goto-char (point-min))
    (while (search-forward "\"" nil t 1)
      (replace-match "\\\\\""))
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'util-cfg)
;;; util-cfg.el ends here
