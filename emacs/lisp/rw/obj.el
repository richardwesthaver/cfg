;;; rw/obj.el --- EIEIO classes and methods -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver

;; Keywords: tools, lisp

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
(eval-when-compile (require 'cl-lib))
(require 'rw-macs "macs")
(using eieio)
;;; utils
(defun oref-or (obj slot)
  "Get the value of object or class."
  (cond
    ((class-p obj) (eieio-oref-default obj slot))
    ((eieio-object-p obj) (eieio-oref obj slot))))

(defun oref-and (obj &rest slots)
  "Return a list of values bound to SLOTS in OBJ. Uses `oref-or' internally."
  (mapcar (lambda (x) (oref-or obj x)) slots))

(defun oset-or (obj slot value)
  "Set the value of object or class."
  (cond
    ((class-p obj) (eieio-oset-default obj slot value))
    ((eieio-object-p obj) (eieio-oset obj slot value))))

(defun oset-and (obj &rest args)
  "Set each SLOT to VALUE in OBJ. Uses `oset-or' internally.
\(fn SLOT VAL SLOT VAL ...)"
  (if (/= (logand (length args) 1) 0)
      (signal 'wrong-number-of-arguments (list 'oset-and (length args)))
      (while args (oset-or obj (pop args) (pop args)))))

(defun obj-sexp-validate (sexp &rest required)
  "Validate an `obj' SEXP, returning non-nil if each
  slot symbol in REQUIRED is found."
  (if-let ((plist (cdr sexp)))
      (let (res)
        (catch 'ret
          (while required
            (let* ((prop (car required))
                   (i (plist-get plist prop)))
              (if (null i)
                  (progn
                    (message "property not found: %s" prop)
                    (throw 'ret nil))
                (progn
                  (push (cons prop i) res)
                  (setq required (cdr required))))))
          res))
    (error "invalid obj-sexp form: %s" sexp)))

;;; obj
;;
;; default superclass for our CLOS extensions
(defclass obj ()
  ((name :type (or string symbol null)
         :initform nil
         :initarg :name
         :accessor name)
   (id :type (or string integer null)
       :initform nil
       :initarg :id
       :accessor id))
  :abstract t)

(cl-defmethod object-write ((obj obj) &optional comment)
  (when (and comment eieio-print-object-name)
    (princ ";; obj:")
    (princ (eieio-object-name-string obj))
    (princ "\n"))
  (when (stringp comment)
    (mapc (lambda (x)
            (princ ";; ")
            (princ x)
            (princ "\n"))
          (split-string comment "[\n\r\v]" nil)))
  (let* ((cl (eieio-object-class obj))
	 (cv (cl--find-class cl)))
    ;; Now output readable lisp to recreate this object
    ;; It should look like this:
    ;; (<name> <:slot val> <:slot val> ... )
    ;; Each slot's slot is writen using its :writer.
    (when eieio-print-indentation
      (princ (make-string (* eieio-print-depth 2) ? )))
    (princ "(")
    (princ (eieio-oref obj :name))
    (when eieio-print-object-name
      (princ " ")
      (prin1 (eieio-object-name-string obj))
      (princ "\n"))
    ;; Loop over all the public slots
    (let ((slots (eieio--class-slots cv))
	  (eieio-print-depth (1+ eieio-print-depth)))
      (dotimes (i (length slots))
        (let* ((slot (aref slots i))
               (name (cl--slot-descriptor-name slot)))
          (when (slot-boundp obj name)
            (unless (string= "name" name)
            (let ((i (eieio--class-slot-initarg
                      cv (cl--slot-descriptor-name slot)))
                  (v (eieio-oref obj (cl--slot-descriptor-name slot))))
              (unless (or (not i) (equal v (cl--slot-descriptor-initform slot)))
                (unless (bolp)
                  (princ "\n"))
                (when eieio-print-indentation
                  (princ (make-string (* eieio-print-depth 2) ? )))
                (princ (symbol-name i))
                (if (alist-get :printer (cl--slot-descriptor-props slot))
                    ;; Use our public printer
                    (progn
                      (princ " ")
                      (funcall (alist-get :printer
                                          (cl--slot-descriptor-props slot))
                               v))
                  ;; Use our generic override prin1 function.
                  (princ (if (or (eieio-object-p v)
                                 (eieio-object-p (car-safe v)))
                             "\n" " "))
                  (eieio-override-prin1 v))))))))
    (princ ")"))))

;;; cache
;;
;; mixin class for cacheable objects. The cache is specified by a symbol
;; whose value is a hash-table.
(defclass cache (obj)
  ((cache :type symbol
          :allocation :class)
   (overwrite :type boolean
              :initarg :overwrite
              :initform t))
  :abstract t)

(cl-defmethod cache-instance ((this cache))
  (let ((table (symbol-value (oref this cache)))
        (key (id this))
        (overwrite (oref this overwrite)))
    (if (gethash key table)
        (if overwrite
            (progn
              (message "duplicate key: %s" key)
              (puthash key this table))
          (message "duplicate key: %s" key)
          nil)
      (puthash key this table))))

(cl-defmethod delete-instance ((this cache))
  "Remove THIS from cache."
  (let ((key (id this)))
    (remhash key (oref this cache)))
  (message "removed key: %s" id))

(cl-defmethod cache-clear ((this cache))
  "Reset the hashtable in THIS."
  (clrhash (oref this cache)))

(cl-defmethod update-instance ((this cache))
  "Update hash of THIS instance."
  (puthash (id this) this (oref this cache)))

(provide 'rw-obj)
;;; rw/obj.el ends here
