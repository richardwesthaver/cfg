;;; rw/forth.el --- experimental forth compiler -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver

;; Author: Richard Westhaver <rwestha2@gdeb.com>
;; Keywords: languages

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

;; This is an elisp implementation of a FORTH compiler, a non-Flub
;; language which is also a Blub. Used by Doug Hoyte as an example of
;; how to make Lisp non-Flub.

;; Forth and Artemis are cut from the same cloth, in some ways. In
;; others, not so much.

;; Forth and Artemis can both be considered fixnum machines, where they
;; differ is in the core data structure - datasets in Artemis, stacks
;; for Forth.

;;; Code:
(require 'rw-macs "macs")
(require 'rw-read "read")
(install-reader:rr)
(defvar forth-registers '(pstack rstack pc dict compiling dtable))
(cl-defstruct forth-word name prev immediate thread)
(defun forth-lookup (w last)
  (if last
      (if (eql (forth-word-name last) w)
          last
        (forth-lookup
         w (forth-word-prev last)))))
(defun forth-inner-interpreter ()
  `(cl-loop
    do (cond
        ((functionp (car pc))
         (funcall (car pc)))
        ((consp (car pc))
         (push (cdr pc) rstack))
        (t
         (push (car pc) pstack)
         (setf pc (cdr pc))))
    until (and (null pc) (null rstack))))
;; Prim-form: (name immediate . forms)
(defvar forth-prim-forms nil)
(defmacro def-forth-naked-prim (&rest code)
  `(push ',code forth-prim-forms))
(defmacro def-forth-prim (&rest code)
  `(def-forth-naked-prim
     ,@code
     (setf pc (cdr pc))))

(def-forth-prim nop nil)
(def-forth-prim * nil
  (push (* (pop pstack) (pop pstack))
        pstack))
(def-forth-prim drop nil
  (pop pstack))
(def-forth-prim dup nil
  (push (car pstack) pstack))
(def-forth-prim swap nil
  (rotatef (car pstack) (cadr pstack)))
(def-forth-prim print nil
  (print "%s" (pop pstack)))
(def-forth-prim >r nil
  (push (pop pstack) rstack))
(def-forth-prim r> nil
  (push (pop rstack) pstack))
(defmacro! go-forth (o!forth &rest words)
  `(dolist (w ',words)
     (funcall ,g!forth w)))
(defvar forth-stdlib nil)
(defmacro forth-stdlib-add (&rest all)
  `(setf forth-stdlib
         (nconc forth-stdlib
                ',all)))

;; Prim-form: (name immediate . forms)
(defmacro forth-install-prims ()
  `(progn
     ,@(mapcar
        #`(let ((thread (lambda ()
                           ,@(cddr a1))))
             (setf dict
                   (make-forth-word
                    :name ',(car a1)
                    :prev dict
                    :immediate ,(cadr a1)
                    :thread thread))
             (setf (gethash thread dtable)
                   ',(cddr a1)))
        forth-prim-forms)))

(def-forth-prim \{| t (setf compiling nil)) ;; conflict with rw-read
(def-forth-prim |} nil (setf compiling t))
(defmacro forth-compile-in (v)
  `(setf (forth-word-thread dict)
         (nconc (forth-word-thread dict)
                (list ,v))))
(defmacro forth-handle-found ()
  `(if (and compiling
            (not (forth-word-immediate word)))
       (forth-compile-in (forth-word-thread word))
     (progn
       (setf pc (list (forth-word-thread word)))
       (forth-inner-interpreter))))
(defmacro forth-handle-not-found ()
  `(cond
    ((and (consp v) (eq (car v) 'quote))
     (if compiling
         (forth-compile-in (cadr v))
       (push (cadr v) pstack)))
    ((and (consp v) (eq (car v) 'postpone))
     (let ((word (forth-lookup (cadr v) dict)))
       (if (not word)
           (error "Postpone failed: %s" (cadr v)))
       (forth-compile-in (forth-word-thread word))))
;; TODO FIXME
    ((symbolp v)
     (if compiling
         (forth-compile-in v)
       (push v pstack)))
;;     (error "Word %s not found" v))
    (t (if compiling
           (forth-compile-in v)
         (push v pstack)))))

(def-forth-prim create nil (setf dict (make-forth-word :prev dict)))
(def-forth-prim name nil (setf (forth-word-name dict) (pop pstack)))
(def-forth-prim immediate nil (setf (forth-word-immediate dict) t))
(forth-stdlib-add create
                  |} create |} \{| '|- name ) ;; conflict with rw-read
(forth-stdlib-add |- (postpone \{|) \{| '-| name immediate )
(def-forth-prim @ nil
  (push (car (pop pstack))
        pstack))
(def-forth-prim ! nil
  (let ((location (pop pstack)))
    (setf (car location) (pop pstack))))

(defmacro new-forth ()
  `(alet ,forth-registers
     (setq dtable (make-hash-table))
     (forth-install-prims)
     (dolist (v forth-stdlib)
       (funcall this v))
     (plambda (v) ,forth-registers
       (let ((word (forth-lookup v dict)))
         (if word
             (forth-handle-found)
           (forth-handle-not-found))))))

(provide 'rw-forth)
;;; forth.el ends here
