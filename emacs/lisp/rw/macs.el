;;; rw-macs.el --- RW macros -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver
;; Version: "0.1.0"
;; Keywords: languages, convenience, tools, data, internal

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
(require 'subr-x)
(require 'seq)
(require 'rw-read "read")
;; ported from obsolete library cl.el
(defmacro defsetf (name arg1 &rest args)
  "Define a `setf' method.
This macro is an easy-to-use substitute for `define-setf-expander'
that works well for simple place forms.

In the simple `defsetf' form, `setf's of the form (setf (NAME
ARGS...) VAL) are transformed to function or macro calls of the
form (FUNC ARGS... VAL).  For example:

  (defsetf aref aset)

You can replace this form with `gv-define-simple-setter'.

Alternate form: (defsetf NAME ARGLIST (STORE) BODY...).

Here, the above `setf' call is expanded by binding the argument
forms ARGS according to ARGLIST, binding the value form VAL to
STORE, then executing BODY, which must return a Lisp form that
does the necessary `setf' operation.  Actually, ARGLIST and STORE
may be bound to temporary variables which are introduced
automatically to preserve proper execution order of the arguments.
For example:

  (defsetf nth (n x) (v) \\=`(setcar (nthcdr ,n ,x) ,v))

You can replace this form with `gv-define-setter'.

\(fn NAME [FUNC | ARGLIST (STORE) BODY...])"
  (declare (debug
            (&define name
                     [&or [symbolp &optional stringp]
                          [cl-lambda-list (symbolp)]]
                     cl-declarations-or-string def-body)))
  (if (and (listp arg1) (consp args))
      ;; Like `gv-define-setter' but with `cl-function'.
      `(gv-define-expander ,name
         (lambda (do &rest args)
           (gv--defsetter ',name
                          (cl-function
                           (lambda (,@(car args) ,@arg1) ,@(cdr args)))
			  do args)))
    `(gv-define-simple-setter ,name ,arg1 ,(car args))))

;; Macros for the macro programmer.

;; TODO 2023-04-21: (with-output-to-string (s) &rest body)

(defun group (source n)
  "This is Paul Graham's group utility from 'On Lisp'.

Group a list of arguments SOURCE by any provided grouping amount
N.

For example:
(group '(foo 2 bar 4) 2) ;=> ((foo 2) (bar 4))
(group '(a b c d e f) 3) ;=> ((a b c) (d e f))
"
  (when (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons
                                    (cl-subseq source 0 n)
                                    acc))
                       (nreverse
                        (cons source acc))))))
    (when source (rec source nil))))

(defun flatten (x)
  "Paul Graham's flatten utility from 'On Lisp'.

Given a tree X, return all the 'leaves' of the tree."
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec
                             (car x)
                             (rec (cdr x) acc))))))
    (rec x nil)))

(defun mkstr (&rest args)
  "Paul Graham's mkstr utility from 'On Lisp'.

Coerce ARGS into a single string and return it."
  (let* ((s ""))
    (dolist (a args)
      (cond
       ((null a) nil)
       ((sequencep a) (setq s (concat s a)))
       ((numberp a) (setq s(concat s (number-to-string a))))
       ((symbolp a) (setq s(concat s (symbol-name a))))))
    s))

(defun symb (&rest args)
  "Paul Graham's symb utility from 'On Lisp'.

Concat ARGS and return a newly interned symbol."
  (intern (apply #'mkstr args)))

(defun g!-symbol-p (s)
  "Return t if S is a G-bang symbol."
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       ;; case doesn't matter -- g!sym = G!sym
       (string-prefix-p "G!" (substring (symbol-name s) 0 2) t)))

(defun o!-symbol-p (s)
  "Return t if S is an O-bang symbol."
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string-prefix-p "O!" (substring (symbol-name s) 0 2) t)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "g!" (seq-subseq (symbol-name s) 2)))

(defmacro defmacro/g! (name arglist &optional docstring &rest body)
  "G-bang notation from 'Let Over Lambda' by Doug Hoyte."
  (declare (doc-string 3) (indent 2))
  (let ((syms
         (cl-remove-duplicates
          (cl-remove-if-not #'g!-symbol-p
                            (flatten-tree body)))))
    (unless (and (stringp docstring) (listp body))
      (setq body (cons docstring body))
      (setq docstring ""))
    `(defmacro ,name ,arglist
       ,docstring
       (let ,(cl-mapcar
              (lambda (s)
                `(,s (gensym ,(seq-subseq (symbol-name s) 2))))
              syms)
         ,@body))))

;; (defmacro/g! nif (expr pos zero neg)
;;   "Paul Graham's nif."
;;   `(let ((,g!res ,expr))
;;      (cond ((cl-plusp ,g!res) ,pos)
;;            ((zerop ,g!res) ,zero)
;;            (t ,neg))))

(defmacro defmacro! (name arglist &optional docstring &rest body)
  "DEFMACRO-BANG

This is a defmacro form which supports Doug Hoyte's gensym and
once-only syntax notation.

Any symbol starting with 'g!' in BODY is a phantom which is
expanded into an uninterned gensym symbol.

Additionally, symbols starting with 'o!' are evaluated exactly
once. This is an implementation of Peter Norvig's 'once-only' in
PAIP."
  (declare (doc-string 3) (indent 2))
  (let* ((os (cl-remove-if-not #'o!-symbol-p arglist))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (unless (and (stringp docstring) (listp body))
      (setq body (cons docstring body))
      (setq docstring ""))
    `(defmacro/g! ,name ,arglist
       ,docstring
       `(let ,(cl-mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

;; (defmacro! square (o!x)
;;   `(progn
;;      (message "[%s = %s]" ',o!x ,g!x)
;;      (* ,g!x ,g!x)))

;;; unit conversion

;; Doug Hoyte's 'defunits' implementation in elisp.

(defun defunits-chaining (u units &optional prev)
  "Doug Hoyte's defunits-chaining from 'Let Over Lambda'.

Give me a unit U, a list of unit specs UNITS, and an optional
list of units previously visited PREV.

Unit specs take the form (UNIT VAL). VAL may be a single number
which is interpreted as a quantity of the base unit, or a list
with an indirection to another unit in a 'chain'.

One important modification we made is support for evaluation of
the unit quantity if the quantity provided satisfies `listp' or
`symbolp'.

For example:
(defunits distance m
  M 60
  CM (.01 M)
  INCH ((/ 12.0) foot))"
  (when (member u prev)
    (error "%s depends on %s" u prev))
  (let ((spec (cl-find u units :key #'car)))
    (if (null spec)
        (error "Unknown unit %s" u)
      (let ((chain (cadr spec)))
        (if (listp chain)
            (progn
              (when (or
                     (symbolp (car chain))
                     (listp (car chain)))
                (setf (car chain) (eval (car chain))))
              (* (car chain)
                 (defunits-chaining
                   (cadr chain)
                   units
                   (cons u prev))))
          chain)))))

(defmacro! defunits (quantity base-unit &rest units)
  "Doug Hoyte's defunits from 'Let Over Lambda'."
  `(defmacro ,(symb 'unit-of- quantity)
       (,g!val ,g!un)
     `(* ,,g!val
         ,(cl-case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining
                             (car x)
                             (cons `(,base-unit 1)
                                   (group units 2))
                             nil)))
                      (group units 2))))))

;; (defunits distance m
;;   km 1000
;;   cm .01
;;   mm (.1 cm)
;;   nm (.001 mm)
;;   yard 0.9144 ;; defined in 1956
;;   foot ((/ 3.0) yard)
;;   inch ((/ 12.0) foot)
;;   mile (1760 yard)
;;   furlong ((/ 8.0) mile)
;;   fathom (2 yard) ;; defined in 1929
;;   nautical-mile 1852
;;   cable (.1 nautical-mile)
;;   old-brit-nautical-mile ((/ 6080 3.0) yard) ;; dropped in 1970
;;   old-brit-cable (.1 old-brit-nautical-mile)
;;   old-brit-fathom (.01 old-brit-cable))

(defun tree-leaves% (tree test result)
  "Inner function for Doug Hoyte's tree-leaves from 'Let Over Lambda'."
  (if tree
      (if (listp tree)
          (cons (tree-leaves% (car tree) test result)
                (tree-leaves% (cdr tree) test result))
        (if (funcall test tree)
            (funcall result tree)
          tree))))

(defmacro tree-leaves (tree test result)
  "Doug Hoyte's tree-leaves from 'Let Over Lambda'."
  `(tree-leaves%
    ,tree
    (lambda (&optional x)
      ,test)
    (lambda (&optional x)
      ,result)))

(defmacro! dlambda (&rest ds)
  "Doug Hoyte's dlambda from 'Let Over Lambda'.

dlambda's are similar to the conventional lambda, which it
expands to. This expansion includes a way for one of many
different branches of code to be executed, depending on the
arguments it is applied to. In other words, Dynamic Dispatch.

The 'd' typically stands for 'destructuring' or 'dispatching'.

Example of the 'let over dlambda' pattern:
(setf (symbol-function 'count-test)
  (let ((count 0))
    (dlambda
      (:inc () (cl-incf count))
      (:dec () (cl-decf count))))) ;=> closure

(count-test :inc) ;=> 1
(count-test :dec) ;=> 0"
  `(lambda (&rest ,g!args)
     (cl-case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                 (list (car d)))
              (apply (lambda ,@(cdr d))
                     ,(if (eq t (car d))
                          g!args
                        `(cdr ,g!args)))))
          ds))))

;;; anaphoric macros
(defmacro alambda (parms &rest body)
  "Paul Graham's alambda from 'On Lisp'."
  (declare (indent 1))
  `(cl-labels ((self ,parms ,@body))
     #'self))

;; (alambda (n)
;;   (if (> n 0)
;;       (cons n (self (- n 1)))))

(defmacro aif (test then &optional else)
  "Paul Graham's aif from 'On Lisp'."
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro alet (letargs &rest body)
  "Doug Hoyte's alet from 'Let Over Lambda'.

Let form with returning body form bound to 'this'."
  (declare (indent 1))
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro alet-fsm (&rest states)
  "Doug Hoyte's alt-fsm from 'Let Over Lambda'."
  `(cl-macrolet ((state (s)
                        `(setq this #',s)))
     (cl-labels (,@states) #',(caar states))))

;; (setf fun
;;       (alet ((acc 0))
;;         (alet-fsm
;;          (up (n)
;;              (if (eq n 'invert)
;;                  (state down)
;;                (cl-incf acc n)))
;;          (down (n)
;;                (if (eq n 'invert)
;;                    (state up)
;;                  (cl-decf acc n))))))
;;
;; (funcall fun 10) ;=> 10
;; (funcall fun 10) ;=> 20
;; (funcall fun 'invert) ;=> (closure ...)
;; (funcall fun 10) ;=> 10


(defmacro! ichain-before (&rest body)
  "Doug Hoyte's ichain-before from 'Let Over Lambda'."
  `(let ((,g!indir-env this))
     (setq this (lambda (&rest ,g!temp-args)
                  ,@body
                  (apply ,g!indir-env ,g!temp-args)))))

;; (setf fun (alet ((acc 0))
;;   (ichain-before
;;    (message "Changing from %s" acc))
;;   (lambda (n)
;;     (cl-incf acc n))))
;;
;; (funcall fun 2) ;=> (message: "Changing from 0") 2
;; (funcall fun 2) ;=> (message: "Changing from 2") 4

(defmacro! ichain-after (&rest body)
  "Doug Hoyte's ichain-after from 'Let Over Lambda'."
  `(let ((,g!indir-env this))
     (setq this (lambda (&rest ,g!temp-args)
                  (prog1
                      (apply ,g!indir-env ,g!temp-args)
                    ,@body)))))

;; (setf fun (alet ((acc 0))
;;   (ichain-before
;;    (message "Changing from %s" acc))
;;   (lambda (n)
;;     (cl-incf acc n))))
;;
;; (funcall fun 7) ;=> (message "Changing from 0") 7 (message "Changed to 7")

(defmacro! ichain-intercept (&rest body)
  "Doug Hoyte's ichain-intercept from 'Let Over Lambda'.

This macro creates a local macro that allows the code inside
`ichain-intercept' to use the block anaphor `intercept' to expand
into a `cl-return-from' where the block is specified by a gensym.

Note that in Common Lisp, we would instead expand the local macro
into a `return-from', which I felt was worth noting considering
the trickiness of anaphora.

This is not the most cost efficient design for many things, tread
<carefully."
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (cl-block ,g!intercept
               (cl-macrolet
                   ((intercept (v)
                               `(cl-return-from
                                    ,',g!intercept
                                  ,v)))
                 (prog1
                     (apply ,g!indir-env ,g!temp-args)
                   ,@body)))))))

;; (setf x (alet ((acc 0))
;;   (ichain-intercept
;;    (when (< acc 0)
;;      (message "Acc went negative")
;;      (setq acc 0)
;;      (intercept acc)))
;;   (lambda (n)
;;     (cl-incf acc n))))
;;
;; (funcall x -8) ;=> (message: "Acc went negative") 0

(defmacro alet-hotpatch (letargs &rest body)
  "Doug Hoyte's alet-hotpatch from 'Let Over Lambda'."
  (declare (indent 1))
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq this closure))
      (t (&rest args)
         (apply this args)))))

;; (setf (symbol-function 'hotpatch-test)
;;       (alet-hotpatch ((acc 0))
;;         (lambda (n)
;;           (cl-incf acc n))))
;;
;; (hotpatch-test 3) ;=> 3
;; (hotpatch-test 4) ;=> 7
;;
;; ;; but wait, there's more!
;; (hotpatch-test :hotpatch
;;                (let ((acc 0))
;;                  (lambda (n)
;;                    (cl-incf acc (* 2 n)))))
;;
;; (hotpatch-test 2) ;=> 4
;; (hotpatch-test 4) ;=> 12

(defmacro let-hotpatch (letargs &rest body)
  "Doug Hoyte's let-hotpatch from 'Let Over Lambda'."
  (declare (indent 1))
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq ,g!this closure))
      (t (&rest args)
         (apply ,g!this args)))))


(defun let-binding-transform (bs)
  "Doug Hoyte's let-binding-transform from 'Let Over Lambda'."
  (if bs
      (cons
       (cond ((symbolp (car bs))
              (list (car bs)))
             ((consp (car bs))
              (car bs))
             (t (error "Bad let bindings")))
       (let-binding-transform (cdr bs)))))

(defmacro sublet (bindings% &rest body)
  "Doug Hoyte's sublet from 'Let Over Lambda'."
  (declare (indent 1))
  (let ((bindings (let-binding-transform bindings%)))
    (setq bindings
          (mapcar
           (lambda (x)
             (cons (gensym (symbol-name (car x))) x))
           bindings))
    `(let (,@(cl-mapcar #'list
                        (mapcar #'car bindings)
                        (mapcar #'caddr bindings)))
       ,@(tree-leaves
          body
          #1=(cl-member x bindings :key #'cadr)
          (caar #1#)))))

(defmacro sublet* (bindings &rest body)
  "Doug Hoyte's sublet* from 'Let Over Lambda'."
  (declare (indent 1))
  `(sublet ,bindings
     ,@(mapcar #'macroexpand-1 body)))

;;; pandoric macros
(defmacro pandoriclet (letargs &rest body)
  "Doug Hoyte's pandoriclet from 'Let Over Lambda'."
  (declare (indent 1))
  (let ((letargs (cons
                  '(this)
                  (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
        (:pandoric-get (sym)
                       ,(pandoriclet-get letargs))
        (:pandoric-set (sym val)
                       ,(pandoriclet-set letargs))
        (t (&rest args)
           (apply this args))))))

(install-reader:rr) ;; TODO make this a compile switch
(defun pandoriclet-get (letargs)
  "Doug Hoyte's pandoriclet-get from 'Let Over Lambda'."
  `(cl-case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error
         "Unknown pandoric get: %s" sym))))

(defun pandoriclet-set (letargs)
  "Doug Hoyte's pandoriclet-set from 'Let Over Lambda'."
  `(cl-case sym
     ,@(mapcar #`((,(car a1))
                  (setq ,(car a1) val))
               letargs)
     (t (error "Unknown pandoric set: (%s %s)" sym val))))

(defsubst get-pandoric (box sym)
  (funcall box :pandoric-get sym))

(defsetf get-pandoric (box sym) (val)
  `(progn (funcall ,box :pandoric-set ,sym ,val)
          ,val))

(defmacro! with-pandoric (syms o!box &rest body)
  `(cl-symbol-macrolet (,@(mapcar #`(,a1 (get-pandoric ,g!box ',a1))
                                  syms))
     ,@body))

(defun pandoric-hotpatch (box new)
  (with-pandoric (this) box
                 (setq this new)))

(defmacro pandoric-recode (vars box new)
  `(with-pandoric (this ,@vars) ,box
                  (setq this ,new)))
(defmacro plambda (largs pargs &rest body)
  (let ((pargs (mapcar #'list pargs)))
    `(let (this self)
       (setq
        this (lambda ,largs ,@body)
        self (dlambda
              (:pandoric-get (sym)
                             ,(pandoriclet-get pargs))
              (:pandoric-set (sym val)
                             ,(pandoriclet-set pargs))
              (t (&rest args)
                 (apply this args)))))))

(defmacro defpan (name args &rest body)
  `(defun ,name (self)
     ,(if args
          `(with-pandoric ,args self
                          ,@body)
        `(progn ,@body))))

(defvar pandoriv-eval-tunnel)

(defmacro pandoric-eval (vars expr)
  `(let ((pandoric-eval-tunnel
          (plambda () ,vars t)))
     (eval `(with-pandoric
             ,',vars pandoric-eval-tunnel
             ,,expr))))


;;; init macros

;; the macros here are intended for user initialization and
;; configuration of Emacs packages.

(defmacro! defsym-x (x)
  "Define a set of functions for handling symbols containing path
  separator X, for example parent-child or parent:child.

Functions defined are symXp, symXchild, and symXparent.

Be careful with the value you pass, as we use 'greedy' regexp
matching to factor out empty symbol components.

For example:
(defsym-x -) ; (sym-child 'foo--bar) => bar
(defsym-x a) ; (symachild 'caaadr)   => dr"
  nil)

(defun sym-path-p (s)
  "Return index of ?- character if symbol S is a module path such
as `rw-macs'."
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string-match "[-]" (symbol-name s))))

(defun sym-path-child (s)
  "Return 'file-name-base' of path-sym S as a string."
  (when-let ((p (sym-path-p s))
             (s (symbol-name s)))
  (substring s (1+ p))))

(defun sym-child (s)
  "Return child module of symbol S. If S is just a parent (no dashes), ")

(defun sym-path-parent (s)
  "Return parent of path-sym S as a string."
  (when-let ((p (sym-path-p s))
             (s (symbol-name s)))
    (substring s 0 p)))

(defmacro requires (&rest args)
  "Apply `require' to ARGS in sequence."
  (declare (indent 0))
  `(mapc
    #'require
    ',args))

(defmacro using (&rest args)
  "Apply `require' to sym-paths ARGS."
  (declare (indent 0))
  `(mapc
    (lambda (x) (require x (sym-path-child x)))
    ',args))

(defmacro unload-features (&rest features))

(defmacro hook-modes (modes &rest body)
  "Add BODY to each list of hooks in MODES. For example, a value
of 'lisp-mode will add BODY to `lisp-mode-hook'."
  (declare (indent 1))
  ;; this is a modified version of my original -- Emacs 27 doesn't have
  ;; `dash' built-in, so we resort to `dolist'.
  `(dolist (m ,modes)
           (add-hook (intern (format "%s-hook" m))
                     (lambda () ,@body))))

;; This really isn't useful unless you're running an Emacs server from
;; Linux/WSL.
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;;; fs macros

;; Emacs doesn't have good APIs for working with files and
;; directories. This section attempts to remedy some of that.

(defmacro with-file (file &rest body)
  "Open FILE and eval BODY."
  (declare (indent 1))
  `(with-current-buffer (find-file-noselect ,file)
     ,@body))

(defmacro with-files (files &rest body)
  "Open each file in FILES in turn and eval BODY."
  `(dolist (f ,files)
     (with-file f ,@body)))

(defmacro with-directory-files (dir &optional regexp recursive &rest body)
  "Open each file in DIR and eval BODY. If REGEXP is non-nil,
only operate on matching files. If RECURSIVE is non-nil, also
recurse into sub-directories.

This is just a handy wrapper for `directory-files' functions."
  (declare (indent 3))
  `(let* ((dir (expand-file-name
              (if (directory-name-p ,dir)
                  ,dir
                (concat ,dir "/"))))
         (files (if ,recursive
                    (directory-files-recursively ,dir (or ,regexp ".*"))
                  (directory-files ,dir t ,regexp))))
    (with-files files ,@body)))

(defmacro browse-files (&optional dir &rest files)
  "Browse FILES under optional directory DIR. Uses `browse-url'
  internally."
     (when dir
       (setf files (mapcar (lambda (x) (concat dir x)) files)))
     `(mapc #'browse-url ',files))

(defmacro with-dir (dir &rest body)
  "Eval BODY with default-directory bound to DIR."
  (declare (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

(defmacro path-join (base rel)
  "Join relative path REL onto BASE directory. The resulting path
  need not point to an existing file. BASE is always assumed to
  be a directory."
  `(concat (file-name-as-directory ,base) ,rel))

;;; set macros

;; these macros are use to simplify usage of set* functions/macros.

;; (defmacro xset)

(defmacro sett (&rest syms)
  "Set the value of all SYMS to t with `set'."
  `(mapc (lambda (x) (set x t)) ',syms))

(defmacro setn (&rest syms)
  "Set the values of all SYMS to nil with `set'."
  `(mapc (lambda (x) (set x nil)) ',syms))

;;; interactive macros

;; generate boilerplate for interactive commands

;; TODO 2023-04-28: use d/alambda
(defmacro! make-prompt! (o!var &optional o!prompt)
  "Generate a 'prompter' from list or variable VAR and optional
PROMPT string.

This isn't an ideal solution as it does in fact expose a dynamic
variable (VAR-prompt-history). We should generate accessors and
keep the variables within lexical scope of the generated
closure."
  `(let ((,g!s (cond ;; prefix symbol
                ((listp ,o!var) ,o!var)
                ((boundp ',o!var) (symbol-value ,o!var))
                (t (error "Second arg should be a list or variable."))))
         (,g!p ,(when (stringp o!prompt) o!prompt)) ;; prompt string
         (,g!h ',(symb o!var '-prompt-history))) ;; history symbol
     ;;1 we use defvar for the implicit bindp check
     (defvar ,(symb o!var '-prompt-history) nil)
     ;;2 our new SYM-prompt macro
     (defun ,(symb o!var '-prompt) (&optional default)
       ,(format "Prompt for a value from `%s', use DEFAULT if non-nil
and no value is provided by user, otherwise fallback to the `car'
of `%s-prompt-history'." #1=o!var #1#)
       (completing-read
        (format "%s [%s]: " ,g!p (car-safe (symbol-value ,g!h)))
        ,g!s nil nil nil ,g!h (car-safe (symbol-value ,g!h))))))

;;; maintenance macros

;; TODO 2023-04-14
(defmacro todo! ()
  "Implementation of Rust's 'todo!()' macro. See
  <https://doc.rust-lang.org/core/macro.todo.html> for details.")

(defmacro unimplemented! ()
  "Implementation of Rust's 'unimplemented!()' macro. See
  <https://doc.rust-lang.org/core/macro.unimplemented.html> for
  details.")

(defalias 'nyi #'unimplemented!)

(provide 'rw-macs)
;;; rw-macs.el ends here
