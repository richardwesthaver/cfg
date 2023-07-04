;;; read.el --- RW reader mods -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver

;; Author: Richard Westhaver <rwestha2@gdeb.com>
;; Keywords: convenience, languages, lisp, internal

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

;; This library covers some low-level hacks to elisp's code reading
;; components. In other words custom `read' functions.

;; I should start by mentioning that this implementation is extremely
;; slow. We are injecting elisp code before a C function
;; call. Byte-compilation helps with this, but acceptable performance
;; can only be achieved by modifying the C source code.

;; This implementation targets Emacs 27.2, but will run MUCH faster on
;; 28.2+ with native-compilation enabled.

;; Modification of C source code is not a possibility at EB, so we're
;; stuck with the slow impl. I am actively working on a Rust/C impl in
;; my spare time which I'm happy to share notes on upon inquiry.

;; Elisp is decades old but has somehow managed to maintain its Lisp
;; status despite not having read macro support built-in. Our partial
;; goal here is to remedy that. This is something that has been
;; discussed in the community over the years but never implemented
;; beyond a prototype.

;;; Code:
(defvar *read-og* (symbol-function 'read))
(defvar *read-from-string-og* (symbol-function 'read-from-string))
(defvar *read-insym*)
(defvar read-macro-chars (make-hash-table :test #'eq))
(defvar read-filename nil)
(defvar read-prefix-symbols (make-hash-table :test #'equal))
(defvar read-substitutions)
(defconst read--all-digits '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?a ?b
?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q r? ?s ?t ?u ?v ?w ?x
?y ?z))

(defun read-og ()
  "Calls the original C function `read' on `*read-insym*'. This
  function should be invoked only within the dynamic extent of
  some `read' or `read-from-string' execution."
  (funcall *read-og* '*read-insym*))

(defun read-from-string-og ()
  (funcall *read-from-string-og* '*read-insym*))

(defun def-reader-syntax (ch reader)
  (puthash ch reader read-macro-chars))

;; RESEARCH 2023-05-04: scheme/cl streams -- elisp concept of stream is
;; very weak. we can develop a better api and make this a macro.
(defun read-make-stream (in)
  (let ((unget nil))
    (when (symbolp in)
      (setq in (symbol-function in)))
    (cond
     ((bufferp in) (lambda (&optional ch)
                     (with-current-buffer in
                       (cond
                        (ch (push ch unget))
                        (unget (pop unget))
                        (t
                         (when (not (eobp))
                           (prog1 (char-after)
                             (forward-char 1))))))))
     ((markerp in) (lambda (&optional ch)
                     (with-current-buffer (marker-buffer in)
                       (cond
                        (ch (push ch unget))
                        (unget (pop unget))
                        (t
                         (when (< (marker-position in) (point-max))
                           (prog1 (char-after in)
                             (move-marker in
                                          (1+ (marker-position in))
                                          (marker-buffer in)))))))))
     ((stringp in) (let ((pos 0))
                     (lambda (&optional ch)
                       (cond
                        ((eq ch :pos)
                         (if (< pos (length in))
                             (- pos 1)
                           pos))
                        (ch (push ch unget))
                        (unget (pop unget))
                        ((< pos (length in))
                         (prog1 (aref in pos)
                           (setq pos (1+ pos))))))))
     ((functionp in (lambda (&optional ch)
                      (cond
                       (ch (push ch unget))
                       (unget (pop unget))
                       (t (funcall in))))))
     (t (read-string "Lisp expression:")))))

;;; The guts
(defun read--peek (in)
  "Given a stream function, return the next char without dropping
  it from the stream."
  (let ((ch (funcall in)))
    (funcall in ch)
    ch))

(defun read--next (in)
  "Given a stream function, return and discard the next char."
  (funcall in))

(defun read-while (in pred)
  "Read and return a string from the input stream, as long as the
  predicate -- which will be called for each char -- returns
  non-nil."
  (let ((chars (list)) ch)
    (while (and (setq ch (read--peek in))
                (funcall pred ch))
      (push (read--next in) chars))
    (apply #'string (nreverse chars))))

(defun read-error (msg &rest args)
  "Return error on failed read."
  (if args
      (apply #'error msg args)
    (error "%s" msg)))

(defun read--string () (read-og))
(defun read--char () (read-og))
(defun read--letter? (ch)
  (memq (get-char-code-property ch 'general-category)
        '(Ll Lu Lo Lt Lm Mn Mc Me Nl)))

(defun read--whitespace? (ch)
  (memq ch '(? ?\t ?\n ?\f ?\r #xa0)))

(defun read--digit? (ch)
  (<= ?0 ch ?9))

(defun read--number? (str)
  ":)"
  (string-match "^[-+]?\\(?:\\(?:[0-9]+\\|[0-9]*\\.[0-9]+\\)\\(?:[E|e][+|-]?[0-9]+\\)?\\)$" str))

(defun read--skip-whitespace (in)
  (read-while in #'read--whitespace?))

(defun read--symbol-name (in)
  (read-while in (lambda (ch)
                   (cond ((eq ch ?\\)
                          (read--next in)
                          (if (read--peek in) t (read-error "Unterminated input")))
                         (t
                          (or (read--letter? ch)
                              (read--digit? ch)
                              (memq ch
                                    '(?- ?+ ?= ?* ?/ ?_ ?~ ?! ?@ ?. ?\| ?$ ?% ?^ ?& ?: ?< ?> ?{ ?} ?\?))))))))

(defun read--integer (in)
  (let ((num (read-while in #'read--digit?)))
    (when (< 0 (length num))
      (string-to-number num))))

(defun read--skip-comment (in)
  (read-while in (lambda (ch)
                   (not (eq ch ?\n)))))

;; TODO 2023-05-04: use rw-macs symb-x functions
(defun read--maybe-prefixed (name &optional filename)
  (unless filename (setq filename (read-get-filename)))
    (let* ((f (gethash filename read-prefix-symbols))
           prefix)
      (cond
       ((not f)
        name)
       ((and (setq prefix (gethash name f))
             (zerop (length prefix)))
        name)
       (prefix
        (format "%s-%s" prefix name))
       ((intern-soft name)
        name)
       ((setq prefix (gethash "" f))
        (format "%s-%s" prefix name))
       (t name))))

(defun read--symbol (in)
  (let ((name (read--symbol-name in)))
    (cond
     ((read--number? name)
      (funcall *read-og* name))
     ((zerop (length name))
      '##)
     (t
      (intern (read--maybe-prefixed name))))))

(defun read--datum (in)
  (read--skip-whitespace in)
  (let ((ch (read--peek in)) macrochar)
    (cond
     ((not ch)
      (read-error "End of file during parsing"))
     ((eq ch ?\;)
      (read--skip-comment in)
      (read--datum in))
     ((eq ch ?\")
      (read--string))
     ((eq ch ?\?)
      (read--char))
     ((eq ch ?\()
      (read--next in)
      (read--list in ?\)))
     ((eq ch ?\[)
      (read--next in)
      (apply #'vector (read--list in ?\] t)))
     ((eq ch ?\')
      (read--next in)
      (list 'quote (read--datum in)))
     ((eq ch ?\`)
      (read--next in)
      (list '\` (read--datum in)))
     ((eq ch ?\,)
      (read--next in)
      (cond
       ((eq (read--peek in) ?\@)
        (read--next in)
        (list '\,@ (read--datum in)))
       (t (list '\, (read--datum in)))))
     ((setq macrochar (gethash ch read-macro-chars))
      (read--next in)
      (funcall macrochar in ch))
     (t
      (read--symbol in)))))

(defun read--list (in end &optional no-dot)
  "Read a list of elements from the input stream, until the end
  of character has been observed. If `no-dot' is nil then it will
  support a dot character before the last element, producing an
  \"improper\" list. If `no-dot' is non-nil, then if a single dot
  character is encountered this will produce an error."
  (let ((ret nil) (p nil) ch)
    (catch 'exit
      (while t
        (read--skip-whitespace in)
        (setq ch (read--peek in))
        (cond
         ((not ch)
          (read-error "Unterminated list"))
         ((eq ch end)
          (read--next in)
          (throw 'exit ret))
         ((eq ch ?\;)
          (read--skip-comment in))
         (t
          (let ((x (read--datum in)))
            (cond
             ((eq x '\.)
              (cond
               (no-dot (read-error "Dot in wrong context"))
               (t
                (rplacd p (read--datum in))
                (read--skip-whitespace in)
                (setq ch (read--next in))
                (unless (eq ch end)
                  (read-error "Dot in wrong context"))
                (throw 'exit ret))))
             (t
              (let ((cell (cons x nil)))
                (setq p (if ret
                            (rplacd p cell)
                          (setq ret cell)))))))))))))

;; http://git.savannah.gnu.org/cgit/emacs.git/commit?id=711ca36
(defun read--internal (in)
  (fset '*read-insym* in)
  (unwind-protect
      (let ((read-substitutions (list)))
        (read--datum in))
    (fset '*read-insym* nil)))

(defun read--index (elt lst)
  (let ((idx 0))
    (catch 'exit
      (while lst
        (if (eq elt (car lst))
            (throw 'exit idx)
          (setq idx (1+ idx))
          (setq lst (cdr lst)))))))

(defun read--substitute (og cell)
  (cl-labels ((subst-in (x)
                        (cond
                         ((eq x cell)
                          og)
                         ((consp x)
                          (subst-in-list x)
                          x)
                         ((stringp x)
                          x)
                         ((arrayp x)
                          (subst-in-array x)
                          x)
                         (t
                          x)))
              (subst-in-list (l)
                             (rplaca l (subst-in (car l)))
                             (rplacd l (subst-in (cdr l))))
              (subst-in-array (a)
                              (cl-loop for y across a
                                       for i upfrom 0
                                       do (aset a i (subst-in y)))))
    (subst-in og)))

(defun read-get-filename ()
  (or read-filename
      load-file-name
      (and (boundp 'byte-compile-current-file) byte-compile-current-file)
      (and (boundp 'byte-compile-dest-file) byte-compile-dest-file)
      (buffer-file-name (current-buffer))
      (buffer-name (current-buffer))))

(defun read-make-prefixed (name &optional pfx filename)
  (unless filename (setq filename (read-get-filename)))
  (unless pfx (setq pfx filename))
  (let ((f (gethash filename read-prefix-symbols)))
    (unless f
      (setq f (make-hash-table :test #'equal))
      (puthash filename f read-prefix-symbols))
    (puthash name pfx f)
    nil))

(defmacro local (pfx &optional names)
  "Declare that the given names (list of strings) should be
auto-prefixed with the given prefix (symbol, string or nil).

You can reference these names in the given file as usual, but
outside of the file context they are exported as PFX-NAME."
  (unless names (setq names '("")))
  `(eval-when-compile
     ,@(mapcar (lambda (n)
                 `(read-make-prefixed ,n ,pfx)) names)))

;;; READ MACROS
(def-reader-syntax ?#
  ;; TODO 2023-05-04: we can do better here
  (lambda (in ch)
    (let ((x (funcall in)) (num nil))
      (cond
       ((read--digit? x)
        (funcall in x)
        (setq num (read--integer in))
        (setq x (read--peek in))
        (cond ((and read-circle (eq x ?=)) ;; #1=...
               (read--next in)
               (let* ((pho (cons nil nil))
                      (cell (cons num pho)))
                 (setq read-substitutions (cons cell read-substitutions))
                 (let ((tok (read--datum in)))
                   (read--substitute tok pho)
                   (rplacd cell tok))))
              ((and read-circle (eq x ?#)) ;; #1#
               (read--next in)
               (let ((x (assq num read-substitutions)))
                 (if (consp x)
                     (cdr x)
                   (read-error "Cannot find substitution for #%d#" num))))
              ((eq x ?\`)
               (read--next in)
               (read--sbq in num)) ;; #2`(,a1 ,a2)
              ((and (<= num 36)
                    (or (eq x ?r) (eq x ?R)))
               ;; #16rFF
               (read--next in)
               (let* ((base num)
                      (digits (cl-subseq read--all-digits 0 base))
                      (num 0)
                      (neg (cond ((eq ?- (read--peek in))
                                  (read--next in)
                                  t)
                                 ((eq ?+ (read--peek in))
                                  (read--next in)
                                  nil))))
                 (read-while in (lambda (ch)
                                  (let ((v (read--index (downcase ch) digits)))
                                    (when v
                                      (setq num (+ v (* num base)))))))
                 (if neg (- num) num)))
              (t (read-error "Unsupported #%d%c syntax" num x))))
      ;; pass-through supported built-in read syntax to `read-og'
      ((memq x '(?s ?^ ?& ?\[ ?\( ?@ ?! ?$ ?: ?# ?x ?X ?o ?O ?b ?B))
       (funcall in x)
       (funcall in ch)
       (read-og))
      ;; handle `function' short-hand explicitly
      ((eq x ?\')
       (list 'function (read--datum in)))
      ;; CUSTOM SYNTAX
      ((eq x ?\/) ;; #/regexp/
       (read--regexp in))
      ((eq x ?\`)
       (read--sbq in num))
      (t (read-error "Unsupported #%c syntax" x))))))

;; regexp scanner
(defun read--regexp (in)
  (let ((ret (list)))
    (catch 'exit
      (while t
        (let ((ch (funcall in))) ;; /
          (cond
           ((eq ch ?\\)
            (let ((next (funcall in)))
              (cond
               ((memq next '(?\\ ?/ ?\( ?\) ?\| ?\{ ?\}))
                (push next ret))
               ((eq next ?n)
                (push ?\n ret))
               ((eq next ?f)
                (push ?\f ret))
               ((eq next ?t)
                (push ?\t ret))
               (t
                (when (memq next '(?\* ?\+ ?\. ?\? ?\[ ?\] ?\^ ?\$ ?\\))
                  (push ?\\ ret))
                (funcall in next)))))
           ((memq ch '(?\) ?\( ?\| ?\{ ?\}))
            (push ?\\ ret)
            (push ch ret))
           ((eq ch ?/)
            (throw 'exit nil))
           ((not ch)
            (read-error "Unterminated regexp"))
           (t
            (push ch ret))))))
    (apply #'string (nreverse ret))))

(defun read--sbq (in &optional num)
    `(lambda
       ,(cl-loop for z from 1 to (or num 1)
                 collect (symb 'a z))
       ,(list '\` (read--datum in))))

;;  TODO 2023-06-07: 
(def-reader-syntax ?{
    (lambda (in ch)
      (let ((list (read--list in ?} t)))
        `(list ,@(cl-loop for (key val) on list by #'cddr
                          collect `(cons ,key ,val))))))

;;; READERS
(defun rr (&optional in)
  (if (and load-file-name
           (string-match "\\.elc$" load-file-name))
      (funcall *read-og* in)
    (let ((read-filename (read-get-filename)))
      (read--internal (read-make-stream in)))))

(defun rr-from-string (str &optional start end)
  (let ((read-filename (read-get-filename)))
    (let* ((stream (read-make-stream
                    (substring-no-properties str start end)))
           (token (read--internal stream)))
      (cons token (+ (or start 0)
                     (funcall stream :pos))))))

(defun dbg-read (&optional in)
  (message "read history: %s" read-expression-history)
  (funcall *read-og* in))

;; (defmacro def-read-syntax ())
;; (defmacro def-read ()
;;   (setq load-read-function #'dbg-read)
;;  )

(defun install-reader:rr ()
  "Install the `rr' and `rr-from-string' reader functions, replacing `read' and `read-from-string' symbols."
  (progn
    (fset 'read (symbol-function 'rr))
    (fset 'read-from-string (symbol-function 'rr-from-string))
    (setq load-read-function (symbol-function 'rr))))


(defun reset-reader ()
  "Restore the OG values of `read' and `read-from-string'."
  (progn
    (fset 'read *read-og*)
    (fset 'read-from-string *read-from-string-og*)
    (setq load-read-function (symbol-function '*read-og*))))

(provide 'rw-read)
;;; eval.el ends here
