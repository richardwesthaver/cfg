;#!/usr/bin/emacs --script
(require 'comp)
(require 'f)
(require 'dash)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local suspicious callargs redefine))

(defun was-byte-compiled-p (path)
  "Does the directory at PATH contain any .elc files?"
  (--any-p (f-ext? it "elc") (f-files path)))

(defun ensure-directory-compiled (path)
  "If any packages in a directory aren't compiled yet, compile them."
  (--each (f-directories path)
    (unless (was-byte-compiled-p it)
	    (byte-recompile-directory it 0))))
