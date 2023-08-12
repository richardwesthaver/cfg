;;; -*- emacs-lisp -*-
(require 'ede/proj)

(defclass ede-proj-target-asd (ede-proj-target)
  (sourcetype :initform '(ede-source-asd))
  "Class for Common Lisp ASD project targets.")

(ede-proj-register-target "asd" ede-proj-target-asd)

(defvar ede-source-asd
  (ede-sourcecode "ede-asd-source"
		  :name "ASD"
		  :sourcepattern "\\.asd$"
		  :garbagepattern '("*.asd"))
  "ASD Source Code definition.")

(defvar ede-asd-compiler
  (ede-compiler
   "ede-asd-compiler"
   :name "asd"
   :variables '(("LISP" . "sbcl"))
   :commands
   (list "$(LISP) $@ $^")
   :autoconf nil)
  "Compiler that can be used with `ede-proj-target-asd'.")

(cl-defmethod project-debug-target ((obj ede-proj-target-asd))
  "Run the current target OBJ in a debugger."
  (message "OK"))

(cl-defmethod project-run-target ((obj ede-proj-target-asd))
  "Run the current target OBJ."
  (message "OK"))

(cl-defmethod ede-proj-makefile-sourcevar ((this ede-proj-target-asd))
  "Return the variable name for THIS's sources."
  (concat (ede-pmake-varname this) "_LISP"))

(cl-defmethod ede-proj-makefile-dependencies ((this ede-proj-target-asd))
  "Return a string representing the dependencies for THIS."
  (or (call-next-method)
      (message nil)))

;; This method lets you control what commands are run when a user
;; wants to compile your target.  If you inherit from a makefile
;; target, then you can use "call-next-method" with a new
;; command if needed, or just comment this out.
;;(defmethod project-compile-target ((obj ede-proj-target-%NAME%)
;;				     &optional command)
;;  "Compile the current target OBJ.
;;Argument COMMAND is the command to use for compiling the target
;;if the user, or child class wishes to modify it."
;;  (or (call-next-method %"new command string"%)
;;      (project-compile-project (ede-current-project) %"some command"%)
;;	(%do-stuff%)))

;; Your main target in the Makefile may depend on additional source
;; dependencies.  Use this to add more stuff.
;;(defmethod ede-proj-makefile-dependency-files ((this ede-proj-target-%NAME%))
;;  "Return a list of source files to convert to dependencies.
;;Argument THIS is the target to get sources from."
;;  (append (call-next-method) (%get-more-dependencies%)))

;; This is a clever way of packing more files into your main source
;; variable.  Only works if your "next" method is ede-proj-target.
;;(defmethod ede-proj-makefile-insert-source-variables
;;           ((this ede-proj-target-%NAME%) &optional moresource)
;;  "Insert variables needed by target THIS."
;;  (call-next-method this (oref this %moresourceslotname%))

;; This method lets you add more variables specific to your type of target.
;;(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-%NAME%)
;;                                                &optional moresource)
;;  "Insert variables needed by target THIS."
;;  (call-next-method)
;;  (insert "other variable thing"))

;; This is one of the most important methods which defines rules to
;; place into a makefile for building.  If you inherit from
;; `ede-proj-target-makefile', then this is the primary build
;; mechanism.  If you can create a compiler object instead, then
;; you probably don't have to do anything with this method.
;; If you have an emacs-centric build method, then this
;; is a secondary build method (for a distribution, for example.)
;; It can also contain auxiliary make commands in addition to
;; the main rules needed if not covered by the compiler object.
;;(defmethod ede-proj-makefile-insert-rules ((this ede-proj-target-%NAME%))
;;  "Insert rules needed by THIS target."
;;  (insert ... ;; Create aux rules here
;;        )
;;  (call-next-method) ;; catch user-rules case and build main rules
;;  (insert ... ;; Code to fill in the commands, or add more rules later.
;;	  ))

;; This function is used to find a header file in which prototypes from
;; BUFFER go.  This is used by advanced features for which this type
;; of behavior is useful.  This feature is used mainly by tools
;; using the SEMANTIC BOVINATOR http://cedet.sourceforge.net/semantic.shtml
;; to perform advanced language specific actions.
;;(defmethod ede-buffer-header-file((this ede-proj-target-%NAME%) buffer)
;;  "Return the name of a file in which prototypes go."
;;  (oref this ...))

;; This function is used to return documentation files.  If this target
;; contains documentation files, then return those files.  If this target
;; does not provide documentation, delete this method.
;;(defmethod ede-documentation ((this ede-target-%NAME%))
;;  "Return a list of files that provides documentation.
;;Documentation is not for object THIS, but is provided by THIS for other
;;files in the project."
;;  nil)

;; In general, none of these need to be defined unless your have slots
;; for auxiliary source files.

;; This lets you add buttons of things your target contains which may
;; not be shown be default.
;;
;; You will need to tweak the functions used when clicking on the
;; expand icon (maybe) and the item name (maybe). Leave those alone
;; if they are simple source files.
;;(defmethod eieio-speedbar-child-make-tag-lines ((this ede-proj-target-%NAME%))
;;  "Create buttons for items belonging to THIS."
;;  (call-next-method) ;; get the default buttons inserted.
;;  (with-slots (%SOME-SLOTS%) this
;;    (mapcar (lambda (car)
;;		(speedbar-make-tag-line 'bracket ?+
;;					'ede-tag-file
;;					(concat (oref this :path) car)
;;					car
;;					'ede-file-find
;;					(concat (oref this :path) car)
;;					'speedbar-file-face depth))
;;	      %A-SLOT%)))

(provide 'ede/proj-asd)
