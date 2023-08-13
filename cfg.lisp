(require 'sb-bsd-sockets)

(defpackage :cfg
  (:use :cl :sxp :macs :organ :skel :krypt :uiop :cl-ppcre :sb-bsd-sockets)
  (:export
   :*cfg-version*
   :*cfg-commit*
   :*cfg-source*
   :*cfg-default-mode*
   :config-profile
   :local-config
   :source-config
   :install-config
   :diff-configs
   :edit-config
   :edit-config-with
   :edit-config-file))

(in-package :cfg)

(defparameter *cfg-version* "0.3.1")
(defparameter *cfg-commit*
  (string-right-trim
   #(#\newline)
   (with-output-to-string (s)
     (sb-ext:run-program "hg" (list "id" "-i") :output s :search "hg"))))
(defparameter *cfg-source* (merge-pathnames "dev/cfg" (user-homedir-pathname)))

(defvar *cfg-default-mode* :local
  "Default mode used for selecting targets for cfg commands. Can either
be ':local', indicating that the locally-installed configuration will
be targeted by default, or ':source' indicating that the
version-controlled source will be targeted.

It is possible to support additional modes for remote source or
configurations too (TBD).")


(defclass config-profile (sxp)
  ((name)
   (id)
   (mode)
   (path)))

(defclass local-config (config-profile)
  ((mode :initform :local)))

(defclass source-config (config-profile)
  ((mode :initform :source)))

(defun install-config (&optional profile)
  "Install the configuration PROFILE. Defaults to the `user-base-config'
profile for the current user at runtime.

The argument to this function is optional because we want to support
overrides via process env vars (CFG_PROFILE, CFG_HOOKS, etc). You will
be able to combine both the profile object and env (TBD).")

(defun diff-configs (a b)
  "Perform a diff on configs A and B, reporting any content/structural
changes.")

(defun edit-config (profile &optional mode)
  "Edit the configuration PROFILE. If optional arg MODE is provided,
   use its value to override `*cfg-default-mode*'.")

(defun edit-config-with (path &optional mode)
  "Edit the configuration responsible for the pathname PATH. If optional
arg MODE is provided, use its value to override `*cfg-default-mode*'.")

(defun edit-config-file (file &optional mode)
  "Edit the configuration FILE. If optional arg MODE is provided, use its
value to override `*cfg-default-mode*'.")
