(require 'sb-bsd-sockets)
(defpackage :cfg
  (:use :cl :sxp :macs :organ :skel :krypt :uiop)
  (:export))

(in-package :cfg)

(defparameter *cfg-version* "0.3.1")
(defparameter *cfg-commit* (uiop:run-program "hg id -i" :output :interactive))
