(defsystem cfg
  :depends-on (:macs :sxp :organ :skel :krypt :cl-ppcre)
  :version "0.3.1"
  :description "system configuration"
  :components ((:file "cfg")))
