(defsystem cfg
  :depends-on ("macs" "sxp" "clingon" "organ" "skel" "krypt" "cl-ppcre")
  :version "0.3.1"
  :description "system configuration"
  :components ((:file "cfg")))
