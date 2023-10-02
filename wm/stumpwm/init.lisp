(require :stumpwm)
(in-package :stumpwm)
(setf *default-package* :stumpwm)
(setf *startup-message* nil)
(set-module-dir "/usr/share/stupmwm/contrib/")
(require :swank)
(swank-loader:init)
;; (swank:create-server :port 4004
;;                      :style swank:*communication-style*
;;                      :dont-close nil)
(setf *mouse-focus-policy*    :click
      *float-window-modifier* :SUPER
      *startup-message* "Greetings, stranger."
      *shell-program* (getenv "SHELL")
      *mode-line-border-width* 0
      *screen-mode-line-format*
      (list "%w | "
            '(:eval (run-shell-command "date" t))))


(clear-window-placement-rules)

(setf *dynamic-group-master-split-ratio* 1/2
      *normal-border-width*       0
      *float-window-border*       0
      *float-window-title-height* 15
      *window-border-style*       :none
      *window-format*             "%n:%t"
      *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top)

;; (load-module "swm-gaps")

;; (setf swm-gaps:*head-gaps-size*  0
;;       swm-gaps:*inner-gaps-size* 2
;;       swm-gaps:*outer-gaps-size* 1)

(set-font "CommitMono")

(setf *colors* (list "#1C2028"      ; 0 black
                     "#BF616A"      ; 1 red
                     "#A3BE8C"      ; 2 green
                     "#EBCB8B"      ; 3 yellow
                     "#5E81AC"      ; 4 blue
                     "#B48EAD"      ; 5 magenta
                     "#8FBCBB"      ; 6 cyan
                     "#ECEFF4"))    ; 7 white

(load #p"~/.stumpwm.d/cmd.lisp")
(load #p"~/.stumpwm.d/keys.lisp")
(load #p"~/.stumpwm.d/modeline.lisp")
(when *initializing*
  (run-shell-command "/usr/bin/dunst")
  (run-shell-command "sh ~/bin/sh/zor-screenlayout-default.sh")
  (run-shell-command "sh ~/.fehbg")
  (update-color-map (current-screen)))

(dolist (i *screen-list*)
  (toggle-mode-line-current)
  (snext))
