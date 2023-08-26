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
      *float-window-modifier* :SUPER)
;; (mpd:mpd-connect)
(setf *startup-message* "Greetings, stranger.")
(setf *mode-line-border-width* 0)

(setf stumpwm:*screen-mode-line-format*
      (list "%w | "
            '(:eval (stumpwm:run-shell-command "date" t))))

(clear-window-placement-rules)

(setf *dynamic-group-master-split-ratio* 1/2)

(setf *normal-border-width*       0
      *float-window-border*       0
      *float-window-title-height* 15
      *window-border-style*       :none
      *window-format*             "%n:%t")

(setf *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top)

;; (load-module "swm-gaps")

;; (setf swm-gaps:*head-gaps-size*  0
;;       swm-gaps:*inner-gaps-size* 2
;;       swm-gaps:*outer-gaps-size* 1)
(set-font "CommitMono")

(load #p"~/.stumpwm.d/cmd.lisp")
(load #p"~/.stumpwm.d/keys.lisp")
(load #p"~/.stumpwm.d/modeline.lisp")
(when *initializing*
  (run-shell-command "/usr/bin/dunst")
  (update-color-map (current-screen)))
(dolist (i *screen-list*)
  (toggle-mode-line-current)
  (snext))
