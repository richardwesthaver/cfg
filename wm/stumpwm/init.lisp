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
(stumpwm:set-font
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

(defcommand firefox () ()
  "Run or raise Firefox."
  (sb-thread:make-thread (lambda () (run-or-raise "firefox" '(:class "Firefox") t nil))))

(defcommand chromium () ()
  "Run or raise Firefox."
  (sb-thread:make-thread (lambda () (run-or-raise "chromium" '(:class "Chromium") t nil))))

(defcommand nyxt () ()
  "Run or raise Nyxt."
  (sb-thread:make-thread (lambda () (run-or-raise "nyxt" '(:class "Nyxt") t nil))))

(defcommand delete-window-and-frame () ()
  "Delete the current frame with its window."
  (delete-window)
  (remove-split))

(defcommand hsplit-and-focus () ()
  "Create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "Create a new frame below and move focus to it."
  (vsplit)
  (move-focus :down))

(defcommand term (&optional program) ()
  "Invoke a terminal, possibly with a @arg{program}."
  (sb-thread:make-thread
   (lambda ()
     (run-shell-command (if program
                            (format nil "kitty ~A" program)
                            "kitty")))))

(load #p"~/.stumpwm.d/keys.lisp")
(when *initializing*
  (run-shell-command "zor-screen-init.sh")
  (run-shell-command "/usr/bin/dunst")
  (mode-line)
  (update-color-map (current-screen)))
