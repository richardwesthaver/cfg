(require :stumpwm)
(in-package :stumpwm)
(setf *default-package* :stumpwm)
(setf *startup-message* nil)
(set-module-dir "/usr/share/stupmwm/contrib/")

(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
(setf *mouse-focus-policy*    :click
      *float-window-modifier* :SUPER)

(load-module "beckon")
(load-module "end-session")
(load-module "globalwindows")
(load-module "mpd")
(load-module "stump-backlight")
(load-module "urgentwindows")
;; (mpd:mpd-connect)
(setf *startup-message* "Greetings, stranger."
      *mode-line-timeout* 2
      *time-modeline-string* "%F %H:%M"
      *group-format* "%t"
      *window-format* "%n: %30t")

;; (load-module "battery-portable")
(load-module "cpu")
(load-module "mem")

(setf cpu::*cpu-modeline-fmt*        "%c"
      cpu::*cpu-usage-modeline-fmt*  "^f2^f0^[~A~2D%^]"
      mem::*mem-modeline-fmt*        "%a%p"
      mpd:*mpd-modeline-fmt*         "%a - %t"
      mpd:*mpd-status-fmt*           "%a - %t"
      *hidden-window-color*          "^**"
      *mode-line-highlight-template* "«~A»")

(defvar phundrak-nord0 "#2e3440")
(defvar phundrak-nord1 "#3b4252")
(defvar phundrak-nord2 "#434c5e")
(defvar phundrak-nord3 "#4c566a")
(defvar phundrak-nord4 "#d8dee9")
(defvar phundrak-nord5 "#e5e9f0")
(defvar phundrak-nord6 "#eceff4")
(defvar phundrak-nord7 "#8fbcbb")
(defvar phundrak-nord8 "#88c0d0")
(defvar phundrak-nord9 "#81a1c1")
(defvar phundrak-nord10 "#5e81ac")
(defvar phundrak-nord11 "#bf616a")
(defvar phundrak-nord12 "#d08770")
(defvar phundrak-nord13 "#ebcb8b")
(defvar phundrak-nord14 "#a3be8c")
(defvar phundrak-nord15 "#b48ead")

(setq *colors*
      `(,phundrak-nord1   ;; 0 black
        ,phundrak-nord11  ;; 1 red
        ,phundrak-nord14  ;; 2 green
        ,phundrak-nord13  ;; 3 yellow
        ,phundrak-nord10  ;; 4 blue
        ,phundrak-nord14  ;; 5 magenta
        ,phundrak-nord8   ;; 6 cyan
        ,phundrak-nord5)) ;; 7 white

(setf *mode-line-background-color* phundrak-nord1
      *mode-line-foreground-color* phundrak-nord5)

(setf *mode-line-border-color* phundrak-nord1
      *mode-line-border-width* 0)

;; (defvar *mode-line-formatter-list*
;;   '(("%g") ("%W") ("^>") ("docker-running" . t) ("mu-unread" . t) ("%m") ("%C") ("%M") ("%B") ("%d"))
;;   "List of formatters for the modeline.")

;; (defun generate-modeline (elements &optional not-invertedp rightp)
;;   "Generate a modeline for StumpWM.
;; ELEMENTS should be a list of `cons'es which `car' is the modeline
;; formatter or the shell command to run, and their `cdr' is either nil
;; when the `car' is a formatter and t when it is a shell command."
;;   (when elements
;;     (cons (format nil
;;                   " ^[~A^]^(:bg \"~A\") "
;;                   (format nil "^(:fg \"~A\")^(:bg \"~A\")^f1~A^f0"
;;                           (if (xor not-invertedp rightp) phundrak-nord1 phundrak-nord3)
;;                           (if (xor not-invertedp rightp) phundrak-nord3 phundrak-nord1)
;;                           (if rightp "" ""))
;;                   (if not-invertedp phundrak-nord3 phundrak-nord1))
;;           (let* ((current-element (car elements))
;;                  (formatter       (car current-element))
;;                  (commandp        (cdr current-element)))
;;             (cons (if commandp
;;                       `(:eval (run-shell-command ,formatter t))
;;                     (format nil "~A" formatter))
;;                   (generate-modeline (cdr elements)
;;                                      (not not-invertedp)
;;                                      (if (string= "^>" (caar elements)) t rightp)))))))

;; (defcommand reload-modeline () ()
;;   "Reload modeline."
;;   (sb-thread:make-thread
;;    (lambda ()
;;      (setf *screen-mode-line-format*
;;            (cdr (generate-modeline *mode-line-formatter-list*))))))

;; (reload-modeline)

(clear-window-placement-rules)

(setf *dynamic-group-master-split-ratio* 1/2)

(set-border-color        phundrak-nord1)
(set-focus-color         phundrak-nord1)
(set-unfocus-color       phundrak-nord3)
(set-float-focus-color   phundrak-nord1)
(set-float-unfocus-color phundrak-nord3)

(set-fg-color phundrak-nord4)
(set-bg-color phundrak-nord1)

(setf *normal-border-width*       0
      *float-window-border*       0
      *float-window-title-height* 15
      *window-border-style*       :none
      *window-format*             "%n:%t")

(setf *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top)

(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 2
      swm-gaps:*outer-gaps-size* 1)

(defcommand firefox () ()
  "Run or raise Firefox."
  (sb-thread:make-thread (lambda () (run-or-raise "firefox" '(:class "Firefox") t nil))))

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
  (update-color-map (current-screen))
  (swm-gaps:toggle-gaps)
  (mode-line))


