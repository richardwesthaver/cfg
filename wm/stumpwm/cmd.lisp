(in-package :stumpwm)
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
