(in-package :stumpwm)
(defun show-kernel ()
  (let ((ip (run-shell-command "uname -r" t)))
    (substitute #\Space #\Newline ip)))

(defun show-ip-address ()
  (let ((ip (run-shell-command "ip addr show dev wlan0 | grep 'inet ' | awk '{print $2 }'" t)))
    (substitute #\Space #\Newline ip)))

(defun show-battery-charge ()
  (let ((raw-battery (run-shell-command "acpi | cut -d, -f2" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun show-battery-state ()
  (let ((raw-battery (run-shell-command "acpi | cut -d: -f2 | cut -d, -f1" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun show-hostname ()
  (let ((host-name (run-shell-command "cat /etc/hostname" t)))
    (substitute #\Space #\Newline host-name)))

(defun toggle-mode-line-current ()
  (toggle-mode-line (current-screen) (current-head)))

;; Set model-line format
(setf *screen-mode-line-format*
      (list
       '(:eval (show-hostname))
       " " '(:eval (show-kernel))
       "| IP " '(:eval (show-ip-address))
       "| " '(:eval (run-shell-command "ruby -e \"print Time.now\"" t))
       "| %g"))
