;;; shed.el --- Shed Emacs Package -*- lexical-binding: t; -*-
;; Copyright (C) 2021  ellis
;; 
;; Author: ellis
;; Keywords: local, vc, net, process
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; 
;; Commentary:
;; 
;; This package provides functions for interacting with the local Shed
;; development system.
;; 
;;; Code:
;;;; Custom 
(defgroup shed nil
  "shed Emacs Modules")

(defcustom shed-dir "~/shed/" "shed directory."
  :group 'shed)

(defcustom server-after-make-frame-hook nil
  "Hook run when the shed server creates a client frame.
The created frame is selected when the hook is called."
  :type 'hook
  :version "27.1"
  :group 'shed)

(defcustom server-done-hook nil
  "Hook run when done editing a buffer for the shed server."
  :type 'hook
  :group 'shed)

(defvar shed-data-dir (file-name-as-directory (expand-file-name "data" shed-dir))
  "shed data directory.")
(defvar shed-src-dir (file-name-as-directory (expand-file-name "src" shed-dir))
  "shed src directory.")
(defvar shed-stash-dir (file-name-as-directory (expand-file-name "stash" shed-dir))
  "shed stash directory.")
(defvar shed-store-dir (file-name-as-directory (expand-file-name "store" shed-dir))
  "shed store directory.")
(defvar shed-lab-dir (file-name-as-directory (expand-file-name "lab" shed-dir))
  "shed lab directory.")

(defvar shed-server-process nil
  "The shed-server process handle.")

(defvar shed-server-clients nil
  "List of current server clients.
Each element is a process.")

(defvar shed-cmd-server-port 62824
  "port of the shed-status broadcaster")

(defvar shed-cmd-server-clients '() 
  "alist where KEY is a client process and VALUE is the string")

;;;; Bindat
(setq shed-header-bindat-spec
      '((dest-ip   ip)
        (dest-port u16)
        (src-ip    ip)
        (src-port  u16)))

(setq shed-body-bindat-spec
      '((type      u8)
        (opcode    u8)
        (length    u16)  ; network byte order
        (id        strz 8)
        (data      vec (length))
        (align     4)))

(setq shed-packet-bindat-spec
      '((header    struct header-spec)
        (counters  vec 2 u32r)   ; little endian order
        (items     u8)
        (fill      3)
        (item      repeat (items)
                   (struct data-spec))))

(defun shed-insert-string (string)
  (insert string 0 (make-string (- 3 (% (length string) 4)) 0)))

(defun shed-insert-int32 (value)
  (let (bytes)
    (dotimes (i 4)
      (push (% value 256) bytes)
      (setq value (/ value 256)))
    (dolist (byte bytes)
      (insert byte))))

(defun shed-insert-float32 (value)
  (let (s (e 0) f)
    (cond
     ((string= (format "%f" value) (format "%f" -0.0))
      (setq s 1 f 0))
     ((string= (format "%f" value) (format "%f" 0.0))
      (setq s 0 f 0))
     ((= value 1.0e+INF)
      (setq s 0 e 255 f (1- (expt 2 23))))
     ((= value -1.0e+INF)
      (setq s 1 e 255 f (1- (expt 2 23))))
     ((string= (format "%f" value) (format "%f" 0.0e+NaN))
      (setq s 0 e 255 f 1))
     (t
      (setq s (if (>= value 0.0)
		  (progn (setq f value) 0)
		(setq f (* -1 value)) 1))
      (while (>= (* f (expt 2.0 e)) 2.0) (setq e (1- e)))
      (if (= e 0) (while (< (* f (expt 2.0 e)) 1.0) (setq e (1+ e))))
      (setq f (round (* (1- (* f (expt 2.0 e))) (expt 2 23)))
	    e (+ (* -1 e) 127))))
    (insert (+ (lsh s 7) (lsh (logand e #XFE) -1))
	    (+ (lsh (logand e #X01) 7) (lsh (logand f #X7F0000) -16))
	    (lsh (logand f #XFF00) -8)
	    (logand f #XFF))))

(defun shed-read-string ()
  (let ((pos (point)) string)
    (while (not (= (following-char) 0)) (forward-char 1))
    (setq string (buffer-substring-no-properties pos (point)))
    (forward-char (- 4 (% (length string) 4)))
    string))

(defun shed-read-int32 ()
  (let ((value 0))
    (dotimes (i 4)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun shed-read-float32 ()
  (let ((s (lsh (logand (following-char) #X80) -7))
	(e (+ (lsh (logand (following-char) #X7F) 1)
	      (lsh (logand (progn (forward-char) (following-char)) #X80) -7)))
	(f (+ (lsh (logand (following-char) #X7F) 16)
	      (lsh (progn (forward-char) (following-char)) 8)
	      (prog1 (progn (forward-char) (following-char)) (forward-char)))))
    (cond
     ((and (= e 0) (= f 0))
      (* 0.0 (expt -1 s))
      ((and (= e 255) (or (= f (1- (expt 2 23))) (= f 0)))
       (* 1.0e+INF (expt -1 s)))
      ((and (= e 255) (not (or (= f 0) (= f (1- (expt 2 23))))))
       0.0e+NaN)
      (t
       (* (expt -1 s)
	  (expt 2.0 (- e 127))
	  (1+ (/ f (expt 2.0 23)))))))))

;;;; Network
;;;###autoload
(defun net-check-opts ()
  ;; https://gnu.huihoo.org/emacs/24.4/emacs-lisp/Network-Options.html#Network-Options
  ;; non-blocking
  (featurep 'make-network-process '(:nowait t))
  ;; UNIX socket
					;(featurep 'make-network-process '(:family local))
  ;; UDP
  (featurep 'make-network-process '(:type datagram)))

;;;; Process
(defun shed-make-client (host port)
  (make-network-process
   :name "shed-cmd-client"
   :coding 'binary
   :host host
   :service port
   :type 'datagram
   :nowait t))

(defun shed-cmd-server-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq shed-cmd-server-clients (assq-delete-all proc shed-cmd-server-clients))
    (shed-cmd-server-log (format "client %s has quit" proc))))

;;from server.el
;;;###autoload
(defun shed-cmd-server-log (string &optional client)
  "If a *shed-cmd-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*shed-cmd-server*")
      (with-current-buffer "*shed-cmd-server*"
        (goto-char (point-max))
        (insert (if client (format "<%s>: " (format-network-address (process-datagram-address client))))
                string)
        (or (bolp) (newline)))))

(defun shed-cmd-server-start nil
  "start a shed-cmd-server over udp"
  (interactive)
  (unless (process-status "shed-cmd-server")
    (make-network-process :name "shed-cmd-server"
			  :buffer "*shed-cmd-server*"
			  :family 'ipv4
			  :service shed-cmd-server-port
			  :type 'datagram
			  :coding 'binary
			  :sentinel 'shed-cmd-server-sentinel
			  :filter 'shed-cmd-server-filter
			  :server t
			  :broadcast t) 
    (setq shed-cmd-server-clients '())

    ;; setup additional filters
    (add-function :after (process-filter (get-process "shed-cmd-server")) #'shed-babel-response-filter))
  (message "shed-cmd-server: ONLINE"))

(defun shed-cmd-server-stop ()
  "stop a shed-cmd-server"
  (interactive)
  (while  shed-cmd-server-clients
    (delete-process (car (car shed-cmd-server-clients)))
    (setq shed-cmd-server-clients (cdr shed-cmd-server-clients)))
  (with-current-buffer "*shed-cmd-server*"
    (delete-process (get-buffer-process (current-buffer)))
    (set-buffer-modified-p nil)
    (kill-this-buffer)))

(defun shed-cmd-server-filter (proc string)   
  (let ((pending (assoc proc shed-cmd-server-clients))
        message
        index)
    ;;create entry if required
    (unless pending
      (setq shed-cmd-server-clients (cons (cons proc "") shed-cmd-server-clients))
      (setq pending  (assoc proc shed-cmd-server-clients)))
    (setq message (concat (cdr pending) string))
    (while (setq index (string-match "\n" message))
      (setq index (1+ index))
;      (process-send-string proc (substring message 0 index))
      (shed-cmd-server-log  (substring message 0 index) proc)
      (setq message (substring message index)))
    (setcdr pending message)))

(defun shed-cmd-packet-filter (proc string)
  "process-filter for decoding 'shed-packet-bindat-spec'"
  (bindat-unpack packet-spec string))

(defun ordinary-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))

        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun shed-babel-response-filter (proc string)
  "match STRING from PROC against 'org-babel-library-of-babel' functions."
  (let ((msg (car (read-from-string string)))
	(status))
    (if (assoc msg org-babel-library-of-babel)
	(progn 
	  (setq status "OK")
	  (shed-cmd-server-log (format "BABEL_CMD:%s" status) proc)
	  (process-send-string proc (concat "\n" (eval (car (read-from-string
							     (format "(org-sbx %s)" string)))) "\n\n")))
      (progn
	(setq status "ERR")
	(shed-cmd-server-log (format "BABEL_CMD:%s" status) proc)))))

;;;; Signals
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun signal-restart-server ()
  "Handler for SIGUSR1 signal, to (re)start an emacs server.

Can be tested from within emacs with:
  (signal-process (emacs-pid) 'sigusr1)

or from the command line with:
$ kill -USR1 <emacs-pid>
$ emacsclient -c
"
  (interactive)
  (server-force-delete)
  (server-start)
  )

(define-key special-event-map [sigusr1] 'signal-restart-server)


;;;; Shells 
;;;;; Python
(setq python-shell-interpreter "shc"
      python-shell-interpreter-interactive-arg "x py"
      python-shell-interpreter-args "x py"
      python-shell-prompt-detect-failure-warning nil
      python-shell-prompt-regexp ">>>>>")
(setq python-shell-completion-native-disabled-interpreters "shc")

;;;; provide
(provide 'shed)
;;; shed.el ends here
