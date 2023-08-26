(require :swank)
(in-package :nyxt-user) ;; let slime know which package we're in

;; by default, JS is not enabled.
(define-configuration web-buffer
  ((default-modes
    (pushnew 'nyxt/mode/no-script:no-script-mode %slot-value%))))

(define-configuration buffer
  ((default-modes
    (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-configuration input-buffer
  ((override-map
    (let ((map (make-keymap "override-map")))
      (define-key map "M-x" 'execute-command)))))

(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'."
  (let ((s-exps-string (cl-ppcre:regex-replace-all
                        ;; Discard the package prefix.
                        "next-user::?"
                        (write-to-string
                         `(progn ,@s-exps) :case :downcase)
                        "")))
    (log:debug "Sending to Emacs: ~s" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

(defvar *kz-keymap* (make-keymap "kz-map")
  "Keymap for `kz-mode'.")

(define-command org-capture (&optional (buffer (current-buffer)))
  "Org-capture current page."
  (eval-in-emacs
   `(org-link-set-parameters
     "next"
     :store (lambda ()
              (org-store-link-props
               :type "next"
               :link ,(url buffer)
               :description ,(title buffer))))
   `(org-capture)))
(define-key *kz-keymap* "C-M-o" 'org-capture)

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (uiop:run-program (list "mpv" (url buffer))))
(define-key *kz-keymap* "C-M-c v" 'play-video-in-current-page)

(define-mode kz-mode ()
  "Dummy mode for the custom key bindings in `*kz-keymap*'."
  ((keymap-scheme (keymap:make-scheme
                   scheme:emacs *kz-keymap*
                   scheme:vi-normal *kz-keymap*))))

(defvar youtube-dl-command "youtube-dl"
  "Path to the 'youtube-dl' program.")

(define-configuration browser
  ((session-restore-prompt :always-restore)))

(setf nyxt/vcs:*vcs-projects-roots* '("~/dev"
                                      "~/lisp"
                                      "~/stash"))

(define-command start-swank (&optional (swank-port 4006))
  (swank:create-server :port swank-port :dont-close t)
  (echo "Swank server started at port ~a" swank-port))

;; Start swank by default.
(unless nyxt::*keep-alive*
  (start-swank))
