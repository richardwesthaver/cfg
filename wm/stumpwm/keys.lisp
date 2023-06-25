(in-package :stumpwm)
(set-prefix-key (kbd "s-;"))
(defvar *rofi-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "r") "exec rofi -combi-modi drun,window -show combi")
    (define-key m (kbd "s") "exec rofi -show ssh")
    (define-key m (kbd "p") "exec rofi-pass -t")
    (define-key m (kbd "P") "exec rofi-pass")
    (define-key m (kbd "e") "exec rofi-emoji")
    (define-key m (kbd "m") "exec rofi-mount")
    (define-key m (kbd "u") "exec rofi-umount")
    (define-key m (kbd "w") "exec wacom-setup")
    (define-key m (kbd "y") "exec ytplay")
    (define-key m (kbd "Y") "exec rofi-ytdl")
    m))

(defvar *scrot-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "d") "exec flameshot gui -d 3000")
    (define-key m (kbd "s") "exec flameshot full")
    (define-key m (kbd "S") "exec flameshot gui")
    m))

(define-key *top-map* (kbd "s-S") '*scrot-keymap*)
(define-key *top-map* (kbd "s-RET") "term")
(define-key *top-map* (kbd "s-r") '*rofi-keymap*)
(define-key *top-map* (kbd "s-e") "exec emacsclient -c .")

(define-key *root-map* (kbd ";") "colon")

(which-key-mode)
