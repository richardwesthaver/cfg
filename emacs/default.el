;;; default.el --- default settings -*- lexical-binding: t -*-
;; 
;; Copyright (C) 2021  ellis
;; 
;; Author: ellis <ellis@rwest.io>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28"))
;; Keywords: convenience
;; 
;;; Commentary:
;; 
;; This package is used to set default values for a vanilla Emacs 28+
;; installation on any supported platform. It should be loaded before
;; any user-specific configuration in `init.el`. This package doesn't
;; have any external dependencies and isn't depended on by other
;; packages.
;; 
;;; Code:
(eval-when-compile (require 'cl-lib))
;; (require 'package-x)
;;;; Custom
(defgroup default nil
  "default settings")

(defcustom user-website-url "https://rwest.io/"
  "default website homepage. don't forget the slash!"
  :group 'default)

;;;; Macros
(defmacro hook-modes (modes &rest body)
  (declare (indent 1))
  `(--each ,modes
     (add-hook (intern (format "%s-hook" it))
               (lambda () ,@body))))

(defmacro define-lambda-choice (name &rest choices)
  "Define a chooser command NAME offering CHOICES.
Each of CHOICES should be a list, the first of which is the
choice's name, and the rest of which is its body forms."
  (declare (indent defun))
  ;; Avoid redefining existing, non-chooser functions.
  (cl-assert (or (not (fboundp name))
                 (get name :define-lambda-choice)))
  (let* ((choice-names (mapcar #'car choices))
         (choice-list (--map (cons (car it) #'(lambda (&rest args)
                                               ,@(cdr it)))
                             choices))
         (prompt (format "Choose %s: " name))
         (docstring (concat "Choose between: " (s-join ", " choice-names))))
    `(progn
       (defun ,name ()
         ,docstring
         (interactive)
         (let* ((choice-name (completing-read ,prompt ',choice-names)))
           (funcall (alist-get choice-name ',choice-list nil nil #'equal))))
       (put ',name :define-lambda-choice t))))


;;; Keys
(define-minor-mode keys
  "Global minor mode containing useful keybinds."
  :lighter " Kz"
  :global t
  :group 'default
  :keymap `(
	    ;; Viper
	    (,(kbd "C-c SPC") . toggle-viper-mode)
	    ;; Registers
	    (,(kbd "C-c M-y") . copy-to-register)
	    (,(kbd"C-c M-j") . jump-to-register)
	    (,(kbd"C-c M-f") . frameset-to-register)
	    (,(kbd"C-c M-SPC") . point-to-register)
	    ;; Outlines
	    (,(kbd "M-TAB") . outline-cycle)
	    (,(kbd "M-n") . outline-next-visible-heading)
	    (,(kbd "M-p") . outline-previous-visible-heading)
	    ;; Windows
	    (,(kbd "C-x -") . split-window-vertically)
	    (,(kbd "C-x =") . split-window-horizontally)
	    ;; Speedbar  
	    (,(kbd "C-c c s") . speedbar)
	    ;; Embark
	    (,(kbd "C-c a") . embark-act)
	    ;; Version Control
	    (,(kbd "C-c v v") . vc-next-action)
	    (,(kbd "C-c v .") . vc-dir)
	    ;; Shell
	    (,(kbd "C-c x x") . async-shell-command)
	    (,(kbd "C-c x SPC") . eshell)
	    ;; Search
	    (,(kbd "M-s w") . search-web)
	    (,(kbd "M-s r") . rg)
	    (,(kbd "C-c s R") . rg-dwim-current-dir)
	    ;; UI
	    (,(kbd "C-c w b") . balance-windows)
	    (,(kbd "C-c w i") . enlarge-window)
	    (,(kbd "C-c w j") . shrink-window-horizontally)
	    (,(kbd "C-c w k") . shrink-window)
	    (,(kbd "C-c w l") . enlarge-window-horizontally)
	    (,(kbd "C-c w s") . switch-window-then-swap-buffer)
	    (,(kbd "C-c w t") . toggle-theme)
	    (,(kbd "C-c w T") . modus-themes-toggle)
	    (,(kbd "C-c w w") . winum-select-window-by-number)
	    ;; Org
	    (,(kbd "C-c n c") . org-capture)
	    (,(kbd "C-c n a") . org-agenda)
	    (,(kbd "C-c n t t") . org-timer)
	    (,(kbd "C-c n t p") . org-timer-pause-or-continue)
	    (,(kbd "C-c n t s") . org-timer-set-timer)
	    (,(kbd "C-c n t t") . org-timer-show-remaining-time)
	    (,(kbd "C-c n 1") . org-timer-start)
	    (,(kbd "C-c n 2") . org-timer-stop)
	    (,(kbd "C-c l") . org-store-link)
	    ;; Org Web Tools
	    (,(kbd "C-c n l") . org-web-tools-insert-link-for-url)
	    (,(kbd "C-c n w") . org-web-tools-read-url-as-org)
	    (,(kbd "C-c n L") . org-web-tools-insert-web-page-as-entry)
	    (,(kbd "C-c n C-l") . org-web-tools-convert-links-to-page-entries)
	    ;; Org Clock
	    ;; (,(kbd "C-c x c") . org-clock-cancel)
	    ;; (,(kbd "C-c x d") . org-clock-display)
	    ;; (,(kbd "C-c x e") . org-clock-modify-effort-estimate)
	    ;; (,(kbd "C-c x i") . org-clock-in)
	    ;; (,(kbd "C-c x j") . org-clock-goto)
	    ;; (,(kbd "C-c x o") . org-clock-out)
	    ;; (,(kbd "C-c x r") . org-clock-report)
	    (,(kbd "C-c i") . new-scratch)
	    ;; Modes
	    (,(kbd "C-c m v") . global-visual-line-mode)
	    (,(kbd "C-c m h") . global-hl-line-mode)
	    (,(kbd "C-c m l") . display-line-numbers-mode)
	    (,(kbd "C-c m L") . global-display-line-numbers-mode)
	    (,(kbd "C-c m a") . gpm-mouse-mode)
	    (,(kbd "C-c m r") . refill-mode)
	    (,(kbd "C-c m k") . which-key-mode)
	    (,(kbd "C-c m R") . global-auto-revert-mode)
	    (,(kbd "C-c m t") . toggle-frame-tab-bar)
	    (,(kbd "C-c m d") . toggle-debug-on-error)
	    ;; Etc
	    (,(kbd "C-c e w") . eww)
	    (,(kbd "C-c e C-w") . webjump)
	    (,(kbd "C-c e W") . browse-url)
	    (,(kbd "C-c e C-e") . notmuch-exec-offlineimap)
	    (,(kbd "C-c e i") . ielm)
	    (,(kbd "C-c e f") . load-file)
	    (,(kbd "C-c e l") . load-library) 
	    (,(kbd "C-c e k") . server-shutdown)
	    ;; Fun & Games
	    (,(kbd "C-c e g t") . tetris)
	    (,(kbd "C-c e g z") . zone)
	    (,(kbd "C-c e g s") . snake)))

;;;; Utils
;;;;; Regex
(defvar base-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	 (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.")

(defvar default-line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `default-line-regexp-p'.")

(defun default-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`default-line-regexp-alist'.  It matches a regular
expression.
With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type default-line-regexp-alist))))))

;;;;; Helpers
;;;###autoload
(defun random-integers (min max n)
  "Return N random integers between MIN (inclusive) and MAX (exclusive)."
  (let ((list ()))
    (dotimes (_ n)
      (push (+ (cl-random (- max min)) min) list))
    list))

;;;###autoload
(defun os-path-join (a &rest ps)
  (let ((path a))
    (while ps
      (let ((p (pop ps)))
        (cond ((string-prefix-p "/" p)
               (setq path p))
              ((or (not path) (string-suffix-p "/" p))
               (setq path (concat path p)))
              (t (setq path (concat path "/" p))))))
    path))

;;;###autoload
(defun int-to-binary-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

;;;###autoload
(defun read-elisp-data (file)
  "Read Elisp data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path"
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defun kill-ring-car-espace-quotes ()
  "Escape doublequotes in car of kill-ring "
  (interactive)
  (with-temp-buffer
    (insert (car kill-ring))
    (goto-char (point-min))
    (while (search-forward "\"" nil t 1)
      (replace-match "\\\\\""))
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Setup
;;;###autoload
(defun default-setup ()
  "Setup defaults"
  ;; set defaults
  (setq-default make-backup-files nil
		auto-save-list-file-prefix (expand-file-name "auto-save/." user-emacs-directory)
		tramp-auto-save-directory (expand-file-name "auto-save/tramp/" user-emacs-directory)
		confirm-kill-emacs nil
		confirm-kill-process nil
		use-short-answers t
		display-time-format "%Y-%m-%d--%H:%M"
		ring-bell-function 'ignore
		gc-cons-percentage 0.6
		completion-ignore-case t
		;; org
		org-agenda-files (list org-directory)
		shr-use-colors nil
		shr-use-fonts nil
		shr-max-image-proportion 0.6
		shr-image-animate nil
		shr-discard-aria-hidden t
		bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
		project-list-file (expand-file-name "projects" user-emacs-directory)
		emms-directory (expand-file-name "emms" user-emacs-directory)
		gnus-cache-directory (expand-file-name "gnus" user-emacs-directory)
		url-cache-directory (expand-file-name "url" user-emacs-directory)
		tab-always-indent 'complete
		shr-cookie-policy nil
		browse-url-browser-function 'browse-url-default-browser
		eww-search-prefix "https://duckduckgo.com/html?q="
		url-privacy-level '(email agent cookies lastloc)))

(add-hook 'after-init-hook 'keys)

(provide 'default)
;;; default.el ends here
