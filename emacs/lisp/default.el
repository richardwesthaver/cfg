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

;;; Customization
(defgroup default nil
  "default settings")

(defcustom user-website-url "https://rwest.io/"
  "default website homepage. don't forget the slash!"
  :group 'default)

;;; Functions
(defun add-packages (&rest pkgs)
  "add list of packages PKGS to `package-selected-packages'"
  (mapcar #'(lambda (x) (add-to-list 'package-selected-packages x)) (flatten-list pkgs)))

;;; Keys
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c L") #'org-insert-link-global)
(global-set-key (kbd "C-c o") #'org-open-at-point-global)

(define-minor-mode keys
  "Global minor mode containing useful keybinds."
  :lighter " Kz"
  :global t
  :group 'default
  :keymap `(
	    ;; Outlines
	    (,(kbd "M-TAB") . outline-cycle)
	    (,(kbd "M-n") . outline-next-visible-heading)
	    (,(kbd "M-p") . outline-previous-visible-heading)
	    ;; Speedbar  
	    (,(kbd "C-c c s") . speedbar)
	    ;; Embark
	    (,(kbd "C-c a") . embark-act)
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
	    ;; Modes
	    (,(kbd "C-c m v") . global-visual-line-mode)
	    (,(kbd "C-c m h") . global-hl-line-mode)
	    (,(kbd "C-c m l") . display-line-numbers-mode)
	    (,(kbd "C-c m L") . global-display-line-numbers-mode)
	    (,(kbd "C-c m a") . gpm-mouse-mode)
	    (,(kbd "C-c m r") . refill-mode)
	    (,(kbd "C-c m R") . global-auto-revert-mode)
	    (,(kbd "C-c m t") . toggle-frame-tab-bar)
	    (,(kbd "C-c m d") . toggle-debug-on-error)
	    ;; Etc
	    (,(kbd "C-c e w") . eww)
	    (,(kbd "C-c e C-w") . webjump)
	    (,(kbd "C-c e W") . browse-url)
	    (,(kbd "C-c e i") . ielm)
	    (,(kbd "C-c e f") . load-file)
	    (,(kbd "C-c e l") . load-library) 
	    (,(kbd "C-c e k") . server-shutdown)
	    ;; Fun & Games
	    (,(kbd "C-c e g t") . tetris)
	    (,(kbd "C-c e g z") . zone)
	    (,(kbd "C-c e g s") . snake)))


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
		epa-pinentry-mode 'loopback
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
		url-privacy-level '(email agent cookies lastloc)
		tramp-default-method "sshx")

  (when (string= system-type "darwin")       
    (setq-default dired-use-ls-dired nil)))

(add-hook 'after-init-hook #'keys)

(provide 'default)
;;; default.el ends here
