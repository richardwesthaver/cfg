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
;; This package is used to set default values for a vanilla Emacs28
;; installation on any supported platform. It should be loaded before
;; any user-specific configuration in `init.el`. This package doesn't
;; have any external dependencies and isn't depended on by other
;; packages.
;; 
;;; Code:
(eval-when-compile (require 'cl-lib))
;;;; Startup 
(defun cleanup-gc ()
  "Perform garbage collection with a threshold of 64M and and default `gc-cons-percentage` (0.1)."
  (setq gc-cons-threshold  67108864)	;64M
  (setq gc-cons-percentage 0.1)		;original value
  (garbage-collect))

;;;###autoload
(defun default-setup ()
  "Setup defaults"
  ;; default settings
  (setq make-backup-files nil
	confirm-kill-emacs nil
	confirm-kill-process nil
	use-short-answers t
	display-time-format "%Y-%m-%d : %H:%M"
	ring-bell-function 'ignore
	gc-cons-percentage 0.6
	gc-cons-threshold most-positive-fixnum
	;; Completion
	completion-styles '(basic partial-completion emacs22)
	completion-ignore-case t
	;; Web
	shr-use-colors nil
	shr-use-fonts nil
	shr-max-image-proportion 0.6
	shr-image-animate nil
	shr-discard-aria-hidden t
	shr-cookie-policy nil
	browse-url-browser-function 'eww-browse-url
	eww-search-prefix "https://duckduckgo.com/html?q="
	url-privacy-level '(email agent cookies lastloc))

  ;; Hooks
  (add-hook 'after-init-hook (lambda () (run-with-idle-timer 4 nil #'cleanup-gc)))
  (add-hook 'after-init-hook 'default-keys-mode)
  (add-hook 'term-exec-hook 'set-no-process-query-on-exit)
  (add-hook 'shell-mode-hook 'set-no-process-query-on-exit))

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
         (choice-list (--map (cons (car it) `(lambda (&rest args)
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


;;;; Scratch
(defcustom default-scratch-buffer-mode 'lisp-interaction-mode
    "Default major mode for new scratch buffers"
    :group 'default)

;; Adapted from the `scratch.el' package by Ian Eure.
(defun default-scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           for name = (symbol-name sym)
           when (and (functionp sym)
                     (not (member sym minor-mode-list))
                     (string-match "-mode$" name)
                     (not (string-match "--" name)))
           collect name))

(defun default-scratch-buffer-setup (region &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
         (string (format "Scratch buffer for: %s\n\n" major))
         (text (concat string region))
         (buf (format "*Scratch for %s*" major)))
    (with-current-buffer (get-buffer-create buf)
      (funcall major)
	  (save-excursion
        (insert text)
        (goto-char (point-min))
        (comment-region (point-at-bol) (point-at-eol)))
	  (vertical-motion 2))
    (pop-to-buffer buf)))

;;;###autoload
(defun default-scratch-buffer (&optional arg)
  "Produce a bespoke scratch buffer matching current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `default-scratch-buffer-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.

If region is active, copy its contents to the new scratch
buffer."
  (interactive "P")
  (let* ((default-mode default-scratch-buffer-mode)
         (modes (default-scratch-list-modes))
         (region (with-current-buffer (current-buffer)
                   (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     "")))
         (m))
    (pcase (prefix-numeric-value arg)
      (16 (progn
            (setq m (intern (completing-read "Select major mode: " modes nil t)))
            (default-scratch-buffer-setup region m)))
      (4 (default-scratch-buffer-setup region default-mode))
      (_ (default-scratch-buffer-setup region)))))

;;;###autoload
(defun new-scratch ()
  "create a new scratch buffer. (could be *scratch* - *scratchN*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname
		   (concat "*scratch"
                           (if (= n 0) "" (int-to-string n))
                           "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (insert initial-scratch-message)
    (lisp-interaction-mode)))

;;;; VC
;;;; Org 
(defgroup default-org ()
  "Default Org-mode extensions"
  :group 'default)

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path"
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defvar org-dir (expand-file-name org-directory)
  "custom directory for user org files")
;;;; Programming
(defgroup default-prog ()
          "basic programming extensions"
          :group 'default)

;;;;; Comments 
(defcustom default-prog-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :group 'default-prog)

(defcustom default-prog-comment-timestamp-format-concise "%F"
  "Specifier for date in `default-prog-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :group 'default-prog)

(defcustom default-prog-comment-timestamp-format-verbose "%F %T %z"
  "Like `default-prog-comment-timestamp-format-concise', but longer."
  :group 'default-prog)

;;;###autoload
(defun default-prog-comment-dwim (arg)
  "Flexible, do-what-I-mean commenting.

If region is active and ARG is either a numeric argument greater
than one or a universal prefix (\\[universal-argument]), then
apply `comment-kill' on all comments in the region.

If the region is active and no ARG is supplied, or is equal to a
numeric prefix of 1, then toggle the comment status of the region.

Else toggle the comment status of the line at point.  With a
numeric prefix ARG, do so for ARGth lines (negative prefix
operates on the lines before point)."
  (interactive "p")
  (cond
   ((and (> arg 1) (use-region-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (num (count-lines beg end)))
      (save-excursion
        (goto-char beg)
        (comment-kill num))))
   ((use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end)))
   (t
    (save-excursion (comment-line (or arg 1))))))

(defvar default-prog-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun default-prog-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car default-prog-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'default-prog-comment--keyword-hist def)))


;;;###autoload
(defun default-prog-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `default-prog-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is always formatted as 'DELIMITER KEYWORD DATE:',
with the date format being controlled by the variable
`default-prog-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `default-prog-comment-timestamp-format-verbose'."
  (interactive
   (list
    (default-prog-comment--keyword-prompt default-prog-comment-keywords)
    current-prefix-arg))
  (let* ((date (if verbose
                   default-prog-comment-timestamp-format-verbose
                 default-prog-comment-timestamp-format-concise))
         (string (format "%s %s: " keyword (format-time-string date)))
         (beg (point)))
    (cond
     ((or (eq beg (point-at-bol))
          (default-line-regexp-p 'empty))
      (let* ((maybe-newline (unless (default-line-regexp-p 'empty 1) "\n")))
        ;; NOTE 2021-07-24: we use this `insert' instead of
        ;; `comment-region' because of a yet-to-be-determined bug that
        ;; traps `undo' to the two states between the insertion of the
        ;; string and its transformation into a comment.
        (insert
         (concat comment-start
                 ;; NOTE 2021-07-24: See function `comment-add' for
                 ;; why we need this.
                 (make-string
                  (comment-add nil)
                  (string-to-char comment-start))
                 comment-padding
                 string
                 comment-end))
        (indent-region beg (point))
        (when maybe-newline
          (save-excursion (insert maybe-newline)))))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

;;;;; Skeletons 
(defgroup default-skel nil
  "base skel functions"
  :group 'default)

(defcustom file-template-insert-automatically nil
  "*Insert file-template automatically.
Can be one of the following values:

nil - do not insert automatically.
t   - always insert automatically.
ask - ask whether to insert or not."
  :group 'default-skel
  :type '(choice (const :tag "No"  nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" 'ask)))

(defvar default-skel-available '()
  "Internal list of available default skeletons.")

(define-abbrev-table 'default-skel-abbrev-table ()
  "Abbrev table for Default skeletons."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*")

(defmacro default-skel-define (name doc &rest skel)
  "Define a default skeleton using NAME DOC and SKEL. The skeleton
will be bound to default-skel-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "default-skel-" name))))
    `(progn
       (define-abbrev default-skel-abbrev-table
         ,name "" ',function-name :system t)
       (setq default-skel-available
             (cons ',function-name default-skel-available))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert default %s skeleton." name))
         ,@skel))))

(define-abbrev-table 'default-abbrev-table ()
  "Default Abbrev table"
  :parents (list default-skel-abbrev-table))

;;;;; Shells 
;; Don't whine if there is a terminal open.
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

;;;;; Iedit 
;;;###autoload
(defun iedit-scoped (orig-fn)
  "Call `iedit-mode' with function-local scope, or global scope if called with a universal prefix."
  (interactive)
  (pcase-exhaustive current-prefix-arg
    ('nil (funcall orig-fn '(0)))
    ('(4) (funcall orig-fn))))

(advice-add #'iedit-mode :around #'iedit-scoped)

;;;; Utils
(defgroup default-util ()
  "base utilities"
  :group 'util)

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

;;;;; Formatting 
(defvar flex-fill-paragraph-column nil
      "Last fill column used in command `flex-fill-paragraph'.")

;;;###autoload
(defun flex-fill-paragraph (&optional fewer-lines unfill)
  "Fill paragraph, incrementing fill column to cause a change when repeated.
The global value of `fill-column' is not modified; it is only
bound around calls to `fill-paragraph'.

When called for the first time in a sequence, unfill to the
default `fill-column'.

When called repeatedly, increase `fill-column' until filling
changes.

With one universal prefix, increase `fill-column' until the
number of lines is reduced.  With two, unfill completely."
  (interactive "P")
  (let* ((fewer-lines (or fewer-lines (equal current-prefix-arg '(4))))
         (unfill (or unfill (equal current-prefix-arg '(16))))
         (fill-column
          (cond (unfill (setf flex-fill-paragraph-column nil)
                        most-positive-fixnum)
                (t (setf flex-fill-paragraph-column
                         (if (equal last-command this-command)
                             (or (flex-fill-paragraph--next-fill-column fewer-lines)
                                 fill-column)
                           fill-column))))))
    (fill-paragraph)
    (message "Fill column: %s" fill-column)))

(defun flex-fill-paragraph--next-fill-column (&optional fewer-lines)
  "Return next `fill-column' value.
If FEWER-LINES is non-nil, reduce the number of lines in the
buffer, otherwise just change the current paragraph."
  ;; This works well, but because of all the temp buffers, sometimes
  ;; when called in rapid succession, it can cause GC, which can be
  ;; noticeable.  It would be nice to avoid that.  Note that this has
  ;; primarily been tested on `emacs-lisp-mode'; hopefully it works
  ;; well in other modes.
  (let* ((point (point))
         (source-buffer (current-buffer))
         (mode major-mode)
         (fill-column (or flex-fill-paragraph-column fill-column))
         (old-fill-column fill-column)
         (hash (unless fewer-lines
                 (buffer-hash)))
         (original-num-lines (when fewer-lines
                               (line-number-at-pos (point-max)))))
    (with-temp-buffer
      (delay-mode-hooks
        (funcall mode))
      (insert-buffer-substring source-buffer)
      (goto-char point)
      (cl-loop while (and (fill-paragraph)
                          (if fewer-lines
                              (= original-num-lines (line-number-at-pos (point-max)))
                            (string= hash (buffer-hash))))
               ;; If filling doesn't change after 100 iterations, abort by returning nil.
               if (> (- fill-column old-fill-column) 100)
               return nil
               else do (cl-incf fill-column)
               finally return fill-column))))

;;;; Registry
(setq bookmark-default-file "~/shed/data/emacs/bookmarks")

;;;; Viper
;; It is sometimes convenient to have vi-style editing available. In
;; Emacs this is provided with the `viper` package. I find myself
;; preferring this input method specifically on mobile devices (iPhone
;; and iPad) as well as machines where I don't have the ability to
;; re-map the `CAPS LOCK` key to `CTRL`.
;;
;; There are other packages available for vi-emulation available on
;; MELPA which you may prefer such as `evil`. This package is great
;; for users who prefer vi input style on all devices, but since I
;; prefer Emacs input most of the time, it's a bit bloated for my
;; needs.
;; 
;; These settings may also be used to provide an Emacs distribution
;; for vim die-hards who have no interest in using Emacs keybinds.
(defgroup default-viper ()
  "Default extensions for `viper`, a vi-emulator."
  :group 'default)

;;;; Keys
(defgroup default-keys nil
  "base keys"
  :group 'default)

(defcustom base-keys-prefix ()
  "base-keys-mode prefix key"
  :group 'default-keys)

;;;;; Global keys 
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key "\C-c l" #'org-store-link)
(global-set-key "\C-c L" #'org-insert-link-global)
(global-set-key "\C-c o" #'org-open-at-point-global)

(define-minor-mode default-keys-mode "Global minor mode containing useful keybinds."
  :lighter " dk"
  :global t
  :keymap `(
	    ;; Registers
	    (,(kbd "C-c r y") . copy-to-register)
	    (,(kbd"C-c r j") . jump-to-register)
	    (,(kbd"C-c r f") . frameset-to-register)
	    (,(kbd"C-c r SPC") . point-to-register)
	    ;; Completion
	    (,(kbd "M-TAB") . company-complete)
	    ;; Commands
	    (,(kbd "C-c c r") . rustic-popup)
	    ;; Outlines
	    (,(kbd "C-c TAB") . outline-cycle)
	    (,(kbd "C-c C-n") . outline-next-visible-heading)
	    (,(kbd "C-c C-p") . outline-previous-visible-heading)
	    ;; Windows
	    (,(kbd "C-x -") . split-window-vertically)
	    (,(kbd "C-x =") . split-window-horizontally)
	    ;; Speedbar  
	    (,(kbd "C-c e s") . speedbar)
	    ;; Embark
	    (,(kbd "C-c a") . embark-act)

	    ;; Projects
	    (,(kbd "C-c p p") . project-switch-project)
	    (,(kbd "C-c p k") . project-kill-buffers)
	    (,(kbd "C-c p v") . project-vc-dir)
	    ;; Version Control
	    (,(kbd "C-c v v") . vc-next-action)
	    (,(kbd "C-c v .") . vc-dir)

	    ;; Shell
	    (,(kbd "C-c t x") . async-shell-command)
	    (,(kbd "C-c t t") . vterm)
	    (,(kbd "C-c t T") . eshell)

	    ;; Search
	    (,(kbd "C-c s w") . search-web)
	    (,(kbd "C-c s r") . rg)
	    (,(kbd "C-c s R") . rg-dwim-current-dir)
	    (,(kbd "C-c ? d") . apropos-documentation)
	    (,(kbd "C-c ? k") . apropos-variable)
	    (,(kbd "C-c ? c") . apropos-command)
	    (,(kbd "C-c ? l") . apropos-library)
	    (,(kbd "C-c ? u") . apropos-user-option)
	    (,(kbd "C-c ? v") . apropos-value)
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
	    (,(kbd "C-c n i") . org-id-get-create)
	    (,(kbd "C-c n p e") . set-effort-prop)
	    (,(kbd "C-c n e") . org-encrypt-entry)
	    (,(kbd "C-c n E") . org-encrypt-entries)
	    (,(kbd "C-c n d") . org-decrypt-entry)
	    (,(kbd "C-c n D") . org-decrypt-entries)
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
	    ;; Quick-links
	    (,(kbd "C-c 1") . log-file)
	    ;; Modes
	    (,(kbd "C-c m v") . global-visual-line-mode)
	    (,(kbd "C-c m h") . global-hl-line-mode)
	    (,(kbd "C-c m l") . global-display-line-numbers-mode)
	    (,(kbd "C-c m a") . gpm-mouse-mode)
	    (,(kbd "C-c m r") . refill-mode)
	    (,(kbd "C-c m k") . which-key-mode)
	    (,(kbd "C-c m c") . global-company-mode)
	    (,(kbd "C-c m R") . global-auto-revert-mode)
	    (,(kbd "C-c m t") . toggle-frame-tab-bar)
	    (,(kbd "C-c m d") . toggle-debug-on-error)
	    (,(kbd "C-c m i") . org-toggle-inline-images)
	    (,(kbd "C-c m y") . yas-global-mode)
	    ;; Etc
	    (,(kbd "C-c e m") . notmuch) ;mail
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
;;;; pkg
(provide 'default)
;;; default.el ends here
