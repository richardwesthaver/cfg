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

(defcustom user-data-dir (expand-file-name "~/shed/data/emacs/")
  "location to store emacs-related data"
  :group 'default)

(defcustom user-settings '()
  "custom settings to load at startup in addition to 'default-settings'
'default-settings'"
  :type 'alist
  :group 'default)

(defcustom default-settings '()
  "alist of VAR . VAL pairs for 'map-settings'. Used in
'default-setup' with the optional arg 'DEFAULT' enabled.

This should only be used for trivial settings that you use with
all of your Emacs configs."
  :type 'alist
  :group 'default)

(defun map-settings (settings &optional default)
  "map an alist SETTINGS to their respective variables. DEFAULT
will change the function used to 'set-default' as opposed to
'set'"
  (dolist (s settings)
    (let ((var (car s))
	  (val (cdr s)))
      (if (eq default t)
	  (set-default var val)
	(set var val)))))

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


;;;; Keys
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key "\C-c l" #'org-store-link)
(global-set-key "\C-c L" #'org-insert-link-global)
(global-set-key "\C-c o" #'org-open-at-point-global)

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
	    (,(kbd "C-c t T") . vterm)
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

;;;; Scratch Buffers
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
(defcustom default-vc-backend "hg"
  "the default vc-backend to use for version-control-related commands."
  :type '(string)
  :group 'default)

;;;; Registers 
(defcustom registers-save-file (expand-file-name ".registers.el" user-data-dir)
  "The place where the contents of the registers should be saved."
  :type '(file)
  :group 'default)

(defun jump-to-register-action (register &optional delete)
  "Do what is the most sane thing to do for the thing stored in
   register Either insert text (evt. a rectangle), move point to
   location stored in a register, a buffer stored in a register,
   a file stored in register, or run a macro saved in a register.
   If the register contains a file name, find that file. Or
   restore a saved window/frame configuration."
  (let ((val (get-register register)))
    (cond
     ((registerv-p val)
      (cl-assert (registerv-jump-func val) nil
                 "Don't know how to jump to register %s"
                 (single-key-description register))
      (funcall (registerv-jump-func val) (registerv-data val)))
     ((and (consp val) (frame-configuration-p (car val)))
      (set-frame-configuration (car val) (not delete))
      (goto-char (cadr val)))
     ((and (consp val) (window-configuration-p (car val)))
      (set-window-configuration (car val))
      (goto-char (cadr val)))
     ((markerp val)
      (or (marker-buffer val)
          (error "That register's buffer no longer exists"))
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     ((and (consp val) (eq (car val) 'buffer))
      (switch-to-buffer (cdr val)))
     ((and (consp val) (eq (car val) 'macro))
                                        ;appearently the only way to run a macro is by putting them in
                                        ;last-kbd-macro (named keyboard macros can only (as far as I
                                        ;know) be called interactively, but this works quite
                                        ;unproblematically).
      (let ((old-macro last-kbd-macro))
        (setq last-kbd-macro (cdr val))
        (call-last-kbd-macro)
        (setq last-kbd-macro old-macro)))
     ((and (consp val) (eq (car val) 'file-query))
      (or (find-buffer-visiting (nth 1 val))
          (y-or-n-p (format "Visit file %s again? " (nth 1 val)))
          (error "Register access aborted"))
      (find-file (nth 1 val))
      (goto-char (nth 2 val)))
     ((or (stringp val)
          (consp val)
          (numberp val)
          (and (markerp val)
               (marker-position val)))
      (insert-register register))
     (t
      (error "Register doesn't contain a buffer, buffer position, macro, file, text, rectangle or configuration")))))

;; overwrite default
(defun jump-to-register (register &optional delete)
  (interactive (list (register-read-with-preview "Jump to register: ")
                     current-prefix-arg))
  (jump-to-register-action register delete))

(defun save-registers-hook ()
  (add-hook 'kill-emacs-hook
            #'(lambda ()
               (better-registers-save-registers))))

(defun save-registers (&optional filename queryp)
  "Print the contents of all registers to a file as loadable data.
   Cannot save window/frame configuration.
   But it works with keyboard macros, text, buffernames,
   filenames and rectangles.

   If filename is non-nil and queryp is nil, use that, otherwise
   use the default filename.  If queryp is non-nil (a prefix
   argument is given), query interactively for the file-name."
  (interactive "i\nP")
  (when queryp
    (setq filename (read-file-name nil registers-save-file)))
  (let ((fn (or filename registers-save-file))
        (print-level nil) ;Let us write anything
        (print-length nil)
        (b (generate-new-buffer "*registers*")))
    (set-buffer b)
    (dolist (i register-alist)
      (let ((char (car i))
            (contents (cdr i)))
        (cond
         ((stringp contents)
          (insert (format "%S\n"
                          `(set-register
                            ,char
                            ,contents))))
         ((numberp contents) ;numbers are printed non-quotes
          (insert (format "%S\n" `(set-register ,char ,contents))))
         ((markerp contents)
          (insert (format
                   "%S\n"
                   `(set-register
                     ,char
                     '(file-query
                       ,(buffer-file-name (marker-buffer contents))
                       ,(marker-position contents))))))
         ((bufferp (cdr contents))
          (insert (format "%s\n"
                          `(set-register ,char
                                         ',(buffer-name (cdr contents))))))
         (t (when (and contents ; different from nil
                       (not (or (window-configuration-p (car contents))
                                (frame-configuration-p (car contents)))))
              (insert (format "%S\n"
                              `(set-register ,char (quote ,contents)))))))))
    (write-file fn)
    (kill-buffer b)))

(defun put-buffer-in-register (register &optional delete)
  "Put current buffername in register - this would also work for
  just buffers, as switch-to-buffer can use both, but it
  facilitates for easier saving/restoring of registers."
  (interactive "cPut current buffername in register: \nP.")
  (set-register register (cons 'buffer (buffer-name (current-buffer)))))

(defun put-buffer-filename-in-register (register &optional delete)
  "This is better than put-buffer-in-register for file-buffers, because a closed
   file can be opened again, but does not work for no-file-buffers."
  (interactive "cPut the filename of current buffer in register: \nP")
  (set-register register (cons 'file (buffer-file-name (current-buffer)))))

(defun put-keyboard-macro-in-register (register &optional delete)
  "Save the contents of the last keyboard macro to the given register.
   can be played again by jump-to-register."
  (interactive "cPut last keyboard-macro in register: \nP")
  (set-register register (cons 'macro last-kbd-macro)))

(defun decrement-register (number register)
  "Subtract NUMBER from the contents of register REGISTER.
Interactively, NUMBER is the prefix arg."
  (interactive "p\ncDecrement register: ")
  (increment-register (- number) register))

(defun toggle-macro-recording ()
  (interactive)
  (message "hej")
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

(defun play-macro-if-not-playing ()
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (call-last-kbd-macro)))

;;;; Org 
(defun org-keys ()
  "add default keys to 'org-mode-map'"
  (define-key org-mode-map (kbd "C-c M-i") 'org-toggle-inline-images)
  (define-key org-mode-map (kbd "C-c M-a") 'org-agenda-current-subtree-or-region)
  (define-key org-mode-map (kbd "C-c n i") 'org-id-get-create)
  (define-key org-mode-map (kbd "C-c M-p e") 'set-effort-prop)
  (define-key org-mode-map (kbd "C-c M-n e") 'org-encrypt-entry)
  (define-key org-mode-map (kbd "C-c M-n E") 'org-encrypt-entries)
  (define-key org-mode-map (kbd "C-c M-n d") 'org-decrypt-entry)
  (define-key org-mode-map (kbd "C-c M-n D") 'org-decrypt-entries)
  (define-key org-mode-map (kbd "C-c M-h") 'src-block-tags))


(defun org-todo-at-date (date)
  "create a todo entry for a given date."
  (interactive (list (org-time-string-to-time (org-read-date))))
  (cl-flet ((org-current-effective-time (&rest r) date)
            (org-today (&rest r) (time-to-days date)))
    (cond ((eq major-mode 'org-mode) (org-todo))
          ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

(defun org-ask-location ()
  "prompt for a location\"\""
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
  (end-of-line))

(defun org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "From [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
    %s
    #+BEGIN_%s %s
 %s
    #+END_%s" initial-txt type headers code-snippet type)))

(defun org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
     within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (org-capture-fileref-snippet f "EXAMPLE" "" nil)))

(defun org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
     within an Org SRC block with a language based on the current mode
     and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (org-capture-fileref-snippet f "SRC" org-src-mode func-name))))

(defun region-to-clocked-task (start end)
  "Copies the selected text to the currently clocked in org-mode task."
  (interactive "r")
  (org-capture-string (buffer-substring-no-properties start end) "3"))

;;;###autoload
(defun src-block-tags (src-block)
  "Return tags for SRC-BLOCK (an org element)."
  (let* ((headers (-flatten
                   (mapcar 'org-babel-parse-header-arguments
                           (org-element-property :header src-block))))
         (tags (cdr (assoc :tags headers))))
    (when tags
      (split-string tags))))

  (setq org-global-properties
        '(quote (("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                 ("STYLE_ALL" . "habit"))))

(defun org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

(add-hook 'org-clock-in-prepare-hook
          'org-mode-ask-effort)

;;;###autoload
(defun org-adjust-tags-column-reset-tags ()
  "In org-mode buffers it will reset tag position according to
`org-tags-column'."
  (when (and
         (not (string= (buffer-name) "*Remember*"))
         (eql major-mode 'org-mode))
    (let ((b-m-p (buffer-modified-p)))
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (command-execute 'outline-next-visible-heading)
            ;; disable (message) that org-set-tags generates
            (flet ((message (&rest ignored) nil))
		  (org-set-tags 1 t))
            (set-buffer-modified-p b-m-p))
        (error nil)))))

;;;###autoload
(defun org-align-all-tables ()
  "align all tables in current buffer"
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(defun org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.

A tag is considered redundant if it is local to a headline and
inherited by a parent headline."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
               local inherited tag)
           (dolist (tag alltags)
             (if (get-text-property 0 'inherited tag)
                 (push tag inherited) (push tag local)))
           (dolist (tag local)
             (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))

;;;###autoload
(defun org-export-headings-to-org ()
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-org-export-to-org nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level))))

(defvar org-agenda-overriding-header)
(defvar org-agenda-sorting-strategy)
(defvar org-agenda-restrict)
(defvar org-agenda-restrict-begin)
(defvar org-agenda-restrict-end)

;;;###autoload
(defun org-agenda-reschedule-to-today ()
  (interactive)
  (flet ((org-read-date (&rest rest) (current-time)))
	(call-interactively 'org-agenda-schedule)))

;; Patch org-mode to use vertical splitting
(defadvice org-prepare-agenda (after org-fix-split)
  (toggle-window-split))
(ad-activate 'org-prepare-agenda)

(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

(defun org-agenda-log-mode-colorize-block ()
  "Set different line spacing based on clock time duration."
  (save-excursion
    (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
                     ('light
                      (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                     ('dark
                      (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          ;; larger duration bar height
          (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)

;;;###autoload
(defun org-agenda-current-subtree-or-region (only-todos)
  "Display an agenda view for the current subtree or region.
 With prefix, display only TODO-keyword items."
  (interactive "P")
  (let ((starting-point (point))
        header)
    (with-current-buffer (or (buffer-base-buffer (current-buffer))
                             (current-buffer))
      (if (use-region-p)
          (progn
            (setq header "Region")
            (put 'org-agenda-files 'org-restrict (list (buffer-file-name (current-buffer))))
            (setq org-agenda-restrict (current-buffer))
            (move-marker org-agenda-restrict-begin (region-beginning))
            (move-marker org-agenda-restrict-end
                         (save-excursion
                           ;; If point is at beginning of line, include
                           ;; heading on that line by moving forward 1.
                           (goto-char (1+ (region-end)))
                           (org-end-of-subtree))))
        ;; No region; restrict to subtree.
        (save-excursion
          (save-restriction
            ;; In case the command was called from an indirect buffer, set point
            ;; in the base buffer to the same position while setting restriction.
            (widen)
            (goto-char starting-point)
            (setq header "Subtree")
            (org-agenda-set-restriction-lock))))
      ;; NOTE: Unlike other agenda commands, binding `org-agenda-sorting-strategy'
      ;; around `org-search-view' seems to have no effect.
      (let ((org-agenda-sorting-strategy '(priority-down timestamp-up))
            (org-agenda-overriding-header header))
        (org-search-view (if only-todos t nil) "*"))
      (org-agenda-remove-restriction-lock t)
      (message nil))))

;;;;; Setup 
;;;###autoload
(defun org-setup ()
  "setup Org-mode"
  ;; keys
  (eval-after-load 'org #'(progn (org-keys)))
  ;; outline
  (setq org-startup-indented t
	org-imenu-depth 6
	org-outline-path-complete-in-steps nil)
  ;; todos
  (setq org-todo-keywords
	'((type "TODO(t!)" "|" "DONE(D!)")
	  (sequence "FIND(q)" "|" "FOUND(!)")
	  (sequence "RESEARCH(r)" "RECORD(e)" "|" "NOTED(!)")
	  (sequence "OUTLINE(o)" "DRAFT(d)" "|" "PUBLISHED(P!)")
	  (sequence "FIX(f)" "REVIEW(w)" "IMPL(i)" "TEST(t)" "|" "FIXED(F!)")
	  (type "GOTO(g)" "HACK(h)" "NOTE(n)" "CODE(c)" "LINK(l)" "|")
	  (sequence "|" "CANCELED(C@)")))
  ;; captures
  (setq org-capture-templates
	'(("t" "task" entry (file "t.org") "* %?\n- %U" :prepend t)
	  ("1" "current-task-item" item (clock) "%i%?")
	  ("2" "current-task-checkbox" checkitem (clock) "%i%?")
	  ("3" "current-task-region" plain (clock) "%i" :immediate-finish t :empty-lines 1)
	  ("4" "current-task-kill" plain (clock) "%c" :immediate-finish t :empty-lines 1)
	  ("l" "log" item (file+headline "i.org" "log") "%U %?" :prepend t)
	  ("s" "secret" table-line (file+headline "i.org" "secret") "| %^{key} | %^{val} |" :immediate-finish t)
	  ("m" "meta" item (file+function "~/shed/src/meta/m.org" org-ask-location) "%?")
	  ("n" "note" item (file+function "~/shed/src/meta/n.org" org-ask-location) "%?")))

  ;; org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))

  ;; src
  (setq org-structure-template-alist
	'(("s" . "src")
	  ("e" . "src emacs-lisp")
	  ("x" . "src shell")
	  ("h" . "export html")
	  ("p" . "src python")
	  ("r" . "src rust")
	  ("E" . "example")
	  ("q" . "quote")
	  ("c" . "center")
	  ("C" . "comment")
	  ("v" . "verse")))

  (setq org-confirm-babel-evaluate nil)

  (setq org-src-fontify-natively t
	org-src-tabs-act-natively t)

  ;; archive
  (setq org-archive-location "basement::")
  ;; refile
  (setq org-refile-use-cache t
	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-targets '((nil :maxlevel . 3)
			     (org-agenda-files :maxlevel . 3)))

  ;; publish
  (setq org-preview-latex-image-directory "~/.config/emacs/.cache/ltximg"
	org-latex-image-default-width "8cm")

  ;; links
  (setq org-link-abbrev-alist
	'(("google" . "http://www.google.com/search?q=")
	  ("gmap" . "http://maps.google.com/maps?q=%s")
	  ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
	  ("ads" . "https://ui.adsabs.harvard.edu/search/q=%20author%3A\"%s\"")
	  ("rw" . "https://rwest.io/%s")
	  ("yt" . "https://youtube.com/watch?v=%s")
	  ("src" . "https://hg.rwest.io/%s")
	  ("contrib" . "https://hg.rwest.io/contrib/%s")
	  ("cdn" . "https://rwest.io/a/%s"))))

;;;; Prog
;;;;; Comments 
(defcustom prog-comment-keywords
        '("TODO" "REVIEW" "FIX" "HACK" "RESEARCH")
        "List of strings with comment keywords."
        :group 'default)

(defcustom prog-comment-timestamp-format-concise "%F"
  "Specifier for date in `prog-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :group 'default)

(defcustom prog-comment-timestamp-format-verbose "%F %T %z"
  "Like `prog-comment-timestamp-format-concise', but longer."
  :group 'default)

(define-key prog-mode-map (kbd "M-;") 'prog-comment-dwim)
(define-key prog-mode-map (kbd "C-c M-;") 'prog-comment-timestamp-keyword)
;;;###autoload
(defun prog-comment-dwim (arg)
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

(defvar prog-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun prog-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car prog-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'prog-comment--keyword-hist def)))


;;;###autoload
(defun prog-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `prog-comment-keywords', though it is possible to
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
`prog-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `prog-comment-timestamp-format-verbose'."
  (interactive
   (list
    (prog-comment--keyword-prompt prog-comment-keywords)
    current-prefix-arg))
  (let* ((date (if verbose
                   comment-timestamp-format-verbose
                 prog-comment-timestamp-format-concise))
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
(defcustom file-template-insert-automatically nil
  "*Insert file-template automatically.
Can be one of the following values:

nil - do not insert automatically.
t   - always insert automatically.
ask - ask whether to insert or not."
  :group 'default
  :type '(choice (const :tag "No"  nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask" 'ask)))

(defvar skel-available '()
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
       (setq skel-available
             (cons ',function-name skel-available))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert default %s skeleton." name))
         ,@skel))))

(define-abbrev-table 'default-abbrev-table ()
  "Default Abbrev table"
  :parents (list default-skel-abbrev-table))

(default-skel-define web
    "Adds a link to 'default-website' while prompting for a possible
  extension."
  "path: "
  default-website str "")

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

;;;; Setup
;;;###autoload
(defun default-setup ()
  "Setup defaults"
  ;; enable native-compilation on supported builds
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (progn
      (setq native-comp-async-report-warnings-errors nil)
      (setq comp-deferred-compilation t)
      (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-data-dir))
      (setq package-native-compile t)
      ))
  ;; set defaults
  (setq-default package-enable-at-startup nil
		make-backup-files nil
		auto-save-list-file-prefix (expand-file-name "auto-save/." user-data-dir)
		tramp-auto-save-directory (expand-file-name "auto-save/tramp/" user-data-dir)
		confirm-kill-emacs nil
		confirm-kill-process nil
		use-short-answers t
		display-time-format "%Y-%m-%d--%H:%M"
		ring-bell-function 'ignore
		gc-cons-percentage 0.6
		gc-cons-threshold most-positive-fixnum
		completion-ignore-case t
		;; org
		org-agenda-files (list org-directory)
		shr-use-colors nil
		shr-use-fonts nil
		shr-max-image-proportion 0.6
		shr-image-animate nil
		shr-discard-aria-hidden t
		bookmark-default-file (expand-file-name "bookmarks" user-data-dir)
		project-list-file (expand-file-name "projects" user-data-dir)
		emms-directory (expand-file-name "emms" user-data-dir)
		gnus-cache-directory (expand-file-name "gnus" user-data-dir)
		url-cache-directory (expand-file-name "url" user-data-dir)
		tab-always-indent 'complete
		shr-cookie-policy nil
		browse-url-browser-function 'browse-url-default-browser
		eww-search-prefix "https://duckduckgo.com/html?q="
		url-privacy-level '(email agent cookies lastloc))

  (setq org-agenda-files '("~/shed/stash/org/"))
  ;; init custom package archive
  (setq package-archives '(("contrib" . (expand-file-name "contrib" user-data-dir))
			   ("local" . (expand-file-name "lisp" user-data-dir))))

  ;; user settings
  (map-settings user-settings)

  ;; hooks
  (add-hook 'after-init-hook 'keys)
  (add-hook 'after-init-hook #'org-setup)
  (add-hook 'term-exec-hook 'set-no-process-query-on-exit)
  (add-hook 'shell-mode-hook 'set-no-process-query-on-exit)
  (package-initialize))
;;;; provide
(provide 'default)
;;; default.el ends here
