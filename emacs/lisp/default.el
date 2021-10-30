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
(require 'package-x)
;;;; Settings
(defgroup default nil
  "default settings")

(defcustom default-website "https://rwest.io/"
  "default website homepage. don't forget the slash!"
  :group 'default)

;;;;; data
(defvar default-data-dir (expand-file-name "~/shed/data/emacs"))
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

(defvar org-dir '(expand-file-name org-directory)
  "custom directory for user org files")

(setq org-todo-keywords
      '((sequence "TODO(t)" "RESEARCH(r)" "HACK(h)" "FIXME(f)" "REVIEW(R)" "NOTE(n)" "GOTO(g)" "NEXT(N)" "|" "DONE(d@)" "KILL(k@)")))

;;;;; org-crypt
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt"))) ;; prevent nested crypts
(setq org-crypt-key nil) ;; Either a GPG Key ID or set to nil to use symmetric encryption.

;;;;; todos
(defun org-todo-at-date (date)
  "create a todo entry for a given date."
  (interactive (list (org-time-string-to-time (org-read-date))))
  (cl-flet ((org-current-effective-time (&rest r) date)
            (org-today (&rest r) (time-to-days date)))
    (cond ((eq major-mode 'org-mode) (org-todo))
          ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))


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

(setq org-refile-use-cache t
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 3))
      org-src-fontify-natively t
      org-src-tabs-act-natively t
      org-src-tabs-act-natively t
      org-startup-indented t
      org-imenu-depth 6
      org-outline-path-complete-in-steps nil
      org-preview-latex-image-directory "~/.config/emacs/.cache/ltximg"
      org-latex-image-default-width "8cm")

;;;###autoload
(defun src-block-tags (src-block)
  "Return tags for SRC-BLOCK (an org element)."
  (let* ((headers (-flatten
                   (mapcar 'org-babel-parse-header-arguments
                           (org-element-property :header src-block))))
         (tags (cdr (assoc :tags headers))))
    (when tags
      (split-string tags))))

;;;;; properties
(progn
  (setq org-global-properties
        '(quote (("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                 ("STYLE_ALL" . "habit")))))

;;;;; org-export
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

;;;;; links 
(require 'ox-publish)
  (setq org-link-abbrev-alist
        '(("google"    . "http://www.google.com/search?q=")
          ("gmap"      . "http://maps.google.com/maps?q=%s")
          ("omap"      . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
          ("ads"       . "https://ui.adsabs.harvard.edu/search/q=%20author%3A\"%s\"")
          ("rw" . "https://rwest.io/%s")
          ("src" . "https://hg.rwest.io/%s")
          ("contrib" . "https://hg.rwest.io/contrib/%s")
          ("cdn" . "https://rwest.io/a/%s")))

(defvar yt-iframe-format
  (concat "<iframe width=\"480\""
          " height=\"360\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

;;;;; agenda
(defvar org-agenda-overriding-header)
(defvar org-agenda-sorting-strategy)
(defvar org-agenda-restrict)
(defvar org-agenda-restrict-begin)
(defvar org-agenda-restrict-end)

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

;;;; Programming
(defgroup default-prog ()
          "basic programming extensions"
          :group 'default)

;;;;; Comments 
(defcustom default-prog-comment-keywords
  '("TODO" "NOTE" "REVIEW" "FIXME" "HACK" "RESEARCH")
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
  "default keys"
  :group 'default)

(defcustom cmd-keys-prefix ()
  "cmd-keys prefix"
  :group 'default-keys)

(defcustom mode-keys-prefix ()
  "mode-keys prefix"
  :group 'default-keys)
(defcustom default-keys-prefix ()
  "default-keys-mode prefix key"
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
	    ;; Commands
	    (,(kbd "C-c c r") . rustic-popup)
	    ;; Outlines
	    (,(kbd "M-TAB") . outline-cycle)
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
	    (,(kbd "M-s w") . search-web)
	    (,(kbd "M-s r") . rg)
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
	    ;; Modes
	    (,(kbd "C-c m v") . global-visual-line-mode)
	    (,(kbd "C-c m h") . global-hl-line-mode)
	    (,(kbd "C-c m l") . global-display-line-numbers-mode)
	    (,(kbd "C-c m a") . gpm-mouse-mode)
	    (,(kbd "C-c m r") . refill-mode)
	    (,(kbd "C-c m k") . which-key-mode)
	    (,(kbd "C-c m c") . global-corfu-mode)
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
;;;; Setup
;;;###autoload
(defun default-setup ()
  "Setup defaults"
  ;; default settings
  (setq package-enable-at-startup nil

	make-backup-files nil
	auto-save-list-file-prefix (concat default-data-dir "auto-save/.")

	confirm-kill-emacs nil
	confirm-kill-process nil
	use-short-answers t

	display-time-format "%Y-%m-%d--%H:%M"
	ring-bell-function 'ignore
	gc-cons-percentage 0.6
	gc-cons-threshold most-positive-fixnum
	;; Completion
	completion-ignore-case t
	;; Web
	shr-use-colors nil
	shr-use-fonts nil
	shr-max-image-proportion 0.6
	shr-image-animate nil
	shr-discard-aria-hidden t
	tab-always-indent 'complete
	shr-cookie-policy nil
	browse-url-browser-function 'eww-browse-url
	eww-search-prefix "https://duckduckgo.com/html?q="
	url-privacy-level '(email agent cookies lastloc))

  ;; Hooks
  (add-hook 'after-init-hook 'default-keys-mode)
  (add-hook 'term-exec-hook 'set-no-process-query-on-exit)
  (add-hook 'shell-mode-hook 'set-no-process-query-on-exit)

  ;; init custom package archive
  (setq package-archives '(("contrib" . "/home/ellis/shed/data/emacs/contrib/")
			   ("local" . "/home/ellis/shed/data/emacs/lisp/")))
  (package-initialize)
  )

;;;; provide
(provide 'default)
;;; default.el ends here
