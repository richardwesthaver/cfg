;;; rw-fu.el --- RW functions -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Richard Westhaver
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is really disorganized but lots of useful bits and bobs here.

;;; Code:

(defun regexp-p (regexp)
  "Return non-nil if REGEXP is valid (from python.el)."
  (ignore-errors (string-match regexp "") t))

;; TODO 2023-04-12: sep arg
(defun os-path-join (a &rest ps)
  (let ((path a))
    (while ps
      (let ((p (pop ps)))
        (cond
         ((string-prefix-p "/" p)
          (setq path p))
         ((or (not path) (string-suffix-p "/" p))
          (setq path (concat path p)))
         (t (setq path (concat path "/" p))))))
    path))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path."
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defun wc (&optional start end)
  "Return a 3-element list with lines, words and characters in
region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (list (count-lines start end) n (- end start))))

(defun get-file-load-history (&optional file load)
  "Return a list of symbols and features defined in
FILE. Defaults to file visited by current buffer.

If LOAD is non-nil, reload the file before checking
`load-history'."
  (let ((file (expand-file-name (or file buffer-file-name))))
    (when (and
           load
           (not (string-suffix-p "pkg" (file-name-base file))))
      (load file t t))
    (assoc file load-history)))

(defun add-to-load-path (&rest paths)
  "Add PATHS to `load-path'."
  (mapc (lambda (x)
          (cond
           ((listp x) (mapc #'add-to-load-path x))
           ('_ (cl-pushnew x load-path))))
        paths))

;;;###autoload
(defun byte-compile-autoload-dir (dir)
  "Byte-compile and autoload all packages in directory DIR."
  (interactive "Ddir: ")
  (require 'autoload)
  (let ((generated-autoload-file (expand-file-name "loaddefs.el" dir)))
    (dolist (dir (directory-files dir t "^[^.]"))
      (when (file-directory-p dir)
        (byte-recompile-directory dir 0)
        (update-directory-autoloads dir)))
    ;; (add-to-list 'load-path dir)
    (load generated-autoload-file nil t)))

(defun byte-compile-autoload-dirs (&rest dirs)
  "Byte-compile and autoload all packages in DIRS, creating a
    'loaddefs.el' for each directory."
  (dolist (d dirs)
    (byte-compile-autoload-dir d)))

(defun format-iso-week-number (&optional date)
  "format DATE as ISO week number with week days starting on
    Monday. If DATE is nil use current date."
  (let* ((week (format-time-string "%W" date))
         (prefix (if (= (length week) 1)
                     "w0" "w")))
    (concat prefix week)))

(defun last-day-of-year (&optional date)
  "Return the last day of the year as time."
  (encode-time 0 0 0 31 12 (nth 5 (decode-time
                                   (or date (current-time))))))

(defun last-day-of-month (&optional date)
  "Return the last day of month as time."
  (let* ((now (decode-time (or date (current-time))))
         (month (nth 4 now))
         (year (nth 5 now))
         (last-day-of-month (calendar-last-day-of-month month year)))
    (encode-time 0 0 0 last-day-of-month month year)))

(defun last-day-of-week (&optional date)
  "Return the last day of the week as time."
  (let* ((now (or date (current-time)))
         (datetime (decode-time now))
         (dow (nth 6 datetime)))
    (time-add now (days-to-time (- 7 dow)))))

(defun first-day-of-week (&optional date)
  "Return the first day of the week as time."
  (let* ((now (or date (current-time)))
         (datetime (decode-time now))
         (dow (nth 6 datetime)))
    (time-subtract now (days-to-time dow))))

;; org-id utils
(defun org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have a CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID"))
	  ;; use CUSTOM_ID for links
	  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new prefix))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun sym-props (s)
  "Return the symbol properties for provided symbol S.

Signals user error if S can't be coerced to a symbol."
  (cond
   ((stringp s) (sym-props (intern s)))
   ((symbolp s) (symbol-plist s))
   (t (user-error "invalid symbol: %s" s))))

(defun sym-docs (s)
  "Return the docstring for provided symbol S.

Signals user error if S can't be coerced to a symbol."
  (cond
   ((stringp s) (sym-docs (intern s)))
   ((featurep s) (documentation-property s 'group-documentation))
   ((functionp s) (documentation s))
   ((macrop s) (documentation s))
   ((boundp s) (documentation-property s 'variable-documentation))
   ;; special symbol (eieio)
   ((listp s) (sym-docs (cadar s)))
   (t (user-error "invalid symbol: %s" s))))

(defun collect-file-symbols (&optional file exclude)
  "Collect and return data for all defined symbols in FILE.
If FILE is nil, defaults to file visited by current buffer.

Elements of EXCLUDE, if any, will be skipped.

The resulting list is of the form:
([(:TYPE SYM DOC)]...)"
  (let ((syms (get-file-load-history file t)))
    (remove
     'nil
     (mapcar
      (lambda (x)
        (if (or
             (eq 'require (car-safe x))
             (eq 'define-symbol-props (car-safe x))
             (eq 'cl-deftype-satisfies (car-safe x))
             (eq 'define-type (car-safe x)))
            nil
          (setf x
                (list
                 (or (car-safe x)
                     'defcustom)
                 (or (cdr-safe x)
                     x)
                 (sym-docs (or
                            (cdr-safe x)
                            x))))))
      (cdr syms)))))

;; quick and dirty duality of syntax
(defvar git-check-ignore t
  "When non-nil, check and obey '.gitignore' files.")

(defun git-check-ignore (&optional dir)
  "Return a list of files to be ignored in DIR (defaults to
`default-directory'). Note that this does NOT include the .git
directory."
  (interactive)
  (with-dir (or dir default-directory)
    (mapcar (lambda (x)
              (replace-regexp-in-string "\\\"" ""
               (replace-regexp-in-string "\\\\\\(.\\|\n|\"\\)" "\\1" x)))
            (split-string
             (shell-command-to-string
              "pwsh.exe -c git check-ignore $(ls)")))))

(defun git-dir-p (dir)
  "Return non-nil if DIR is a '.git' directory."
  (when (string=
         ".git"
         (file-name-nondirectory
          (directory-file-name dir)))
    t))

(defun collect-dir-symbols (&optional dir)
  "Collect and return data for all define symbols in DIR.
Defaults to `default-directory'.

The result list is of the form:
([(FILE [:TYPE SYM DOC]...)]...)"
  (mapcan
   (lambda (x)
     (setf x (collect-file-symbols x)))
   (directory-files-recursively
    (or dir default-directory)
    ".el$")))

(defun symbol-definition-to-org (symdef)
  "Interpret SYMDEF as an org-mode heading and return it as a
   string."
  (org-element-interpret-data
   (org-element-headline-interpreter
    `(headline (:title ,(mkstr (cadr symdef))
                       :level 1
                       :tags (,(mkstr (car symdef)))))
    (mkstr (caddr symdef)))))

(defun sym-defs-to-org (path &optional stream)
  "Print all symdefs found at PATH (file or directory) to STREAM.

Defaults to returning a STRING but also accepts buffers and
filenames."
  (let* ((syms
          (cond
           ((file-directory-p path) (collect-dir-symbols path))
           ((file-exists-p path) (collect-file-symbols path))
           (t (user-error "%s is not an existing file or directory." path))))
         (res (mapconcat #'symbol-definition-to-org syms "\n")))
    (cond
     ((null stream) res)
     ((bufferp stream) (with-current-buffer stream (insert res)))
     ((file-exists-p stream) (with-file stream (insert res)))
     ((stringp stream) (with-temp-file stream (insert res))))))

(defun describe-symbols (pattern)
  "Describe the Emacs Lisp symbols matching PATTERN.
All symbols that have PATTERN in their name are described in the
*Help* buffer."
  (interactive "sDescribe symbols matching: ")
  (let ((describe-func
         (lambda (s)
           ;; Print description of symbol.
           (if (fboundp s)             ; It is a function.
               (princ
                (format "%s\t%s\n%s\n\n" s
                        (if (commandp s)
                            (let ((keys (where-is-internal s)))
                              (if keys
                                  (concat
                                   "Keys: "
                                   (mapconcat 'key-description
                                              keys " "))
                                "Keys: none"))
                          "Function")
                        (or (documentation s)
                            "not documented"))))

           (if (boundp s)              ; It is a variable.
               (princ
                (format "%s\t%s\n%s\n\n" s
                        (if (custom-variable-p s)
                            "Option " "Variable")
                        (or (documentation-property
                             s 'variable-documentation)
                            "not documented"))))))
        sym-list)

    ;; Build a list of symbols that match pattern.
    (mapatoms (lambda (sym)
                (if (string-match pattern (symbol-name sym))
                    (setq sym-list (cons sym sym-list)))))

    ;; Display the data.
    (help-setup-xref (list 'describe-symbols pattern)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (mapcar describe-func (sort sym-list 'string<)))))

(defun translate-string (s tbl)
  "Convenience function wrapping `translate-region'. Convert
String using char Table."
  (with-temp-buffer
    (insert s)
    (translate-region (point-min) (point-max) tbl)
    (buffer-string)))

;;;###autoload
(defun net-check-opts ()
  ;; https://gnu.huihoo.org/emacs/24.4/emacs-lisp/Network-Options.html#Network-Options
  ;; non-blocking
  (let ((non-blocking (featurep 'make-network-process '(:nowait t)))
        ;; UNIX socket
        (unix (featurep 'make-network-process '(:family local)))
        ;; UDP
        (udp (featurep 'make-network-process '(:type datagram))))
    (message "non-blocking: %s, unix: %s, udp: %s" non-blocking unix udp)))

  ;;;###autoload
(defun random-integers (min max n)
  "Return N random integers between MIN (inclusive) and MAX (exclusive)."
  (let ((list ()))
    (dotimes (_ n)
      (push (+ (cl-random (- max min)) min) list))
    list))

  ;;;###autoload
(defun int-to-binary-string (i)
  "convert an integer into its binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

  ;;;###autoload
(defun read-elisp-file (file)
  "Read Elisp data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun kill-ring-car-escpace-quotes ()
  "Escape doublequotes in car of kill-ring."
  (interactive)
  (with-temp-buffer
    (insert (car kill-ring))
    (goto-char (point-min))
    (while (search-forward "\"" nil t 1)
      (replace-match "\\\\\""))
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun emacs-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;;;###autoload
(defun emacs-restart ()
  "Handler for SIGUSR1 signal, to (re)start an emacs server.
Can be tested from within emacs with:
  (signal-process (emacs-pid) 'sigusr1)
or from the command line with:
$ kill -USR1 <emacs-pid>
$ emacsclient -c
"
  (interactive)
  (server-force-delete)
  (server-start))

;;;###autoload
(defun byte-compile-autoload-dir (dir)
  "Byte-compile and autoload all packages in directory DIR."
  (interactive "Ddir: ")
  (require 'autoload)
  (let* ((dir (expand-file-name dir))
        (generated-autoload-file
         (expand-file-name "loaddefs.el" dir)))
    (unless (file-exists-p generated-autoload-file)
      (make-empty-file generated-autoload-file t))
    (dolist (d (directory-files dir t "^[^.]"))
      (when (file-directory-p d)
        (byte-recompile-directory dir 0)
        (update-directory-autoloads dir)))))

;;;###autoload
(defun recompile-user-dirs (&optional force)
  "Recompile `user-lisp-directory' and
`user-site-lisp-directory', generating autoloads along the way."
  (interactive)
  (dolist (p (list user-site-lisp-directory user-lisp-directory))
    (let ((generated-autoload-file (expand-file-name "loaddefs.el" p)))
      (message "recompiling and generating autoloads for: %s" p)
      (byte-compile-autoload-dir p))))

;;;###autoload
(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'rw-fu)
;;; rw-fu.el ends here
