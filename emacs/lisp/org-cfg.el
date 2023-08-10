;;; org-cfg.el --- Org Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: Richard Westhaver <ellis@rwest.io>
;; Keywords: wp

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

;;; Code:
(add-to-list 'package-selected-packages 'org-web-tools 'citeproc)

(setq-default org-log-into-drawer t)
(setq org-cite-export-processors '((t csl)))
(setq org-html-style-default ""
      org-html-scripts ""
      org-html-htmlize-output-type 'inline-css
      org-export-htmlize-output-type 'inline-css
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-src-fontify-natively t
      make-backup-files nil
      debug-on-error t)
(setq org-html-postamble "<footer><div><p>created %d;<br>updated %C;</p></div></footer>")

(setq org-html-head-include-default-style t
      org-html-head-include-scripts t)

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

;;; Dynamic Blocks 
(defun org-dblock-write:meta-info (v)
  "this is a dynamic-block writer function. Creates a new info-tbl block for meta docs."
  (let ((name (plist-get v :src)))
    (insert "name"
	    name)))

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

;;;###autoload
(defun org-export-headings-to-html ()
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
           (org-html-export-to-html nil t nil t)
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
                     (light
                      (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                     (dark
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
	'((type "TBD(0)" "TODO(t!)" "|" "DONE(D!)")
	  (sequence "FIND(q)" "|" "FOUND(!)")
	  (sequence "RESEARCH(r)" "RECORD(e)" "|" "DONE(!)")
	  (sequence "OUTLINE(o)" "RESEARCH(r)" "DRAFT(d)" "REVIEW(w)" "|" "DONE(D!)")
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
	  ("s" "secret" table-line (file+headline "i.org" "krypt") "| %^{key} | %^{val} |" :immediate-finish t :kill-buffer t)
	  ("n" "note" item (file+function "~/shed/src/meta/n.org" org-ask-location) "%?")
	  ("i" "ramble" entry (file "t.org") "* OUTLINE %?\n:notes:\n:end:\n- _outline_ [/]\n  - [ ] \n  - [ ] \n- _refs_" :prepend t)
	  ("b" "bug" entry (file "t.org") "* FIX %?\n- _review_\n- _fix_\n- _test_" :prepend t)
	  ("r" "research" entry (file "t.org") "* RESEARCH %?\n:notes:\n:end:\n- _refs_" :prepend t)))
  (setq org-html-htmlize-output-type 'css
	org-html-head-include-default-style nil)
  (setq org-footnote-section nil)
  ;; org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt" "k"))

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
  (setq org-preview-latex-image-directory "~/.emacs.d/.cache/ltximg"
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

(add-hook 'org-mode-hook #'org-setup)
(provide 'org-cfg)
;;; org-cfg.el ends here
