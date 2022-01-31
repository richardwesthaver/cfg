;;; emms-cfg.el --- EMMS Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: multimedia

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

;; 

;;; Code:
(add-to-list 'package-selected-packages 'emms)

(require 'emms-setup)

(emms-all)
(emms-default-players)

(setq emms-info-functions '(emms-info-exiftool))
(setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
(setq emms-browser-thumbnail-small-size 64)
(setq emms-browser-thumbnail-medium-size 128)
(setq emms-playlist-mode-window-width 60)
(setq emms-playlist-mode-hook #'emms-mark-mode)

(cond
 ((eq system-type 'darwin)
  (require 'emms-player-mplayer)
  (define-emms-simple-player mplayer '(file url)
			     (regexp-opt
			      '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
				".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://"
				"mms://" ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a"
				".flv" ".ogv" ".pls"))
			     "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen"))
 ((eq system-type 'linux)
  (require 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (define-emms-simple-player mikmod '(file)
			     (regexp-opt '(".669" ".AMF" ".DSM" ".FAR" ".GDM" ".IT" ".IMF"  
					   ".MED" ".MTM" ".OKT" ".S3M" ".STM" ".STX" ".ULT" 
					   ".APUN" ".XM" ".MOD" ".amf" ".dsm" ".far" ".gdm"                            
					   ".it" ".imf" ".mod" ".med" ".mtm" ".okt" ".s3m" 
					   ".stm" ".stx" ".ult" ".apun" ".xm" ".mod" ".MOD")
					 ) "mikmod" "-q" "-p" "1" "-X")
  (add-to-list 'emms-player-list 'emms-player-mikmod)))

;;; Keys
(define-prefix-command 'emms-keys)

(keymap-set emms-keys "b" #'emms-browser)
(keymap-set emms-keys "m" #'emms)
(keymap-set emms-keys "l" #'emms-metaplaylist-mode-go)
(keymap-set emms-keys "c" #'emms-playlist-mode-go-popup)
(keymap-set emms-keys "." #'emms-play-now)
(keymap-set emms-keys "SPC" #'emms-pause)
(keymap-set emms-keys "x" #'emms-stop)
(keymap-set emms-keys "s" #'emms-seek)
(keymap-set emms-keys "n" #'emms-next)
(keymap-set emms-keys "p" #'emms-previous)
(keymap-set emms-keys "r" #'emms-random)
(keymap-set emms-keys "q" #'emms-play-matching)
(keymap-set emms-keys "RET" #'emms-show)
(keymap-set emms-keys "-" #'emms-volume-lower)
(keymap-set emms-keys "+" #'emms-volume-raise)

(with-eval-after-load 'default
  (keymap-set keys-map "C-c m" #'emms-keys))

;;; Functions
(defun track-title-from-file-name (file)
  "For using with EMMS description functions. Extracts the track
title from the file name FILE, which just means a) taking only
the file component at the end of the path, and b) removing any
file extension."
  (with-temp-buffer
    (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
    (ignore-error 'search-failed
      (search-forward-regexp (rx "." (+ alnum) eol))
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun hd-emms-track-description (track)
  "Return a description of TRACK, for EMMS, but try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and artist title)
           (concat artist " - " title))
          (title title)
          ((eq (emms-track-type track) 'file)
           (track-title-from-file-name (emms-track-name track)))
          (t (emms-track-simple-description track)))))

(setq emms-track-description-function #'hd-emms-track-description)

(defun emms-play-smooth-jazz ()
  "Start up some nice Jazz"
  (interactive)
  (emms-play-streamlist "http://thejazzgroove.com/itunes.pls"))

(defun emms-play-matching (pattern)
  "Play matching song."
  (interactive "sPlay song matching: ")
  ;; Won't clear playlist when
  ;; `current-prefix-arg' is non-nil.
  (unless current-prefix-arg
    (with-current-emms-playlist
      (emms-playlist-clear)))
  (emms-play-find emms-source-file-default-directory pattern))

(defun emms-delete-file-from-disk ()
  "Delete this file from disk."
  (interactive)
  (let* ((current-track (emms-track-name (emms-playlist-track-at))))
    (when (yes-or-no-p (format "Are you really want to delete \' %s \' from disk? " current-track))
      (if (string-equal current-track (emms-playlist-play-filename))
          (emms-stop))
      (emms-playlist-mode-kill-entire-track)
      (dired-delete-file current-track)
      (message (format "Have delete \' %s \' from disk." current-track)))))

(defun emms-playlist-play-filename ()
  "Return the filename the current play."
  (cdr (assoc 'name (emms-playlist-current-selected-track))))

(defun emms-play-now()
  "Play default music directory."
  (interactive)
  (emms-play-directory-tree emms-source-file-default-directory))

(defun emms-play-online()
  "Play online music use emms."
  (interactive)
  (if (and (require 'w3m nil t)
           (w3m-anchor))
      (emms-play-url (w3m-anchor))
    (message "No valid url in here.")))

(defun emms-mark-track-and-move-next ()
  "Mark the current track, and move next track."
  (interactive)
  (call-interactively 'emms-mark-track)
  (call-interactively 'next-line))

(defun emms-mark-unmark-track-and-move-next ()
  "Unmark the current track, and move next track."
  (interactive)
  (call-interactively 'emms-mark-unmark-track)
  (call-interactively 'next-line))

(defun emms-tag-editor-next-same-field (&optional reverse)
  "Jump to next same field."
  (interactive)
  (let (filed-name)
    (save-excursion
      (beginning-of-line)
      (if (search-forward-regexp "^[^ ]*[ \t]+= " (line-end-position) t)
          (setq filed-name (buffer-substring (match-beginning 0) (match-end 0)))))
    (when filed-name
      (if (null reverse)
          (search-forward-regexp filed-name (point-max) t)
        (beginning-of-line)
        (search-backward-regexp filed-name (point-min) t))
      (goto-char (match-end 0)))))

(defun emms-tag-editor-prev-same-field ()
  "Jump to previous same field."
  (interactive)
  (emms-tag-editor-next-same-field t))

(defun emms-tag-editor-set-all+ ()
  "Set TAG to VALUE in all tracks.
If transient-mark-mode is turned on, you can apply the command to
a selected region.

 If `transient-mark-mode' is on and the mark is active, the
changes will only take effect on the tracks in the region.

This function is extension `emms-tag-editor-set-all',
make user can modified TAG content, not just type."
  (interactive)
  (let (tag current-value value)
    (setq tag (completing-read "Set tag: " emms-tag-editor-tags nil t))
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp (concat "^" tag "[ \t]+= ") (point-max) t 1)
      (setq current-value (buffer-substring (match-end 0) (line-end-position))))
    (setq value (read-from-minibuffer "To: " current-value))
    (save-excursion
      (save-restriction
        (if (and mark-active transient-mark-mode)
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^" (regexp-quote tag)) nil t)
          (skip-chars-forward " \t=")
          (delete-region (point) (line-end-position))
          (insert value))))))

(defun emms-tag-editor-set-tracknumber ()
  "Set `info-tracknumber' tag with a init increment value
and special alternation number."
  (interactive)
  (let (init-number alternation-number times)
    (setq init-number (read-number "Init number: " 1))
    (setq alternation-number (read-number "Alternation number: " 1))
    (setq times 0)
    (save-excursion
      (save-restriction
        (if (and mark-active transient-mark-mode)
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^info-tracknumber") nil t)
          (skip-chars-forward " \t=")
          (delete-region (point) (line-end-position))
          (insert (format "%s" (+ init-number (* alternation-number times))))
          (setq times (1+ times))
          )))))

(defun emms-tag-editor-set-tracknumber+ ()
  "Set `info-tracknumber' tag with a init increment value
and special alternation number."
  (interactive)
  (let (init-number alternation-number times)
    (setq init-number 1)
    (setq alternation-number 1)
    (setq times 0)
    (save-excursion
      (save-restriction
        (if (and mark-active transient-mark-mode)
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward (concat "^info-tracknumber") nil t)
          (skip-chars-forward " \t=")
          (delete-region (point) (line-end-position))
          (insert (format "%s" (+ init-number (* alternation-number times))))
          (setq times (1+ times))
          )))))

(defun emms-mark-duplicate-track ()
  "Mark duplicate track."
  (interactive)
  (let (have-duplicate-track
        current-track-title
        next-track-title
        original-tracks
        selected-point
        original-point)
    ;; Backup playlist data.
    (setq original-point (point))       ;backup cursor position
    (emms-playlist-mode-center-current) ;goto current play track
    (setq selected-point (point))       ;backup select track
    (setq original-tracks (nreverse     ;backup playlist with sequence
                           (emms-playlist-tracks-in-region
                            (point-min) (point-max))))
    ;; Find duplicate track.
    (emms-playlist-sort-by-info-title)
    (goto-char (point-min))
    (while (not (eobp))
      (save-excursion
        (setq current-track-title (emms-playlist-current-title))
        (forward-line +1)
        (setq next-track-title (emms-playlist-current-title)))
      (if (string-equal current-track-title next-track-title)
          (progn
            (or have-duplicate-track (setq have-duplicate-track t))
            (emms-mark-track)
            (forward-line +1)
            (emms-mark-track))
        (forward-line +1)))
    ;; Last step.
    (if have-duplicate-track
        (emms-first-mark-track)
      (emms-playlist-clear)             ;clear playlist
      (dolist (track original-tracks)   ;restore origianl playlist
        (emms-playlist-insert-track track))
      (emms-playlist-select selected-point) ;select play track
      (goto-char original-point)            ;restore original point
      (message "Haven't found duplicate track."))))

(defun emms-first-mark-track ()
  "Jump to first mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (goto-char (point-min))
    (if (search-forward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No mark track."))))

(defun emms-last-mark-track ()
  "Jump to last mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (goto-char (point-max))
    (if (search-backward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No mark track."))))

(defun emms-next-mark-track ()
  "Jump to next mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (if (bolp)
        (forward-char +1))
    (if (search-forward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No next mark track."))))

(defun emms-prev-mark-track ()
  "Jump to previous mark track."
  (interactive)
  (let ((original-point (point))
        (original-column (current-column)))
    (if (not (bolp))
        (beginning-of-line))
    (if (search-backward-regexp (format "^%c" emms-mark-char) nil t)
        (move-to-column original-column t)
      (goto-char original-point)
      (message "No previous mark track."))))

(defun emms-playlist-current-title ()
  "Return the filename the current play."
  (cdr (assoc 'info-title (emms-playlist-track-at))))

(provide 'emms-cfg)
;;; emms-cfg.el ends here
