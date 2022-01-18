;;; mail-cfg.el --- Mail Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Richard Westhaver

;; Author: Richard Westhaver <ellis@jekyll>
;; Keywords: mail

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
(add-to-list 'package-selected-packages 'notmuch)
(require 'notmuch)

(defgroup hyde-mail ()
  "Hyde email system.
requires: notmuch, offlineimap
env: USER_EMAIL"
  :group 'hyde)

(defcustom hyde-mail-dir ()
  "Root path for mail-related data"
  :group 'hyde-mail)

(defcustom hyde-mailbox-alist ()
  "list of initialized mailboxes to query"
  :group 'hyde-mail)

(define-key keys-map (kbd "C-c e m") #'notmuch)
;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)

(setq user-mail-address "ellis@rwest.io"
      user-full-name "ellis")

;; smtp config
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it
      smtpmail-debug-info t
      message-default-mail-headers "Cc: \nBcc: \n"
      message-kill-buffer-on-exit t)

;;;###autoload
(defun notmuch-exec-offlineimap ()
  "execute offlineimap command and tag new mail with notmuch"
  (interactive)
  (start-process-shell-command "offlineimap"
                               "*offlineimap*"
                               "offlineimap -o")
  (notmuch-refresh-all-buffers))

(provide 'mail-cfg)
;;; mail-cfg.el ends here
