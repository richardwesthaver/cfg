;;; erc-cfg.el --- ERC config -*- lexical-binding: t; -*-

;; Copyright (C) 2023  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: comm

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
(require 'default)
(require 'tls)
(require 'erc)
 (defun start-erc ()
   "Connect to IRC."
   (interactive)
   (erc-tls :server "irc.libera.chat" :port 6667
	:client-certificate '("/mnt/k/krypt/libera.pem"))
   (setq erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs")
				       ("irc.libera.chat" "#linux")
				       ("irc.libera.chat" "#rust")
				       ("irc.libera.chat" "#btrfs")
				       ("irc.libera.chat" "#lisp")
				       ("irc.libera.chat" "#sbcl")
				       ("irc.oftc.net" "#llvm"))

(provide 'erc-cfg)
;;; erc-cfg.el ends here
