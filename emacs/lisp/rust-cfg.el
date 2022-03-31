;;; rust-cfg.el --- Rust Config -*- lexical-binding: t; -*-

;; Copyright (C) 2022  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: languages

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
(require 'fu)

(add-packages '(rust-mode eglot))

(with-eval-after-load 'rust-mode
  (setq rust-indent-offset 2))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

(fu-define-skeleton rust-err
    "Insert default err.rs content"
  nil > "pub type Result<T> = std::result::Result<T, Error>;" \n \n
  "#[derive(Debug)]" \n "pub enum Error {" \n
  "  " > _ \n
  "  Io(std::io::Error)," \n
  "}" \n \n
  "impl std::error::Error for Error {" \n "fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {" \n
  "  match *self {" \n
  "    " \n
  "    Error::Io(ref err) => Some(err)," \n
  "}" \n \n
  "impl std::fmt::Display for Error {" \n
  "fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {" \n
  "  match *self {" \n
  "    " \n
  "    Error::Io(ref err) => err.fmt(f)," \n
  "  }" \n
  "}")

(fu-define-skeleton rust-fn
  "Insert a Rust function."
  nil > "fn " > _ "() {" \n \n "}")

(fu-define-skeleton rust-match
    "Insert a Rust match."
  nil > "match " > _ "{" \n \n
  "}")

(provide 'rust-cfg)
;;; rust-cfg.el ends here
