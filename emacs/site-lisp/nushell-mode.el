;;; nushell-mode.el --- Major mode for nushell scripts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Azzam S.A

;; Author: Azzam S.A <vcs@azzamsa.com>
;; Homepage: https://github.com/azzamsa/emacs-nushell
;; Keywords: Nushell, shell

;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "24"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A very basic version of major mode for nushell shell scripts.
;; Current features:
;;
;;  - keyword highlight
;;

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defgroup nushell nil
  "Nushell shell support."
  :group 'languages)

(defcustom nushell-indent-offset 4
  "Default indentation offset for Nushell."
  :group 'nushell
  :type 'integer
  :safe 'integerp)

(defvar nushell-enable-auto-indent nil
  "Controls auto-indent feature.
If the value of this variable is non-nil, whenever a word in
`nushell-auto-indent-trigger-keywords' is typed, it is indented instantly.")

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    `(set (make-local-variable ',var) ,val)))

;;; Syntax highlighting
;;; To get the commands, use `help commands | get name | | save --raw tmp'
(defconst nushell-builtins
  (list
   "agg"
   "agg-groups"
   "alias"
   "all-false"
   "all-true"
   "all?"
   "and"
   "and"
   "ansi"
   "ansi gradient"
   "ansi strip"
   "any?"
   "append"
   "append"
   "arg-max"
   "arg-min"
   "arg-sort"
   "arg-true"
   "arg-unique"
   "as"
   "as"
   "as"
   "as-date"
   "as-datetime"
   "benchmark"
   "br_cmd"
   "build-string"
   "bytes"
   "bytes add"
   "bytes at"
   "bytes build"
   "bytes collect"
   "bytes ends-with"
   "bytes index-of"
   "bytes length"
   "bytes remove"
   "bytes replace"
   "bytes reverse"
   "bytes starts-with"
   "cache"
   "cal"
   "cd"
   "char"
   "clear"
   "col"
   "collect"
   "collect"
   "collect"
   "columns"
   "compact"
   "complete"
   "concat-str"
   "concatenate"
   "config"
   "config env"
   "config nu"
   "contains"
   "count"
   "count-null"
   "cp"
   "cumulative"
   "date"
   "date format"
   "date humanize"
   "date list-timezone"
   "date now"
   "date to-record"
   "date to-table"
   "date to-timezone"
   "debug"
   "decode"
   "decode base64"
   "def"
   "def-env"
   "default"
   "describe"
   "describe"
   "describe"
   "detect columns"
   "df-not"
   "do"
   "drop"
   "drop"
   "drop column"
   "drop nth"
   "drop-duplicates"
   "drop-nulls"
   "dtypes"
   "du"
   "dummies"
   "each"
   "each while"
   "echo"
   "empty?"
   "encode"
   "encode base64"
   "enter"
   "env"
   "error make"
   "every"
   "exec"
   "exit"
   "explode"
   "export"
   "export alias"
   "export def"
   "export def-env"
   "export env"
   "export extern"
   "expr-not"
   "extern"
   "fetch"
   "fetch"
   "field"
   "fill-na"
   "fill-null"
   "filter-with"
   "find"
   "first"
   "first"
   "first"
   "flatten"
   "flatten"
   "fmt"
   "fn"
   "for"
   "format"
   "format filesize"
   "from"
   "from"
   "from csv"
   "from eml"
   "from ics"
   "from ini"
   "from json"
   "from nuon"
   "from ods"
   "from ssv"
   "from toml"
   "from tsv"
   "from url"
   "from vcf"
   "from xlsx"
   "from xml"
   "from yaml"
   "from yml"
   "g"
   "get"
   "get"
   "get-day"
   "get-hour"
   "get-minute"
   "get-month"
   "get-nanosecond"
   "get-ordinal"
   "get-second"
   "get-week"
   "get-weekday"
   "get-year"
   "glob"
   "grid"
   "group"
   "group-by"
   "group-by"
   "group-by"
   "hash"
   "hash base64"
   "hash md5"
   "hash sha256"
   "headers"
   "help"
   "hide"
   "histogram"
   "history"
   "if"
   "ignore"
   "input"
   "insert"
   "into"
   "into binary"
   "into bool"
   "into datetime"
   "into db"
   "into decimal"
   "into df"
   "into duration"
   "into filesize"
   "into int"
   "into lazy"
   "into nu"
   "into nu"
   "into nu"
   "into string"
   "is-admin"
   "is-duplicated"
   "is-in"
   "is-not-null"
   "is-not-null"
   "is-null"
   "is-null"
   "is-unique"
   "join"
   "join"
   "keep"
   "keep until"
   "keep while"
   "keybindings"
   "keybindings default"
   "keybindings list"
   "keybindings listen"
   "kill"
   "last"
   "last"
   "last"
   "length"
   "let"
   "let-env"
   "limit"
   "lines"
   "list"
   "lit"
   "load-env"
   "lowercase"
   "ls"
   "ls-df"
   "match"
   "math"
   "math abs"
   "math avg"
   "math ceil"
   "math eval"
   "math floor"
   "math max"
   "math median"
   "math min"
   "math mode"
   "math product"
   "math round"
   "math sqrt"
   "math stddev"
   "math sum"
   "math variance"
   "max"
   "max"
   "mean"
   "mean"
   "median"
   "median"
   "melt"
   "merge"
   "metadata"
   "min"
   "min"
   "mkdir"
   "module"
   "move"
   "mv"
   "n"
   "n-unique"
   "n-unique"
   "nth"
   "nu-check"
   "nu-highlight"
   "open"
   "open-db"
   "open-df"
   "or"
   "or"
   "order-by"
   "otherwise"
   "over"
   "overlay"
   "overlay add"
   "overlay list"
   "overlay new"
   "overlay remove"
   "p"
   "par-each"
   "parse"
   "path"
   "path basename"
   "path dirname"
   "path exists"
   "path expand"
   "path join"
   "path parse"
   "path relative-to"
   "path split"
   "path type"
   "pivot"
   "port"
   "post"
   "prepend"
   "print"
   "ps"
   "quantile"
   "quantile"
   "query"
   "random"
   "random bool"
   "random chars"
   "random decimal"
   "random dice"
   "random integer"
   "random uuid"
   "range"
   "reduce"
   "register"
   "reject"
   "rename"
   "rename"
   "replace"
   "replace-all"
   "reverse"
   "reverse"
   "rm"
   "roll"
   "roll down"
   "roll left"
   "roll right"
   "roll up"
   "rolling"
   "rotate"
   "run-external"
   "sample"
   "save"
   "schema"
   "select"
   "select"
   "select"
   "seq"
   "seq char"
   "seq date"
   "set"
   "set-with-idx"
   "shape"
   "shells"
   "shift"
   "shuffle"
   "size"
   "skip"
   "skip until"
   "skip while"
   "sleep"
   "slice"
   "sort"
   "sort-by"
   "sort-by"
   "source"
   "split"
   "split chars"
   "split column"
   "split list"
   "split row"
   "split-by"
   "std"
   "std"
   "str"
   "str camel-case"
   "str capitalize"
   "str collect"
   "str contains"
   "str downcase"
   "str ends-with"
   "str find-replace"
   "str index-of"
   "str kebab-case"
   "str length"
   "str lpad"
   "str pascal-case"
   "str replace"
   "str reverse"
   "str rpad"
   "str screaming-snake-case"
   "str snake-case"
   "str starts-with"
   "str substring"
   "str title-case"
   "str to-datetime"
   "str to-decimal"
   "str to-int"
   "str trim"
   "str upcase"
   "str-lengths"
   "str-slice"
   "strftime"
   "sum"
   "sum"
   "sys"
   "table"
   "take"
   "take"
   "take until"
   "take while"
   "term size"
   "testing-db"
   "to"
   "to csv"
   "to csv"
   "to html"
   "to json"
   "to md"
   "to nuon"
   "to parquet"
   "to text"
   "to toml"
   "to tsv"
   "to url"
   "to xml"
   "to yaml"
   "touch"
   "transpose"
   "tutor"
   "unalias"
   "uniq"
   "unique"
   "update"
   "update cells"
   "uppercase"
   "upsert"
   "url"
   "url host"
   "url path"
   "url query"
   "url scheme"
   "use"
   "value-counts"
   "var"
   "var"
   "version"
   "view-source"
   "watch"
   "when"
   "where"
   "where"
   "which"
   "window"
   "with-column"
   "with-env"
   "wrap"
   "z"
   "zi"
   "zip"
   ))

(defconst nushell-keywords
  (list
   "let"
   "if"
   "+"
   "-"
   "*"
   "/"
   "**"
   "mod"
   "=="
   "!="
   "<"
   "<="
   ">"
   ">="
   "=~"
   "!~"
   "in"
   "not-in"
   "not"
   "&&"
   "and"
   "||"
   "or"
   "//"
   "**"
   "bit-or"
   "bit-xor"
   "bit-and"
   "bit-shl"
   "bit-shr"
   "starts-with"
   "ends-with"
   ))

;;; Add `nushell-builtin' and `nushell-keywords' to
;;; font-lock
(defconst nushell-font-lock-keywords-1
  (list

   ;; Builtins
   `( ,(rx-to-string `(and
                       symbol-start
                       (eval `(or ,@nushell-builtins))
                       symbol-end)
                     t)
      .
      font-lock-builtin-face)

   ;; Keywords
   `( ,(rx-to-string `(and
                       symbol-start
                       (eval `(or ,@nushell-keywords))
                       symbol-end)
                     t)
      .
      font-lock-keyword-face)))

(defvar nushell-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?\' "\"'" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?$ "'" table)
    table)
  "Syntax table for `nushell-mode'.")


;;; Mode definition

;;;###autoload
(define-derived-mode nushell-mode prog-mode "Nushell"
  "Major mode for editing nushell shell files."
  :syntax-table nushell-mode-syntax-table
  (setq-local indent-line-function 'nushell-indent-line)
  (setq-local font-lock-defaults '(nushell-font-lock-keywords-1))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

;;;###autoload
;;; Specify major mode by file extension .nu
(add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-mode))
;;; Specify major mode by shebang
(add-to-list 'interpreter-mode-alist '("nu" . nushell-mode))

(provide 'nushell-mode)

;;; nushell-mode.el ends here
