;;; repeat-cfg.el --- repeat-mode config -*- lexical-binding: t; -*-

;; Copyright (C) 2023  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: convenience

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

;; https://tildegit.org/acdw/define-repeat-map.el/src/branch/main/define-repeat-map.el

;;; Code:
(require 'default)
(require 'repeat)

(with-eval-after-load 'which-key
  (defun repeated-prefix-help-command ()
    (interactive)
    (when-let* ((keys (this-command-keys-vector))
		(prefix (seq-take keys (1- (length keys))))
		(orig-keymap (key-binding prefix 'accept-default))
		(keymap (copy-keymap orig-keymap))
		(exit-func (set-transient-map keymap t #'which-key-abort)))
      (define-key keymap [remap keyboard-quit]
		  (lambda () (interactive) (funcall exit-func)))
      (which-key--create-buffer-and-show nil keymap)))
  (setq prefix-help-command #'repeated-prefix-help-command))

(defun define-repeat-map--make-alias (cmd map)
  "Internal.  Make an alias for CMD in `repeat-map' MAP."
  (intern (concat (symbol-name cmd) "|"
                  (symbol-name map))))

(defun define-repeat-map--map-commands (fn args)
  "Internal.  Map FN over ARGS, whch are commands in MAP."
  (let (res)
    (dolist (arg args)
      (unless (stringp arg)
        (push (funcall fn arg) res)))
    (reverse res)))

(defun define-repeat-map--define-keys (map fn args)
  "Internal.  Map `define-key' in MAP over ARGS, transorming them with FN."
  (unless (zerop (mod (length args) 2))
    (signal 'wrong-number-of-arguments (length args)))
  (let (res)
    (while args
      (let ((key (pop args))
            (cmd (funcall fn (pop args))))
        (push `(define-key ,map (kbd ,key) #',cmd)
              res)))
    (reverse res)))

;;;###autoload
(defmacro define-repeat-map (name &rest keys)
  "Define a `repeat-map', NAME -repeat-map, and bind KEYS to it.
Each ARG is a list of lists containing keybind definitions of
the form (KEY DEFINITION) KEY is anything `kbd' can recognize,
and DEFINITION is passed directly to `define-key'.

Optionally, the car of an arglist can contain the following
symbols, which changes the behavior of the key definitions in the
rest of the list:

:enter - Provided commands can enter the `repeat-map', but aren't
bound in the map.  They need to be bound elsewhere, however.

:exit - Keys are bound in the `repeat-map', but can't enter the
map.  Their invocation exits the `repeat-map'.

:continue - Keys are bound in the `repeat-map', but can't enter the
map.  However, their invocations keep the `repeat-map' active."
  (declare (indent 1))
  (let ((define-repeat-map--result)
        (map (intern (concat (symbol-name name) "-repeat-map"))))
    ;; Create the keymap
    (push `(defvar ,map (make-sparse-keymap)
             "Defined by `define-repeat-map'.")
          define-repeat-map--result)

    ;; Iterate through KEYS
    (dolist (arg keys)
      (pcase (car arg)
        (:enter
         ;; Add the map to the commands' repeat-map property.
         (push `(progn
                  ,@(define-repeat-map--map-commands
                      (lambda (cmd) `(put ',cmd 'repeat-map ',map))
                      (cdr arg)))
               define-repeat-map--result))
        
        (:exit
         ;; Bind the commands in the map.
         (push `(progn
                  ,@(define-repeat-map--define-keys
                      `,map #'identity (cdr arg)))
               define-repeat-map--result))

        (:continue
         ;; Make an alias for each command, and process that alias like the
         ;; default, below.
         (push `(progn
                  ,@(define-repeat-map--define-keys
                      `,map
                      (lambda (cmd) (define-repeat-map--make-alias cmd map))
                      (cdr arg))
                  ,@(define-repeat-map--map-commands
                      (lambda (cmd)
                        (let ((alias (define-repeat-map--make-alias cmd map)))
                          `(progn
                             (defalias ',alias ',cmd
                               "Defined by `define-repeat-map'.")
                             (put ',alias
                                  'repeat-map ',map))))
                      (cdr arg)))
               define-repeat-map--result))

        (_
         ;; Default: bind the commands in the map, and add the map to the
         ;; commands' repeat-map property.
         (push `(progn
                  ,@(define-repeat-map--define-keys `,map #'identity arg)
                  ,@(define-repeat-map--map-commands
                      (lambda (cmd) `(put ',cmd 'repeat-map ',map))
                      arg))
               define-repeat-map--result))))

    `(add-hook 'repeat-mode-hook
               (lambda nil
                 ,@(reverse define-repeat-map--result)))))

(provide 'repeat-cfg)
;;; repeat-cfg.el ends here
