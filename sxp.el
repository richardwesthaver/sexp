;;; sxp.el --- S-Expression Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: lisp

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

;; Emacs Support for S-Expression files (.sxp).

;;; Code:
(define-derived-mode sxp-mode lisp-data-mode "S-Expr"
  "Major mode for editing '.sxp' files.")

(add-to-list 'auto-mode-alist '("\\.sxp" . sxp-mode))

;;; OOP
(require 'eieio)

(defclass sxp () () :abstract t)

(cl-defgeneric from-sxp (obj sxp)
  "Update OBJ using values from SXP.")
(cl-defgeneric to-sxp (obj)
  "Return OBJ as a list.")
(cl-defgeneric read-sxp (obj &optional stream)
  "Read S-Expressions directly from STREAM (default =
  `standard-input')and updated OBJ.")
(cl-defgeneric write-sxp (obj &optional stream comment)
  "Write S-Expressions directly to STREAM (default =
  `standard-output') with optional COMMENT.")

(defclass form (sxp)
  ((form :initarg :form :accessor form)))

(cl-defmethod from-sxp ((obj form) sxp)
  (initialize-instance obj (list :form sxp))
  obj)
(cl-defmethod to-sxp ((obj form))
  (oref obj :form))
(cl-defmethod read-sxp ((obj form) &optional stream)
  (initialize-instance obj (list :form (read stream)))
  obj)
(cl-defmethod write-sxp ((obj form) &optional stream)
  (print (to-sxp obj) stream))

(with-eval-after-load "ert"
  (defmacro deftest (name &rest body)
    "shorthand for `ert-deftest'."
    (declare (indent 1))
    `(ert-deftest ,name () :tags '(sxp) ,@body))
  (deftest sxp:read
    (should (read-sxp (form) "(hey stranger)")))
  (deftest sxp:write
    (should (write-sxp (form :form nil))))
  (deftest sxp:from
    (should (from-sxp (form) '(test 1 2 3))))
  (deftest sxp:to
    (should (to-sxp (form :form '("test" 'ing)))))
  (deftest sxp:fmt
    (should t))
  (deftest sxp:mode
    (should t)))

(provide 'sxp)
;;; sxp.el ends here
