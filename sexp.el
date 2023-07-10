;;; sexp.el --- S-Expression Mode -*- lexical-binding: t; -*-

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

;; Emacs Support for S-Expression files (.sexp).

;;; Code:
(define-derived-mode sexp-mode lisp-data-mode "S-Expr"
  "Major mode for editing '.sexp' files.")

(add-to-list 'auto-mode-alist '("\\.sexp" . sexp-mode))

;;; OOP
(require 'eieio)

(cl-defgeneric from-sexp (obj sexp)
  "Update OBJ using values from SEXP.")
(cl-defgeneric to-sexp (obj)
  "Return OBJ as a list.")
(cl-defgeneric read-sexp (obj &optional stream)
  "Read S-Expressions directly from STREAM (default =
  `standard-input')and updated OBJ.")
(cl-defgeneric write-sexp (obj &optional stream comment)
  "Write S-Expressions directly to STREAM (default =
  `standard-output') with optional COMMENT.")

(with-eval-after-load "ert"
  (ert-deftest sexp:read()
    (should t))
  (ert-deftest sexp:write()
    (should t))
  (ert-deftest sexp:from()
    (should t))
  (ert-deftest sexp:to()
    (should t))
  (ert-deftest sexp:fmt()
    (should t))
  (ert-deftest sexp:mode()
    (should t)))

(provide 'sexp)
;;; sexp.el ends here
