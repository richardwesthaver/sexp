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

(with-eval-after-load "ert"
  (ert-deftest sxp:read()
    (should t))
  (ert-deftest sxp:write()
    (should t))
  (ert-deftest sxp:from()
    (should t))
  (ert-deftest sxp:to()
    (should t))
  (ert-deftest sxp:fmt()
    (should t))
  (ert-deftest sxp:mode()
    (should t)))

(provide 'sxp)
;;; sxp.el ends here
