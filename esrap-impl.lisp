;;; esrap-impl.lisp --- ESRAP parser implementation of sxp protocol
(defpackage :sxp-esrap
  (:use :cl :esrap :sxp :parse-number :uiop)
  (:export :form :parse-sxp-string :parse-sxp-file :parse-form))

(in-package :sxp-esrap)

(defun not-dq (c) (not (eql #\" c)))
(defun is-q (c) (eql #\' c))
(defun is-bq (c) (eql #\` c))
(defun is-uq (c) (eql #\, c))
(defun is-sharp (c) (eql #\# c))

(defrule nl (* (or #\newline #\return)))
(defrule ws (+ (or #\space #\tab nl)) (:constant nil))
(defrule alpha (+ (alpha-char-p character)))
(defrule digit (+ (digit-char-p character)))
(defrule string-char (or (not-dq character) (and #\\ #\")))
(defrule q (is-q character))
(defrule bq (is-bq character))
(defrule uq (is-uq character))
(defrule sharp (is-sharp character))
;;; macro char
;; (defrule mc alpha)

(defrule number (+ (or digit #\. #\- #\+ #\/))
  (:lambda (n)
    (parse-number (text n) :radix 10)))
(defrule string (and #\" (* string-char) #\")
  (:destructure
   (q1 s q2)
   (declare (ignore q1 q2))
   (text s)))
;; must be parsed /after/ numbers
(defrule symbol (+ (or alpha digit #\- #\_ #\: #\+ #\% #\! #\?))
  (:lambda (s)
    (intern (text s))))

(defrule atom (or number string symbol))

(defrule list (and #\( (? ws) form (? ws) (* form) (? ws) #\))
  (:destructure
   (p1 w1 ca w2 cd w3 p2)
   (declare (ignore p1 p2 w1 w2 w3))
   `(,ca ,cd)))

(defrule form (or list qforms atom)
  (:function second)
  (:lambda (s &bounds start end)
    (list s (cons start end))))

(defrule qform (and q form)
  (:lambda (s)
    (text s)))
(defrule bqform (and bq form))
(defrule uqform (and uq form))
(defrule sharpform (and sharp character form))
(defrule qforms (or form qform bqform uqform sharpform)
  (:lambda (s)
    (text s)))

(defrule comment (and (+ #\;) (* (or #\space #\tab)) (* character) (+ nl)))
(defrule sxp (+ (or form nl ws comment))
  (:function second)
  (:lambda (s &bounds start end)
    (list s (cons start end))))

(defun parse-form (s) (parse 'form s))
(defun parse-sxp-string (s) (parse 'sxp s))
(defun parse-sxp-file (path) (parse-sxp-string (uiop:read-file-string path)))
