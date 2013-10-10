;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

;;; Package Name : cla
;;; File Name : conditions.lisp
;;; Description : Here goes the Description

(in-package :cla)

(define-condition cla-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to CLA"))

(define-condition cla-error (cla-condition simple-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "~?"
		     (simple-condition-format-control condition)
		     (simple-condition-format-arguments condition))))
  (:documentation "All errors signaled by cla are of this type"))

(define-condition cla-database-error (cla-error)
  ((database-message :initarg :message
		     :initform nil
		     :reader cla-database-error-message))
  (:report (lambda (condition stream)
             (format stream "The following ~a happened" (cla-database-error-message condition))))
  (:documentation "Clsql errors"))

(define-condition cla-registration-error (cla-error)
  ()
  (:documentation "CLA registration errors"))

(define-condition cla-login-error (cla-error)
  ()
  (:documentation "CLA login errors"))


;;; EOF
