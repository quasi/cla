;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Package Name : cla
;;; File Name : cla.asd
;;; Description : Auth system for Common Lisp web applications.

;;; Author : Abhijit Rao (mostly based on earlier work by Prashant Acharekar and Sourabh Nanda)
;;; Date Created :

;;; Date Last Modified :

;;; See the LICENSE file for licensing information.


(asdf:defsystem :cla
  :version "0.0.1"
  :author "Abhijit Rao"
  :maintainer "Abhijit Rao"
  :depends-on (:clsql :ironclad :hunchentoot :cl-base64 :babel :date-calc :pooler)
  :serial t
  :components ((:file "packages")
               (:file "conditions")
               (:file "utils")
               (:file "db")
               (:file "encrypt")
               (:file "cookie")
               (:file "cla")))


