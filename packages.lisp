;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Package Name : CLA


(in-package #:cl-user)

(defpackage #:cla
  (:use #:cl #:clsql)
  (:export #:+cla-db-pool+
           #:is-auth-cookie-valid
           #:set-auth-cookie
           #:destroy-auth-cookie
           #:+cla-database-type+
           #:+cla-database-username+
           #:+cla-database-password+
           #:+cla-database-host+
           #:+cla-database-name
           #:new-user-registration
           #:do-login
           #:do-logout
           #:complete-user-registration
           #:change-user-password
           #:reset-password
           #:reset-password-request))

