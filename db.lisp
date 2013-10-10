;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

;;; Package Name : cla
;;; File Name : db.lisp
;;; Description : Here goes the Description

(in-package :cla)

#.(locally-enable-sql-reader-syntax)

(defvar +cla-database-type+ :mysql)
(defvar +cla-database-username+ "test")
(defvar +cla-database-password+ "test")
(defvar +cla-database-host+ "127.0.0.1")
(defvar +cla-database-name+ "test")
(defvar +cla-db-pool+ nil)

(defun cla-db-connect ()
  (handler-case (clsql:connect (list +cla-database-host+ +cla-database-name+ +cla-database-username+ +cla-database-password+)
                               :database-type +cla-database-type+
                               :if-exists :new)
    (error () (error 'cla-database-error))))

(defun cla-db-disconnect (connection)
  (ignore-errors (clsql:disconnect :database connection)))


(setf +cla-db-pool+ (pooler:make-pool :item-maker #'cla-db-connect
                                      :item-destroyer #'cla-db-disconnect))


(def-view-class login-data ()
  ((id
    :reader id
    :initarg :id
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment)
    :documentation "unique user id")
   (created-on
    :reader created-on
    :initarg :created-on
    :type wall-time
    :documentation "datetime of user registration")
   (modified-on
    :accessor modified-on
    :initarg :modified-on
    :type wall-time
    :documentation "datetime of last changes in login details")
   (password
    :accessor password
    :initarg :password
    :type (string 50)
    :documentation "password of user")
   (email-id
    :accessor email-id
    :initarg :email-id
    :type (string 40)
    :documentation "emailid of user")
   (user-status
    :accessor user-status
    :initarg :user-status
    :type integer
    :documentation "user status, UNCONFIRMED|CONFIRMED|ACTIVE|PASSWORD-RESET-REQUESTED|BLOCKED")
   (persistent-login
    :initarg :persistent-login
    :accessor persistent-login
    :type boolean
    :documentation "A flag (T/NIL) specifying whether the user wanted a persistent login.")
   (reset-question
    :accessor reset-question
    :initarg :reset-question
    :type (string 100)
    :documentation "secret question")
   (reset-answer
    :accessor reset-answer
    :initarg :reset-answer
    :type (string 20)
    :documentation "secret question's answer")
   (confirmation-reset-url
    :accessor confirmation-reset-url
    :initarg :confirmation-reset-url
    :type (string 100)
    :documentation "url used for confirmation or reset"))
  (:base-table login-data))


(defmethod print-object ((ld login-data) stream)
  (format stream "#<~S login-data-id:~A email-id:~A user-status:~A last-login:~A>"
	  (type-of ld)
	  (when (slot-boundp ld 'id) (id ld))
	  (when (slot-boundp ld 'email-id) (email-id ld))
	  (when (slot-boundp ld 'user-status) (user-status ld))
	  (when (slot-boundp ld 'last-login) (last-login ld))))


(def-view-class login-instances ()
  ((id
    :reader id
    :initarg :id
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment)
    :documentation "unique id")
   (login-data-id
    :reader login-data-id
    :initarg :login-data-id
    :type integer
    :documentation "login data id")
   (login-timestamp
    :reader login-timestamp
    :initarg :login-timestamp
    :type wall-time
    :documentation "login time")
   (remote-addr
    :reader remote-addr
    :initarg :remote-addr
    :type (string 20)
    :documentation "remote address")
   (app-id
    :reader app-id
    :initarg :app-id
    :type integer
    :documentation "Website is app number 0, mobile site maybe 1")) ;trying to future proof
  (:base-table login-instances))


(defmethod print-object ((li login-instances) stream)
  (format stream "#<~S ~A logged-in from ~A at ~A>"
	  (type-of li)
	  (when (slot-boundp li 'login-data-id) (login-data-id li))
	  (when (slot-boundp li 'remote-addr) (remote-addr li))
	  (when (slot-boundp li 'login-timestamp) (login-timestamp li))))



;;; create login-data table
(defun cla-db-setup ()
  ""
  (with-cla-db ()
    (drop-view-from-class 'login-data)
    (create-view-from-class 'login-data)
    (execute-command "alter table LOGIN_DATA change CREATED_ON CREATED_ON DATETIME DEFAULT CURRENT_TIMESTAMP;")
    (execute-command "alter table LOGIN_DATA change MODIFIED_ON MODIFIED_ON DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP;")
    (drop-view-from-class 'login-instances)
    (create-view-from-class 'login-instances)
    (execute-command "alter table LOGIN_INSTANCES change LOGIN_TIMESTAMP LOGIN_TIMESTAMP DATETIME DEFAULT CURRENT_TIMESTAMP;")))


;;; Add new user
(defun add-new-user (email-id password)
  "Adds user to the login-data table"
  (let ((new-reset-url (generate-unique-id)))
    (insert-records :into 'login-data
                    :attributes '(password email-id user_status confirmation-reset-url)
                    :values `(,password ,email-id 0 ,new-reset-url))))


(defun add-new-login-instance (login-data-id remote-addr app-id)
  "Adds a login-instance"
  (when (null login-data-id)
    (error 'cla-error :format-control "We need login-data-id"))
  (insert-records :into 'login-instances
                  :attributes '(login-data-id remote-addr app-id)
                  :values `(,login-data-id ,remote-addr ,app-id)))


(defun delete-login-instances (login-data-id app-id)
  (delete-records :from [login-instances]
                  :where [and [= [login-data-id] login-data-id] [= [app-id] app-id]]))


(defun find-login-instance (login-data-id app-id)
  (select 'login-instances :where [and [= [login-data-id] login-data-id] [= [app-id] app-id]] :refresh t))


;;; find user by EMAIL-ID, ID or CONFIRMATION-RESET-URL
(defun find-user (field data)
  (caar
   (case field
     (EMAIL-ID
      (select 'login-data :where [= [email-id] data] :refresh t))
     (ID
      (select 'login-data :where [= [id] data] :refresh t))
     (CONFIRMATION-RESET-URL
      (select 'login-data :where [= [confirmation-reset-url] data] :refresh t))
     (otherwise (error "No such searchable field : ~a" field)))))



(defun update-user (id field data)
  (update-records [login-data]
                  :av-pairs `((,field ,data))
                  :where [= [id] id]))


(defun remove-user (id)
  (update-user id 'uesr-status (user-status-code 'DELETED)))


#.(restore-sql-reader-syntax-state)

;;; EOF
