;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

;;; Package Name : cla
;;; File Name : cla.lisp
;;; Description : Here goes the Description


(in-package :cla)

(export '(reset-password reset-password-request new-user-registration do-login cla-init complete-user-registration change-login-data-password verify-comfirmation-reset-url))


;;; Registers a new user in the system. The status of the user is unconfirmed untill he confirms it.
;;; Valid email-id and password are needed here. Password strength chould should happen before this step.
(defun new-user-registration (email-id password)
  "Registers a new user. Returns LOGIN-DATA data-structure."
  (when (or (not password) (not email-id))
    (error 'cla-registration-error :format-control "Password and email-id both needed!"))
  (with-cla-db
    (if (find-user 'EMAIL-ID email-id)
        (error 'cla-registration-error :format-control "Email-id: ~A already exists in the system." :format-arguments (list email-id))
        (progn
          (add-new-user email-id (hash-password password))
          (find-user 'EMAIL-ID email-id))))) ;;FIXME : check for existance or throw error



;;; TODO error conditions
(defun do-login (email-id password)
  "validates user password and sets the auth cookie"
  (unless (boundp 'hunchentoot:*request*)
    (error 'cla-login-error :format-control "No Hunchentoot:*request* found - how to set cookie ?"))
  (with-cla-db
    (let ((login-data (find-user 'EMAIL-ID email-id)))
      (cond ((or (null login-data)
                 (string/= (hash-password password) (password login-data)))
	     (error 'cla-login-error :format-control "Incorrect email-id or password."))
	    ((not (= (user-status login-data) (user-status-code 'CONFIRMED))) ;; TODO - currently setting 2 as confirmed user
	     (error 'cla-login-error
                    :format-control "Unconfirmed account for the email-id: ~A."
                    :format-arguments (list email-id)))
	    (t
             (add-new-login-instance (id login-data) (hunchentoot:real-remote-addr) 0)
             (set-auth-cookie login-data)
             (update-login-data login-data)
             login-data)))))


(defun do-logout (login-data-id)
  (destroy-auth-cookie)
  (delete-login-instances login-data-id 0))


;;; Confirming user registration or varifying user email address
(defun verify-comfirmation-reset-url (confirmation-reset-url)
  "Finds the user with the url in the db, changes the user status to 2 and delets the url from the db and returns t.
   If url is not found returns nil"
  (with-cla-db
    (find-user 'CONFIRMATION-RESET-URL confirmation-reset-url)))


;;; confirming and deleting the confirmation-reset-url
(defun complete-user-registration (id)
  ""
  (with-cla-db
    (update-user id 'user-status (user-status-code 'CONFIRMED))
    (update-user id 'confirmation-reset-url "")))



;;; FIXME init function
(defun quasi-test-cla-init ()
  "Creates the cipher for the encrypt/decrypt"
  (progn
    (construct-cipher-for-encrypt "quasi test")
    (setf +cla-database-type+ :mysql
          +cla-database-username+ "quasi"
          +cla-database-password+ "quasi"
          +cla-database-host+ "127.0.0.1"
          +cla-database-name+ "quasidb")))

;;; Temp function for initial testing purposes
;; (defun add-users-to-login-data ()
;;   (new-user-registration "percy@quasilab.in" "percy123")
;;   (new-user-registration "quasi@quasilab.in" "quasi123")
;;   (new-user-registration "dinsesh@quasilab.in" "dinesh123")
;;   (new-user-registration "shot@quasilab.in" "shot123"))


;;; to register new user from the console

(defun cla-new-user-registration-console (email-id password)
  "registers the new user from the console without confirmation"
  (with-cla-db
    (when (find-user 'EMAIL-ID email-id)
      (error 'cla-login-error
             :format-control "Email-id ~A already exists in the Tripr system."
             :format-arguments (list email-id)))
    (add-new-user email-id (hash-password password))
    (let ((login-data (find-user 'EMAIL-ID email-id)))
      (complete-user-registration (id login-data))
      login-data)))


(defun change-user-password (login-data old-password new-password)
  "Changes the login-data  password and updates the login data in the db"
  (with-cla-db
    (cond ((not (and login-data old-password new-password))
	   (error 'cla-login-error :format-control "None of LOGIN-DATA, OLD-PASSWORD and NEW-PASSWORD can be NULL"))
	  ((string/= (hash-password old-password) (password login-data))
	   (error 'cla-login-error :format-control "Incorrect old password"))
	  (t
           (update-user (id login-data) 'password new-password)))))


;;; password reset flow
(defun reset-password-request (login-data)
  ""
  (with-cla-db
    (setf (confirmation-reset-url login-data) (generate-unique-id)
          (user-status login-data) (user-status-code 'PASSWORD-RESET-REQUESTED))
    (update-login-data login-data)
    login-data))


(defun reset-password (login-data password reset-url)
  ""
  (when (string= (confirmation-reset-url login-data) reset-url)
    (with-cla-db
        (setf (password login-data) (hash-password password)
              (confirmation-reset-url login-data) ""
              (user-status login-data) (user-status-code 'CONFIRMED))
      (update-login-data login-data)
      login-data)))


;;; EOF
