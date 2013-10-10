;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

;;; Package Name : cla
;;; File Name : cla.lisp
;;; Description : Here goes the Description


(in-package :cla)


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
  "Logout. Destroyes the cookie as well as _ALL_ the LOGIN-INSTANCEs"
  (destroy-auth-cookie)
  (delete-login-instances login-data-id 0))


;;; Registers a new user in the system. The status of the user is UNCONFIRMED until he confirms it by clicking the CONFIRMATION-URL.
;;; Valid email-id and password are needed here. Password strength check should happen before this step.
(defun new-user-registration (email-id password)
  "Registers a new user. Returns LOGIN-DATA data-structure."
  (when (or (not password) (not email-id))
    (error 'cla-registration-error :format-control "Password and email-id both needed!"))
  (with-cla-db
    (if (find-user 'EMAIL-ID email-id)
        (error 'cla-registration-error :format-control "Email-id ~A already exists in the system." :format-arguments (list email-id))
        (progn
          (add-new-user email-id (hash-password password))
          (find-user 'EMAIL-ID email-id))))) ;;FIXME : check for existance or throw error


(defun complete-user-registration (email-id confirmation-url)
  "Complete user registration. Verify the confirmation-url and change the user status to CONFIRMED."
  (let ((login-data (find-user 'EMAIL-ID email-id)))
    (cond (((not login-data)
            (error 'cla-registration-error
                   :format-control "Panic! No account for this email-id: ~A !"
                   :format-arguments (list email-id)))
           ((string/= confirmation-url (confirmation-reset-url login-data))
            (error 'cla-registration-error
                   :format-control "Confrimation URL does not match."))
           (t
            (update-user id 'user-status (user-status-code 'CONFIRMED))
            (update-user id 'confirmation-reset-url ""))))))


;;; To register new user from the REPL or application console
(defun cla-new-user-registration-REPL (email-id password)
  "registers the new user from the console without confirmation"
  (with-cla-db
    (when (find-user 'EMAIL-ID email-id)
      (error 'cla-login-error
             :format-control "Email-id ~A already exists in this CLA system."
             :format-arguments (list email-id)))
    (add-new-user email-id (hash-password password))
    (let ((login-data (find-user 'EMAIL-ID email-id)))
      (complete-user-registration (id login-data) (confirmation-reset-url login-data)) ;cheating
      login-data)))


(defun change-user-password (login-data old-password new-password)
  "Verifies the old PASSWORD in LOGIN-DATA with new PASSWORD and then updates the LOGIN-DATA with the new PASSWORD."
  (with-cla-db
    (cond ((not (and login-data old-password new-password))
	   (error 'cla-login-error :format-control "None of LOGIN-DATA, OLD-PASSWORD and NEW-PASSWORD can be NULL"))
	  ((string/= (hash-password old-password) (password login-data))
	   (error 'cla-login-error :format-control "Incorrect old password"))
	  (t
           (update-user (id login-data) 'password new-password)))))


;;; password reset request
(defun reset-password-request (login-data)
  "Generates the new RESET-URL and adds it to the LOGIN-DATA. Changes the user-status-code. The calling function will have to
take care of followup action like sending email(s) etc."
  (with-cla-db
    (setf (confirmation-reset-url login-data) (generate-unique-id)
          (user-status login-data) (user-status-code 'PASSWORD-RESET-REQUESTED))
    (update-login-data login-data)
    login-data))


(defun reset-password (login-data password reset-url)
  "Resets the PASSWORD. Verifies the RESET-URL and then sets the PASSWORD hash."
  (when (string= (confirmation-reset-url login-data) reset-url)
    (with-cla-db
        (setf (password login-data) (hash-password password)
              (confirmation-reset-url login-data) ""
              (user-status login-data) (user-status-code 'ACTIVE))
      (update-login-data login-data)
      login-data)))


;;; EOF
