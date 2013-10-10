;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

;;; Package Name : cla
;;; File Name : cookie.lisp
;;; Description : Here goes the Description


(in-package :cla)

(defparameter *auth-cookie-name* "auth" "Name of the CLA authentication cookie.")
(defparameter *auth-cookie-path* "/" "The path for which the auth cookie should be set.")
(defparameter *auth-cookie-domain* "localhost" "The domain for which the auth cookie should be set.")
(defparameter *auth-cookie-delimiter* "|")
(defparameter *auth-cookie-delimiter-regex* "\\|")

(defparameter *inactivity-timeout* 1200 "The session inactivity timeout in seconds")
(defvar *login-uri* "https://localhost/signin" "The login page URI. _is-auth-cookie-valid_ redirects to this URI if specified. Defaults to http://(host)/signin")



;;;
(defun set-auth-cookie (login-data)
  "Given a LOGIN-DATA object sets the auth cookie for that LOGIN-DATA by modifying the Hunchentoot cookies-out. If the login-data login timestamp or last accessed timestamp is zero, also sets the login-dataid cookie (for Apache level BI tracking). Internally calls CREATE-AUTH-COOKIE to create the auth cookie string."
  (hunchentoot:set-cookie *auth-cookie-name*
                          :value (make-auth-cookie login-data)
                          :expires (when (persistent-login login-data)
                                     (date-add :timestamp (get-universal-time) :month 1))
                          :path *auth-cookie-path*))


;;; FIXME - what happens if there is no login-instance found ? Need to list all use-cases
(defun make-auth-cookie (login-data &key (last-accessed-timestamp (get-universal-time)))
  "Given a LOGIN-DATA object, this function simply returns the auth cookie as a base64 encoded string. The format of the unencrypted auth cookie is the following:
16-byte-random-string|last-login-timestamp|last-accessed-timestamp|login-data-id|persistent-login|checksum"
  (let* ((login-instance (find-login-instance (id login-data) 0 remote-addr))
         (auth-cookie (format nil (concatenate 'string "~{~a~^" *auth-cookie-delimiter* "~}")
                             (list (make-random-string 16)
                                   (login-timestamp login-instance)
                                   last-accessed-timestamp
                                   (login-data-id login-instance)
                                   (persistent-login login-data)))))
    (setf auth-cookie (concatenate 'string auth-cookie *auth-cookie-delimiter* (sha1-hex-digest auth-cookie)))
    (encrypt-and-encode auth-cookie)))



;;; FIXME : generally rework
(defun is-auth-cookie-valid (&key (auth-cookie-value (cookie-in *auth-cookie-name*) auth-cookie-value-supplied-p)
			       (reset-last-accessed-timestamp t)
			       (delete-if-timeout t)
			       (honour-persistent-login t)
			       (honour-timeout-p nil)
			       (login-uri *login-uri*)
			       service-uri)

  "This function looks up the _*auth-cookie-name*_ cookie in the *request* variable set by hunchentoot. If the auth cookie is invalid or missing the funtion returns nil. If the auth cookie is valid the function returns an instance of the _login-data_ object.

Arguments:
# auth-cookie-value:
Explicity specify the auth-cookie-value. Default is to pick up the cookie value from *request*. You should NOT need to specify this parameter. It is used to ease the task of automated unit testing.

# reset-last-accessed-timestamp:
Each time this function is called, it modifies *reply* to send out a cookie with the auth-cookie containing the modified last-accessed-timestamp. This is required so that login-data sessions times out properly. If set to nil this behaviour will be subverted. You should NOT need such a thing. Default value is 't'.

# delete-if-timeout:
If 't', the auth cookie is deleted if the login-data session has timed out. If nil, nothing is done. Default is 'nil'

# honour-persistent-login:
If 't', timeout due to inactivity is ignored if the login-data cookie indicates a persistent login. If set to 'nil', the login-data is considered timed out even if he/she has made a persistent login.

# login-uri:
The URI to redirect the client to, if the cookie is invalid. Defaults to _*login-uri*_

# service-uri:
The URI the login page will redirect back to once the authentication succeeds. Defaults to http://(host)/(request-uri). You should not normally need to set this unless you want to login-data to be redirected back to a page he was not originally redirected away from (!). You might also have to use this if you need to make sure that the used is redirected back using HTTPS.
"
  (with-cla-db
    (handler-case
	(let* ((auth-cookie (decode-and-decrypt auth-cookie-value))) ; can raise a type-error if there is no cookie
	  (when (> (length auth-cookie) 0)
	    ;; TODO -- use bind instead of let* here
	    (let* ((timestamp (get-universal-time))
		   (cookie-elements (cl-ppcre:split *auth-cookie-delimiter-regex* auth-cookie))
		   (rand-string (first cookie-elements))
		   (last-login (parse-integer (second cookie-elements)))
		   (last-accessed-timestamp (parse-integer (third cookie-elements)))
		   (login-data-id (parse-integer-or-zero (fourth cookie-elements)))
                   (persistent-login (fifth cookie-elements))
		   (checksum (sixth cookie-elements))
		   (new-checksum (sha1-hex-digest (subseq auth-cookie 0 (search *auth-cookie-delimiter* auth-cookie :from-end t))))
		   (login-data nil))

	      (when (and (= (length cookie-elements) 8)
			 (string= checksum new-checksum)
			 (= (length rand-string) 16)
			 (<= last-login timestamp) ; login timestamp should not be in the future
			 (> login-data-id 0))
		;; (setf login-data (find-user 'EMAIL-ID email-id)) ;; why we need login-data ?
		;; checking for timeouts
                (when (and (and honour-timeout-p (< (+ last-accessed-timestamp *inactivity-timeout*) timestamp))
                           (not persistent-login)
                           delete-if-timeout
                           (not auth-cookie-value-supplied-p))
                  (destroy-auth-cookie)
                  (delete-login-instances login-data-id 0))
		t))))
      (error () nil))))


;;;
(defun destroy-auth-cookie ()
  "This function sets the auth-cookie to a blank string (thus effectively destroying it)."
  (hunchentoot:set-cookie *auth-cookie-name*
	      :value ""
	      :path *auth-cookie-path*))



;;; EOF
