;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

;;; Package Name : cla
;;; File Name : helpers.lisp
;;; Description : Here goes the Description


(in-package :cla)

;;;

(defconstant +user-status-codes+ '((UNCONFIRMED . 0)
                                   (CONFIRMED . 2)
                                   (ACTIVE . 3)
                                   (PASSWORD-RESET-REQUESTED . 5)
                                   (BLOCKED . 10)
                                   (DELETED . 99)))


(defun user-status-code (status)
  (rest (assoc status +user-status-codes+)))


;;; helper function to parse an integer
;;; returns zero if num is nil else simply parses
(defmacro parse-integer-or-zero (num)
  `(if ,num
      (parse-integer ,num)
      0))


;;; helper function to do some date arithmetic
(defun date-add (&key (timestamp (get-universal-time))
                   (sec 0) (min 0) (hour 0) (date 0) (month 0) (year 0))
  (multiple-value-bind (sec1 min1 hour1 date1 month1 year1)
      (decode-universal-time timestamp)
    (multiple-value-bind (year2 month2 date2 hour2 min2 sec2)
        (date-calc:add-delta-ymdhms year1 month1 date1 hour1 min1 sec1 year month date hour min sec)
      (encode-universal-time sec2 min2 hour2 date2 month2 year2))))


;;; from CliKi/Ironclad -- SHA1
(defun sha1-hex-digest (password)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1
    (ironclad:ascii-string-to-byte-array password))))


(defmacro hash-password (password)
  `(sha1-hex-digest ,password))


;;; helper function to generate a random
;;; string comprising a-zA-Z0-9 characters of given length
(defun make-random-string (length)
  (let ((res "")
	(temp 0)
	(c nil))
    (dotimes (i length)
      (setf temp (+ 1 (random 62)))
      (if (and (>= temp 1) (<= temp 26))
	  (setf c (string (code-char (+ 96 temp))))
	  (if (and (>= temp 27) (<= temp 52))
	      (setf c (string (code-char (+ 64 -26 temp))))
	      (setf c (string (code-char (+ 47 -52 temp))))))
      (setf res (concatenate 'string res c)))
    (return-from make-random-string res)))


;;; Creating a key for the first time registration user.
(defun generate-unique-id (&key (host-name "") (pid 0))
  (cl-base64:string-to-base64-string
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :md5
     (ironclad:ascii-string-to-byte-array
      (format nil "~A~A~A~A~A"
              host-name
              (random 1000000)
              pid
              (get-universal-time)
              (random 1000000)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-cla-db (&body body)
    `(pooler:with-pool (clsql:*default-database* +cla-db-pool+)
       ,@body)))


;;; EOF
