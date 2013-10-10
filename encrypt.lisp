;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

;;; Package Name : cla
;;; File Name : encrypt.lisp
;;; Description : Here goes the Description

(in-package :cla)

;;; FIXME how to fetch this one from some secure location ?
(defvar *key* nil "The AES key used for encryption/decryption of the auth cookie. Very important.")

(defun construct-cipher-for-encrypt (text-string)
  ""
  (setf *key* (ironclad:make-cipher :blowfish :mode :ecb :key (ironclad:ascii-string-to-byte-array text-string))))


;;; helper function to encrypt a string using whatever cypher/key that
;;; has been finally chosen. The return value is a base64 encoded string
(defun encrypt-and-encode (data &key (key *key*))
  (let ((data-length (length data))
	res-array
	data-array)
    ;; if the data length is not a multiple of sixteen, pad it up with null
    (let ((padding (mod data-length 16)))
      (when (not (= padding 0))
        (with-output-to-string (s)
          (write-string data s)
          (dotimes (x padding)
            (write-char #\Nul s)))
        (setf data-length (length data))))

    (setf data-array (babel:string-to-octets data)) ; initializing the plain text array
    (setf res-array (make-array data-length :element-type '(unsigned-byte 8))) ; initializing the cipher text array

    ;; now break the data into chunks of 16 and encode each one of them
    (do ((i 0 (+ i 16)))
	((= i data-length) ; end test form
	 (cl-base64:string-to-base64-string (babel:octets-to-string res-array)))
      (ironclad:encrypt key data-array res-array :plaintext-start i :plaintext-end (+ i 16) :ciphertext-start i))))




;;; function to base64 unencode and decrypt the data
(defun decode-and-decrypt (data &key (key *key*))
  (when (not data)
    (return-from decode-and-decrypt nil))

  (let* ((data (cl-base64:base64-string-to-string data))
	 (l (length data))
	 (data-array (ironclad:ascii-string-to-byte-array data))
	 (res-array (make-array l :element-type '(unsigned-byte 8))))

    (when (/= (mod l 16) 0)
      (error 'cla-error :format-control "Decoded data length is not a multiple of 16. Possible data corruption."))

    (do ((i 0 (+ i 16)))
	((>= i l)
	 ;; removing all nulls before returning ths string
	 (cl-ppcre:regex-replace (concatenate 'string (string #\Nul) ".*")
				 (babel:octets-to-string res-array)
				 ""))
      (ironclad:decrypt key data-array res-array :ciphertext-start i :ciphertext-end (+ i 16) :plaintext-start i))))


;;; EOF
