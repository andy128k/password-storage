(in-package :pass-storage)

(defparameter +gp-alphabet+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890")

(defun random-character ()
  (aref +gp-alphabet+ (random (length +gp-alphabet+))))

(defun generate-password (&key (length 16))
  (let ((password (make-string length)))
    (iter (for i from 0 below length)
	  (setf (aref password i) (random-character)))
    password))

