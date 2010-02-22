(in-package :pass-storage)

(defun decrypt-file (buffer password)
  
  (setf password
	(adjust-array
	 (babel:string-to-octets password :encoding :utf-8)
	 '(32)))
  
  (let ((size (length buffer)))
    (when (or (< size 28)
	      (/= 0 (mod (- size 28) 16)))
      (error "wrong size"))

    ;; TODO: check version

    ;; decrypt the initial vector for CBC decryption
    (ironclad:decrypt-in-place
     (ironclad:make-cipher :AES
			   :mode :ECB
			   :key password)
     buffer
     :start 12
     :end 28)

    (ironclad:decrypt-in-place
     (ironclad:make-cipher :AES
			   :mode :CBC
			   :key password
			   :initialization-vector (subseq buffer 12 28))
     buffer
     :start 28)

    ;; decompress data
    (let ((padlen (aref buffer (- size 1))))
      (unless (<= 1 padlen 16)
	(error "wrong pad length"))

      (when (find padlen buffer :start (- size padlen) :test '/=)
	(error "padding is corrupted"))

      (cl-z:uncompress-sequence buffer :start 28 :end (- size padlen)))))

(defun read-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((v (make-array (list (file-length stream)) :element-type '(unsigned-byte 8))))
      (read-sequence v stream)
      v)))

(defun load-revelation-file (filename password)
  (s-xml:parse-xml-string
   (babel:octets-to-string 
    (decrypt-file (read-file filename) password)
    :encoding :utf-8)
   :output-type :lxml))

(defun encrypt-file (buffer password)

  (setf password
	(adjust-array
	 (babel:string-to-octets password :encoding :utf-8)
	 '(32)))

  (let* ((buffer (cl-z:compress-sequence buffer))
	 (oldlen (length buffer))
	 (padlen (- 16 (mod oldlen 16)))
	 (newlen (+ oldlen padlen)))

    (setf buffer (adjust-array buffer (list newlen) :fill-pointer newlen))
    (setf buffer (coerce buffer '(simple-array (unsigned-byte 8) (*))))
    (fill buffer padlen :start oldlen)
    
    (let ((initialization-vector (make-array '(16) :element-type '(unsigned-byte 8))))

      (iter (for i from 0 to 15)
	    (setf (aref initialization-vector i) (coerce (random 256) '(unsigned-byte 8))))
      
      (ironclad:encrypt-in-place
       (ironclad:make-cipher :AES
			     :mode :CBC
			     :key password
			     :initialization-vector initialization-vector)
       buffer)

      (ironclad:encrypt-in-place
       (ironclad:make-cipher :AES
			     :mode :ECB
			     :key password)
       initialization-vector)

      (concatenate '(vector (unsigned-byte 8))
		   #(#x72 #x76 #x6C #x00 ; magic string
		     #x01                ; data version
		     #x00                ; separator
		     #x00 #x04 #x0B      ; app version
		     #x00 #x00 #x00)     ; separator
		   initialization-vector
		   buffer
		   ))))

(defun write-file (filename data)
  (with-open-file (stream filename :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))
    (write-sequence data stream)))

(defun save-revelation-file (filename password xml)
  (write-file filename
	      (encrypt-file

	       (babel:string-to-octets
		(concatenate 'string "<?xml version='1.0' encoding='utf-8'?>"
			     (s-xml:print-xml-string xml :pretty t))
		:encoding :utf-8)

	       password)))

