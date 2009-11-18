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

      (zlib:uncompress (subseq buffer 28
			       (- size padlen))))))

(defun read-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((v (make-array (list (file-length stream)) :element-type '(unsigned-byte 8))))
      (read-sequence v stream)
      v)))

(defun write-file (filename data)
  (with-open-file (stream filename :direction :output :if-exists :overwrite :element-type '(unsigned-byte 8))
    (write-sequence data stream)))

(defun load-revelation-file (filename password)
  (s-xml:parse-xml-string
   (babel:octets-to-string 
    (decrypt-file (read-file filename) password)
    :encoding :utf-8)
   :output-type :lxml))

; (write-file "output"
; 	    (decrypt-file (read-file "passwords") "Nd3e"))

