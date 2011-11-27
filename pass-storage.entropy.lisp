(in-package :pass-storage)

(defmacro define-characrter-classes (&rest classes)
  (labels ((ensure-code (ch)
	     (if (numberp ch)
		 ch
		 (char-code ch)))
	   (range-size (b)
	     (if (listp b)
		 (- (ensure-code (second b)) (ensure-code (first b)) -1)
		 1))
	   (class-size (bounds)
	     (iter (for b in bounds)
		   (summing (range-size b)))))

    (let ((total-characrters (iter (for (class . bounds) in classes)
				   (summing (class-size bounds)))))
      (when (/= total-characrters 256)
	(error "total-characrters = ~A (256 required)" total-characrters)))

    `(progn
       (defun detect-character-class (ch)
	 (let ((code (char-code ch)))
	   (cond
	     ,@(iter (for (class . bounds) in classes)
		     (appending
		      (iter (for b in bounds)
			    (collect (if (listp b)
					 `((<= ,(ensure-code (first b)) code ,(ensure-code (second b))) ',class)
					 `((= code) ',class))))))
	     (:otherwise (error "Character code ~A is out of bounds" code)))))
       
       ,@(iter (for (class . bounds) in classes)
	       (collect
		`(setf (get ',class 'character-class-capacity)
		       ,(class-size bounds))))
       
       (defun character-class-capacity (class)
	 (get class 'character-class-capacity)))))

(define-characrter-classes
    (control
     (0 #X1F))
    
    (number
     (#\0 #\9))
  
    (upper
     (#\A #\Z))

    (lower
     (#\a #\z))

    (punct-1
     #X20
     #X21
     (#X23 #X26)
     (#X28 #X2B)
     #X2D
     #X2F
     #X3D
     #X40
     #X5E
     #X5F)

    (punct-2
     #X22
     #X27
     #X2C
     #X2E
     (#X3A #X3C)
     #X3E
     #X3F
     (#X5B #X5D)
     #X60
     (#X7B #X7F))

    (extended
     (#X80 #XFF)))

(defun character-distance (ch1 ch2)
  ;; TODO: check in known sequences... qwertyuiop, PI digits...
  (abs (- (char-code ch1)
	  (char-code ch2))))

(defun password-entropy (passw)
  (let (classes
	(char-count (make-hash-table)) ;; to count characters quantities
	(distances (make-hash-table))) ;; to collect differences between adjacent characters
    
    (let ((eff-len
	   (iter (for nc in-string passw)
		 (for prev-nc previous nc)
		 
		 (pushnew (detect-character-class nc) classes)
		 
		 ;; value/factor for increment effective length
		 (let ((dw
			(if-first-time 1
				       (let ((d (character-distance nc prev-nc)))
					 (setf (gethash d distances)
					       (1+ (gethash d distances 0))))))
		       (cw (setf (gethash nc char-count)
				 (1+ (gethash nc char-count 0)))))
		   (summing (/ 1 dw cw))))))
      
      ;; Password complexity index
      (let ((pci
	     (iter (for c in classes)
		   (summing (character-class-capacity c)))))
	
	(if (/= 0 pci)
	    (* (log pci 2) eff-len)
	    0)))))

