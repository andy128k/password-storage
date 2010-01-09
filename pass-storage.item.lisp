(in-package :pass-storage)

;;
;; base class (abstract)
;;

(defclass entry (gobject:g-object)
  ((name :accessor entry-name :initarg :name :initform "")
   (description :accessor entry-description :initarg :description :initform "")
   (updated :accessor entry-updated :initarg :updated :initform nil))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntry"))

(gobject:register-object-type-implementation "PassStorageEntry" entry "GObject" nil nil)

(defgeneric entry-slots (entry))
(defmethod entry-slots ((entry entry))
  '((name "Name" :entry :required) 
    (description "Description" :area)))

(defgeneric load-entry (xml-name xml-node))
(defgeneric save-entry (entry))

(defgeneric entry-has-text (entry text))

;;
;; helpers
;;

(defmacro define-entry (name g-name xml-name title icon &rest slots)
  `(progn
     (defclass ,name (entry)
       (,@(iter (for (slot title type) in slots)
		(collect `(,slot :initform "")))
	(title :allocation :class :initform ,title)
	(icon :allocation :class :initform ,icon))
       (:metaclass gobject:gobject-class)
       (:g-type-name . ,g-name))

     (gobject:register-object-type-implementation ,g-name ,name "GObject" nil nil)

     (defmethod entry-slots ((entry ,name))
       (append (call-next-method entry)
	       (quote ,slots)))

     (defmethod load-entry ((xml-name (eql ,(intern xml-name 'keyword))) xml-node)
       (let ((entry (make-instance (quote ,name)
				   :name (entry-node-get-value xml-node :|name|)
				   :description (entry-node-get-value xml-node :|description|))))
	 ,@(iter (for (slot title type field-name) in slots)
		 (collect `(setf (slot-value entry (quote ,slot))
				 (entry-node-get-value xml-node :|field| ,field-name))))
	 entry))

     (defmethod save-entry ((entry ,name))
       (list
	(list :|entry| :|type| ,xml-name)
	(list :|name| (slot-value entry 'name))
	(list :|description| (slot-value entry 'description))
	,@(iter (for (slot title type field-name) in slots)
		(collect
		 `(list (list :|field| :|id| ,field-name) (slot-value entry ',slot))))))

     (defmethod entry-has-text ((entry ,name) text)
       (or
	(search text (entry-name entry))
	(search text (entry-description entry))
	,@(iter (for (slot title type field-name) in slots)
		(collect `(search text (slot-value entry (quote ,slot)))))))))

(defgeneric entry-get-name (entry))
(defgeneric entry-get-password (entry))

;;
;; subtypes
;;

(define-entry entry-group "PassStorageEntryGroup" "folder"
  "group"
  "gtk-directory")

(defmethod entry-get-name ((entry entry-group))
  nil)
(defmethod entry-get-password ((entry entry-group))
  nil)

(define-entry entry-generic "PassStorageEntryGeneric" "generic"
  "generic entry"
  "gtk-file"
  (hostname    "Hostname"    :entry "generic-hostname")
  (username    "Username"    :entry "generic-username")
  (password    "Password"    :entry "generic-password"))

(defmethod entry-get-name ((entry entry-generic))
  (slot-value entry 'username))
(defmethod entry-get-password ((entry entry-generic))
  (slot-value entry 'password))

(define-entry entry-creditcard "PassStorageEntryCreditcard" "creditcard"
  "credit card"
  "ps-stock-entry-creditcard"
  (cardtype    "Card type"   :entry "creditcard-cardtype")
  (cardnumber  "Card number" :entry "creditcard-cardnumber")
  (expirydate  "Expiry date" :entry "creditcard-expirydate")
  (ccv         "CCV number"  :entry "creditcard-ccv")
  (pin         "PIN"         :entry "generic-pin"))

(defmethod entry-get-name ((entry entry-creditcard))
  (slot-value entry 'cardnumber))
(defmethod entry-get-password ((entry entry-creditcard))
  (slot-value entry 'pin))

(define-entry entry-cryptokey "PassStorageEntryCryptokey" "cryptokey"
  "crypto key"
  "ps-stock-entry-keyring"
  (hostname    "Hostname"    :entry "generic-hostname")
  (certificate "Certificate" :entry "generic-certificate")
  (keyfile     "Key file"    :entry "generic-keyfile")
  (password    "Password"    :entry "generic-password"))

(defmethod entry-get-name ((entry entry-cryptokey))
  (slot-value entry 'hostname))
(defmethod entry-get-password ((entry entry-cryptokey))
  (slot-value entry 'password))

(define-entry entry-database "PassStorageEntryDatabase" "database"
  "database"
  "ps-stock-entry-database"
  (hostname    "Hostname"    :entry "generic-hostname")
  (username    "Username"    :entry "generic-username")
  (password    "Password"    :entry "generic-password")
  (database    "Database"    :entry "generic-database"))

(defmethod entry-get-name ((entry entry-database))
  (slot-value entry 'database))
(defmethod entry-get-password ((entry entry-database))
  (slot-value entry 'password))

(define-entry entry-door "PassStorageEntryDoor" "door"
  "door"
  "ps-stock-entry-door"
  (location    "Location"    :entry "generic-location")
  (code        "Code"        :entry "generic-code"))

(defmethod entry-get-name ((entry entry-door))
  (slot-value entry 'location))
(defmethod entry-get-password ((entry entry-door))
  (slot-value entry 'code))

(define-entry entry-email "PassStorageEntryEmail" "email"
  "e-mail"
  "ps-stock-entry-email"
  (email       "E-mail"      :entry "generic-email")
  (hostname    "Hostname"    :entry "generic-hostname")
  (username    "Username"    :entry "generic-username")
  (password    "Password"    :entry "generic-password"))

(defmethod entry-get-name ((entry entry-email))
  (slot-value entry 'username))
(defmethod entry-get-password ((entry entry-email))
  (slot-value entry 'password))

(define-entry entry-ftp "PassStorageEntryFtp" "ftp"
  "FTP"
  "ps-stock-entry-ftp"
  (hostname    "Hostname"    :entry "generic-hostname")
  (port        "Port"        :entry "generic-port")
  (username    "Username"    :entry "generic-username")
  (password    "Password"    :entry "generic-password"))

(defmethod entry-get-name ((entry entry-ftp))
  (slot-value entry 'username))
(defmethod entry-get-password ((entry entry-ftp))
  (slot-value entry 'password))

(define-entry entry-phone "PassStorageEntryPhone" "phone"
  "phone"
  "ps-stock-entry-phone"
  (phonenumber "Number"      :entry "phone-phonenumber")
  (pin         "PIN"         :entry "generic-pin"))

(defmethod entry-get-name ((entry entry-phone))
  (slot-value entry 'phonenumber))
(defmethod entry-get-password ((entry entry-phone))
  (slot-value entry 'pin))

(define-entry entry-shell "PassStorageEntryShell" "shell"
  "shell"
  "ps-stock-entry-shell"
  (hostname    "Hostname"    :entry "generic-hostname")
  (domain      "Domain"      :entry "generic-domain")
  (username    "Username"    :entry "generic-username")
  (password    "Password"    :entry "generic-password"))

(defmethod entry-get-name ((entry entry-shell))
  (slot-value entry 'username))
(defmethod entry-get-password ((entry entry-shell))
  (slot-value entry 'password))

(define-entry entry-website "PassStorageEntryWebsite" "website"
  "website"
  "ps-stock-entry-website"
  (url         "URL"         :entry "generic-url")
  (username    "Username"    :entry "generic-username")
  (password    "Password"    :entry "generic-password"))

(defmethod entry-get-name ((entry entry-website))
  (slot-value entry 'username))
(defmethod entry-get-password ((entry entry-website))
  (slot-value entry 'password))

;;
;; functions
;;

(defgeneric is-group (o))
(defmethod is-group ((o entry-group)) t)
(defmethod is-group (o) nil)

(defun entry-title (o)
  (slot-value o 'title))

(defun entry-icon (o)
  (slot-value o 'icon))

(defun edit-entry (entry parent-window title)
  (edit-object entry
	       parent-window
	       (concatenate 'string title " " (entry-title entry))
	       (entry-icon entry)
	       (entry-slots entry)))

