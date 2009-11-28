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

;;
;; helpers
;;

(defmacro define-entry (name g-name title icon &rest slots)
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
	       (quote ,slots)))))

;;
;; subtypes
;;

(define-entry entry-group "PassStorageEntryGroup"
  "group"
  "gtk-directory")

(define-entry entry-generic "PassStorageEntryGeneric"
  "generic entry"
  "gtk-file"
  (hostname "Hostname" :entry)
  (username "Username" :entry)
  (password "Password" :entry))

(define-entry entry-creditcard "PassStorageEntryCreditcard"
  "credit card"
  "ps-stock-entry-creditcard"
  (cardtype "Card type" :entry)
  (cardnumber "Card number" :entry)
  (expirydate "Expiry date" :entry)
  (ccv "CCV number" :entry)
  (pin "PIN" :entry))

(define-entry entry-cryptokey "PassStorageEntryCryptokey"
  "crypto key"
  "ps-stock-entry-keyring"
  (hostname "Hostname" :entry)
  (certificate "Certificate" :entry)
  (keyfile "Key file" :entry)
  (password "Password" :entry))

(define-entry entry-database "PassStorageEntryDatabase"
  "database"
  "ps-stock-entry-database"
  (hostname "Hostname" :entry)
  (username "Username" :entry)
  (password "Password" :entry)
  (database "Database" :entry))

(define-entry entry-door "PassStorageEntryDoor"
  "door"
  "ps-stock-entry-door"
  (location "Location" :entry)
  (code "Code" :entry))

(define-entry entry-email "PassStorageEntryEmail"
  "e-mail"
  "ps-stock-entry-email"
  (email "E-mail" :entry)
  (hostname "Hostname" :entry)
  (username "Username" :entry)
  (password "Password" :entry))

(define-entry entry-ftp "PassStorageEntryFtp"
  "FTP"
  "ps-stock-entry-ftp"
  (hostname "Hostname" :entry)
  (port "Port" :entry)
  (username "Username" :entry)
  (password "Password" :entry))

(define-entry entry-phone "PassStorageEntryPhone"
  "phone"
  "ps-stock-entry-phone"
  (phonenumber "Number" :entry)
  (pin "PIN" :entry))

(define-entry entry-shell "PassStorageEntryShell"
  "shell"
  "ps-stock-entry-shell"
  (hostname "Hostname" :entry)
  (domain "Domain" :entry)
  (username "Username" :entry)
  (password "Password" :entry))

(define-entry entry-website "PassStorageEntryWebsite"
  "website"
  "ps-stock-entry-website"
  (url "URL" :entry)
  (username "Username" :entry)
  (password "Password" :entry))

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

