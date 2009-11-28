(in-package :pass-storage)

;;
;; base class
;;

(defclass entry (gobject:g-object)
  ((name :accessor entry-name :initarg :name :initform "")
   (description :accessor entry-description :initarg :description :initform "")
   (updated :accessor entry-updated :initarg :updated :initform nil))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntry"))

(gobject:register-object-type-implementation "PassStorageEntry" entry "GObject" nil nil)

;;
;; group
;;

(defclass entry-group (entry)
  ()
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryGroup"))

(gobject:register-object-type-implementation "PassStorageEntryGroup" entry-group "GObject" nil nil)

;;
;; generic
;;

(defclass entry-generic (entry)
  ((hostname :accessor generic-hostname :initarg :hostname :initform "")
   (username :accessor generic-username :initarg :username :initform "")
   (password :accessor generic-password :initarg :password :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryGeneric"))

(gobject:register-object-type-implementation "PassStorageEntryGeneric" entry-generic "GObject" nil nil)

;;
;; credit card
;;

(defclass entry-creditcard (entry)
  ((cardtype :accessor creditcard-cardtype :initarg :cardtype :initform "")
   (cardnumber :accessor creditcard-cardnumber :initarg :cardnumber :initform "")
   (expirydate :accessor creditcard-expirydate :initarg :expirydate :initform "")
   (ccv :accessor creditcard-ccv :initarg :ccv :initform "")
   (pin :accessor creditcard-pin :initarg :pin :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryCreditcard"))

(gobject:register-object-type-implementation "PassStorageEntryCreditcard" entry-creditcard "GObject" nil nil)

;;
;; crypto key
;;

(defclass entry-cryptokey (entry)
  ((hostname :accessor cryptokey-hostname :initarg :hostname :initform "")
   (certificate :accessor cryptokey-certificate :initarg :certificate :initform "")
   (keyfile :accessor cryptokey-keyfile :initarg :keyfile :initform "")
   (password :accessor cryptokey-password :initarg :password :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryCryptokey"))

(gobject:register-object-type-implementation "PassStorageEntryCryptokey" entry-cryptokey "GObject" nil nil)

;;
;; database
;;

(defclass entry-database (entry)
  ((hostname :accessor database-hostname :initarg :hostname :initform "")
   (username :accessor database-username :initarg :username :initform "")
   (password :accessor database-password :initarg :password :initform "") 
   (database :accessor database-database :initarg :database :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryDatabase"))

(gobject:register-object-type-implementation "PassStorageEntryDatabase" entry-database "GObject" nil nil)

;;
;; door
;;

(defclass entry-door (entry)
  ((location :accessor door-location :initarg :location :initform "")
   (code :accessor door-code :initarg :code :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryDoor"))

(gobject:register-object-type-implementation "PassStorageEntryDoor" entry-door "GObject" nil nil)

;;
;; email
;;

(defclass entry-email (entry)
  ((email :accessor email-email :initarg :email :initform "")
   (hostname :accessor email-hostname :initarg :hostname :initform "")
   (username :accessor email-username :initarg :username :initform "")
   (password :accessor email-password :initarg :password :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryEmail"))

(gobject:register-object-type-implementation "PassStorageEntryEmail" entry-email "GObject" nil nil)

;;
;; ftp
;;

(defclass entry-ftp (entry)
  ((hostname :accessor ftp-hostname :initarg :hostname :initform "")
   (port :accessor ftp-port :initarg :port :initform "")
   (username :accessor ftp-username :initarg :username :initform "")
   (password :accessor ftp-password :initarg :password :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryFtp"))

(gobject:register-object-type-implementation "PassStorageEntryFtp" entry-ftp "GObject" nil nil)

;;
;; phone
;;

(defclass entry-phone (entry)
  ((phonenumber :accessor phone-phonenumber :initarg :phonenumber :initform "")
   (pin :accessor phone-pin :initarg :pin :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryPhone"))

(gobject:register-object-type-implementation "PassStorageEntryPhone" entry-phone "GObject" nil nil)

;;
;; shell
;;

(defclass entry-shell (entry)
  ((hostname :accessor shell-hostname :initarg :hostname :initform "")
   (domain :accessor shell-domain :initarg :domain :initform "")
   (username :accessor shell-username :initarg :username :initform "")
   (password :accessor shell-password :initarg :password :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryShell"))

(gobject:register-object-type-implementation "PassStorageEntryShell" entry-shell "GObject" nil nil)

;;
;; website
;;

(defclass entry-website (entry)
  ((url :accessor website-url :initarg :url :initform "")
   (username :accessor website-username :initarg :username :initform "")
   (password :accessor websize-password :initarg :password :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageEntryWebsite"))

(gobject:register-object-type-implementation "PassStorageEntryWebsite" entry-website "GObject" nil nil)

;;
;; functions
;;

(defgeneric is-group (o))
(defmethod is-group ((o entry-group)) t)
(defmethod is-group (o) nil)

(defgeneric entry-title (o))
(defmethod entry-title ((o entry-group))
  "group")
(defmethod entry-title ((o entry-generic))
  "generic entry")
(defmethod entry-title ((o entry-creditcard))
  "credit card")
(defmethod entry-title ((o entry-cryptokey))
  "crypto key")
(defmethod entry-title ((o entry-database))
  "database")
(defmethod entry-title ((o entry-door))
  "door")
(defmethod entry-title ((o entry-email))
  "e-mail")
(defmethod entry-title ((o entry-ftp))
  "FTP")
(defmethod entry-title ((o entry-phone))
  "phone")
(defmethod entry-title ((o entry-shell))
  "shell")
(defmethod entry-title ((o entry-website))
  "website")

(defgeneric entry-icon (o))
(defmethod entry-icon ((o entry-group))
  "gtk-directory")
(defmethod entry-icon ((o entry-generic))
  "gtk-file")
(defmethod entry-icon ((o entry-creditcard))
  "ps-stock-entry-creditcard")
(defmethod entry-icon ((o entry-cryptokey))
  "ps-stock-entry-keyring")
(defmethod entry-icon ((o entry-database))
  "ps-stock-entry-database")
(defmethod entry-icon ((o entry-door))
  "ps-stock-entry-door")
(defmethod entry-icon ((o entry-email))
  "ps-stock-entry-email")
(defmethod entry-icon ((o entry-ftp))
  "ps-stock-entry-ftp")
(defmethod entry-icon ((o entry-phone))
  "ps-stock-entry-phone")
(defmethod entry-icon ((o entry-shell))
  "ps-stock-entry-shell")
(defmethod entry-icon ((o entry-website))
  "ps-stock-entry-website")

(defgeneric entry-slots (entry))

(defmethod entry-slots ((entry entry-group))
  '((name "Name" :entry :required)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-generic))
  '((name "Name" :entry :required)
    (username "Username" :entry)
    (password "Password" :entry)
    (hostname "Hostname" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-creditcard))
  '((name "Name" :entry :required)
    (cardtype "Card type" :entry)
    (cardnumber "Card number" :entry)
    (expirydate "Expiry date" :entry)
    (ccv "CCV number" :entry)
    (pin "PIN" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-cryptokey))
  '((name "Name" :entry :required)
    (hostname "Hostname" :entry)
    (certificate "Certificate" :entry)
    (keyfile "Key file" :entry)
    (password "Password" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-database))
  '((name "Name" :entry :required)
    (username "Username" :entry)
    (password "Password" :entry)
    (hostname "Hostname" :entry)
    (database "Database" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-door))
  '((name "Name" :entry :required)
    (location "Location" :entry)
    (code "Code" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-email))
  '((name "Name" :entry :required)
    (email "E-mail" :entry)
    (hostname "Hostname" :entry)
    (username "Username" :entry)
    (password "Password" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-ftp))
  '((name "Name" :entry :required)
    (hostname "Hostname" :entry)
    (port "Port" :entry)
    (username "Username" :entry)
    (password "Password" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-phone))
  '((name "Name" :entry :required)
    (phonenumber "Number" :entry)
    (pin "PIN" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-shell))
  '((name "Name" :entry :required) 
    (hostname "Hostname" :entry)
    (domain "Domain" :entry)
    (username "Username" :entry)
    (password "Password" :entry)
    (description "Description" :area)))

(defmethod entry-slots ((entry entry-website))
  '((name "Name" :entry :required) 
    (url "URL" :entry)
    (username "Username" :entry)
    (password "Password" :entry)
    (description "Description" :area)))

(defun edit-entry (entry parent-window title)
  (edit-object entry
	       parent-window
	       (concatenate 'string title " " (entry-title entry))
	       (entry-icon entry)
	       (entry-slots entry)))

