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

(defgeneric entry-icon (o))
(defmethod entry-icon ((o entry-group))
  "gtk-directory")
(defmethod entry-icon ((o entry-generic))
  "gtk-file")
(defmethod entry-icon ((o entry-creditcard))
  "stock_creditcard")
(defmethod entry-icon ((o entry-cryptokey))
  "stock_keyring")
(defmethod entry-icon ((o entry-database))
  "stock_data-sources")

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

(defun edit-entry (entry parent-window title)
  (edit-object entry
	       parent-window
	       (concatenate 'string title " " (entry-title entry))
	       (entry-icon entry)
	       (entry-slots entry)))

