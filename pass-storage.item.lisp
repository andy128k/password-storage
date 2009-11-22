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

(defgeneric entry-icon (o))
(defmethod entry-icon ((o entry-group))
  "gtk-directory")
(defmethod entry-icon ((o entry-generic))
  "gtk-file")

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

(defun edit-entry (entry parent-window title)
  (edit-object entry
	       parent-window
	       title
	       (entry-icon entry)
	       (entry-slots entry)))

