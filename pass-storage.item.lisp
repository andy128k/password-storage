(in-package :pass-storage)

(defclass item (gobject:g-object)
  ((name :accessor item-name :initarg :name :initform "")
   (login :accessor item-login :initarg :login :initform "")
   (password :accessor item-password :initarg :password :initform "")
   (url :accessor item-url :initarg :url :initform "")
   (email :accessor item-email :initarg :email :initform "")
   (comment :accessor item-comment :initarg :comment :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageItem"))

(gobject:register-object-type-implementation "PassStorageItem" item "GObject" nil nil)

(defclass group (gobject:g-object)
  ((name :accessor group-name :initarg :name :initform ""))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageGroup"))

(gobject:register-object-type-implementation "PassStorageGroup" group "GObject" nil nil)

(defgeneric is-group (o))
(defmethod is-group ((o item)) nil)
(defmethod is-group ((o group)) t)

(defun is-item (o)
  (not (is-group o)))

