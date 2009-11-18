(in-package :pass-storage)

(defclass item (gobject:g-object)
  ((name :accessor item-name :initarg :name)
   (login :accessor item-login :initarg :login)
   (password :accessor item-password :initarg :password)
   (url :accessor item-url :initarg :url)
   (email :accessor item-email :initarg :email)
   (comment :accessor item-comment :initarg :comment))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageItem"))

(gobject:register-object-type-implementation "PassStorageItem" item "GObject" nil nil)

(defclass group (gobject:g-object)
  ((name :accessor group-name :initarg :name))
  (:metaclass gobject:gobject-class)
  (:g-type-name . "PassStorageGroup"))

(gobject:register-object-type-implementation "PassStorageGroup" group "GObject" nil nil)

(defgeneric is-group (o))
(defmethod is-group ((o item)) nil)
(defmethod is-group ((o group)) t)

(defun is-item (o)
  (not (is-group o)))

