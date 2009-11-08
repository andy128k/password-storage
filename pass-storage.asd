;; -*- mode: Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage pass-storage-system
  (:use :common-lisp :asdf))

(in-package :pass-storage-system)

(defsystem "pass-storage"
  :description "Password storage utility"
  :version "0.2"
  :author "Andrey Kutejko <andy128k@gmail.com>"
  :licence "LGPL"
  :depends-on (:cffi :trivial-garbage :cl-gtk2-gtk)
  :components ((:file "pass-storage.package")
               (:file "pass-storage.utils" :depends-on ("pass-storage.package"))
               (:file "pass-storage.item" :depends-on ("pass-storage.package"))
               (:file "pass-storage.item-edit" :depends-on ("pass-storage.item"))
               (:file "pass-storage.main" :depends-on ("pass-storage.utils"
						       "pass-storage.item"
						       "pass-storage.item-edit"))))

