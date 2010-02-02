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
  :depends-on (:cffi :trivial-garbage :cl-gtk2-gtk :babel :ironclad :cl-z :s-xml :cl-binary-location :cl-fad)
  :serial t
  :components ((:file "pass-storage.package")
               (:file "pass-storage.utils")
               (:file "pass-storage.xml")
               (:file "pass-storage.item")
               (:file "pass-storage.load")
               (:file "pass-storage.config")
               (:file "pass-storage.main")))

