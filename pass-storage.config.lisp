(in-package :pass-storage)

(defstruct config
  default-file
  search-in-secrets
  show-secrets-on-preview)

(defvar *config* nil)

(defun config-path ()
  (pathname (glib:build-filename
	     (glib:get-user-config-dir)
	     "PassStorage.conf")))

(defun load-config ()
  (with-open-stream (stream (open (config-path)
				  :direction :input
				  :if-does-not-exist nil))
    (setf *config*
	  (if stream
	      (read stream)
	      (make-config)))))

(defun save-config ()
  (with-open-stream (stream (open (config-path)
				  :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create))
    (format stream "~S" *config*)))

