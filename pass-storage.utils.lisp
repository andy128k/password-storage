(in-package :pass-storage)

(defmacro lambda-u (&body body)
  (let ((p (gensym)))
    `(lambda (&rest ,p)
       (declare (ignore ,p))
       ,@body)))

(defmacro connect (builder &rest connects)
  `(gtk:builder-connect-signals-simple
    ,builder
    (list
     ,@(loop
	  for c in connects
	  for event = (first c)
	  for func = (second c)
	  for args = (cddr c)
	  collect
	    (list 'list event `(lambda-u (funcall (function ,func) ,@args)))))))

(defun ask (message)
  (eql (gtk:show-message message :buttons :yes-no :message-type :warning)
       :yes))

(defun make-std-dialog (parent-window title stock-icon content)
  (let ((dlg (make-instance 'gtk:dialog
			    :border-width 8
			    :modal t
			    :resizable t
			    :window-position :center-on-parent
			    :title title
			    :has-separator nil
			    :type-hint :dialog
			    :skip-taskbar_hint t
			    :skip-pager-hint t
			    :gravity :center
			    :transient-for parent-window)))

    (setf (gtk:gtk-window-icon dlg)
	  (gtk:widget-render-icon dlg stock-icon :dialog ""))

    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-ok" :ok)
    (setf (gtk:dialog-default-response dlg) :ok)
    (gtk:dialog-set-response-sensitive dlg :ok nil)
    
    (gtk:container-add (gtk:dialog-content-area dlg) content)
    
    dlg))

(defun std-dialog-run (dlg)
  (prog2
      (gtk:widget-show dlg :all t)
      (eql (gtk:dialog-run dlg) :ok)
      (gtk:widget-hide dlg)))

(defun ask-string (parent-window title icon caption &key (validate nil) (start-value ""))
  (gtk:let-ui
   (gtk:h-box
    :var box
    :border-width 5
    :spacing 8
    
    (gtk:label :label caption) :expand nil
    (gtk:entry :var entry :activates-default t :text start-value) :expand t)
   
   (let ((dlg (make-std-dialog parent-window title icon box)))

     (flet ((validator (entry)
	      (when validate
		(let ((msg (funcall validate (gtk:entry-text entry))))
		  (cond
		    (msg
		     (setf (gtk:entry-secondary-icon-stock entry) "gtk-no"
			   (gtk:entry-secondary-icon-tooltip-text entry) msg)
		     (gtk:dialog-set-response-sensitive dlg :ok nil))
		    (t
		     (setf (gtk:entry-secondary-icon-stock entry) "gtk-yes"
			   (gtk:entry-secondary-icon-tooltip-text entry) "")
		     (gtk:dialog-set-response-sensitive dlg :ok t)))))))
     
       (gobject:g-signal-connect entry "changed" #'validator)

       (validator entry)
       
       (prog1
	   (when (std-dialog-run dlg)
	     (gtk:entry-text entry))
	   (gtk:object-destroy dlg))))))

