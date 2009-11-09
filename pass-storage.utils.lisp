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

(defun make-std-dialog (parent-window title icon content)
  (gtk:let-ui
   (gtk:gtk-window
    :var dlg
    :border-width 8
    :modal t
    :resizable t
    :window-position :center-on-parent
    :title title
    :icon-name icon
    :type-hint :dialog
    :skip-taskbar_hint t
    :skip-pager-hint t
    :gravity :center
    :transient-for parent-window

    (gtk:v-box
     :spacing 8
     :var box
    
     (gtk:h-button-box
      :layout-style :end
      :spacing 5

      (gtk:button
       :var btn_cancel
       :label "gtk-cancel"
       :can-focus t
       :receives-default nil
       :use-stock t)
      :expand nil :fill nil :position 0

      (gtk:button
       :var btn_ok
       :label "gtk-ok"
       :sensitive nil
       :can-focus t
       :can-default t
       :receives-default nil
       :use-stock t)
      :expand nil :fill nil :position 1)
     :expand nil))

   (gtk:box-pack-start box content :expand t)
   (gtk:box-reorder-child box content 0)

   (setf (gtk:gtk-window-default-widget dlg) btn_ok)

   (gobject:connect-signal dlg "destroy" (lambda-u (gtk:gtk-main-quit)))
   
   (gobject:connect-signal btn_cancel "clicked" (lambda-u (gtk:gtk-main-quit)))
   
   (gobject:connect-signal btn_ok "clicked"
			   (lambda-u
			    (setf (gtk:gtk-object-user-data dlg) (cffi:make-pointer 1))
			    (gtk:gtk-main-quit)))

   (values dlg
	   btn_ok
	   btn_cancel)))

(defun std-dialog-run (w)
  (setf (gtk:gtk-object-user-data w) (cffi:null-pointer))
  (gtk:widget-show w)
  (gtk:gtk-main)
  (gtk:widget-hide w)
  (not (cffi:null-pointer-p (gtk:gtk-object-user-data w))))

(defun ask-string (parent-window title icon caption &key (validate nil))
  (gtk:let-ui
   (gtk:h-box
    :var box
    :spacing 8
    
    (gtk:label :label caption) :expand nil
    (gtk:entry :var entry :activates-default t) :expand t)

   (multiple-value-bind (dlg btn_ok)
       (make-std-dialog parent-window title icon box)
     
     (gobject:g-signal-connect entry "changed"
			       (lambda (entry)
				 (let ((text (gtk:entry-text entry)))
				   (when validate
				     (let ((msg (funcall validate text)))
				       (if msg
					   (setf (gtk:entry-secondary-icon-stock entry) "gtk-no"
						 (gtk:entry-secondary-icon-tooltip-text entry) msg
						 (gtk:widget-sensitive btn_ok) nil)

					   (setf (gtk:entry-secondary-icon-stock entry) "gtk-yes"
						 (gtk:entry-secondary-icon-tooltip-text entry) ""
						 (gtk:widget-sensitive btn_ok) t)))))))

     (when (std-dialog-run dlg)
       (gtk:entry-text entry)))))

