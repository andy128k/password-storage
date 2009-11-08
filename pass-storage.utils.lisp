(in-package :pass-storage)

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
	    (list 'list event `(lambda (&rest unused-rest)
				 (declare (ignore unused-rest))
				 (funcall (function ,func) ,@args)))))))

(defun ask (message)
  (eql (gtk:show-message message :buttons :yes-no :message-type :warning)
       :yes))

(defun window-run (w)
  (setf (gtk:gtk-object-user-data w) (cffi:null-pointer))
  (gtk:widget-show w)
  (gtk:gtk-main)
  (gtk:widget-hide w)
  (not (cffi:null-pointer-p (gtk:gtk-object-user-data w))))

(defun ask-string (parent-window title icon caption)
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

     (gtk:h-box
      ; :border-width 8
      :spacing 8
      
      (gtk:label :label caption) :expand nil
      (gtk:entry :var entry) :expand t)
     
     (gtk:h-button-box
      :layout-style :end

      (gtk:button
       :var btn_cancel
       :label "gtk-cancel"
       :can-focus t
       :can-default t
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

   (gobject:connect-signal dlg "destroy"
			   (lambda (u)
			     (declare (ignore u))
			     (gtk:gtk-main-quit)))
   
   (gobject:connect-signal btn_cancel "clicked"
			   (lambda (u)
			     (declare (ignore u))
			     (gtk:gtk-main-quit)))
   
   (gobject:connect-signal btn_ok "clicked"
			   (lambda (u)
			     (declare (ignore u))
			     (setf (gtk:gtk-object-user-data dlg) (cffi:make-pointer 1))
			     (gtk:gtk-main-quit)))

   (gobject:g-signal-connect entry "changed"
			     (lambda (entry)
			       (setf (gtk:widget-sensitive btn_ok)
				     (/= 0 (length (gtk:entry-text entry))))))

   (when (window-run dlg)
     (gtk:entry-text entry))))

