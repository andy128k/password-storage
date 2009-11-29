(in-package :pass-storage)

(defun ask (message)
  (eql (gtk:show-message message :buttons :yes-no :message-type :warning)
       :yes))

(defun say-error (message)
  (gtk:show-message message :buttons :ok :message-type :error))

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

(defun make-table (conf)
  (let ((table (make-instance 'gtk:table
			      :n-rows (length conf)
			      :n-columns 2
			      :border-width 5
			      :column-spacing 8
			      :row-spacing 8)))

    (values
     table
     (iter (for item in conf)
	   (for i from 0)
	   (let ((slot-name (first item))
		 (title (second item))
		 (kind (third item))
		 (params (cdddr item)))
	     
	     (gtk:table-attach table
			       (make-instance 'gtk:label
					      :label title
					      :xalign 0 :yalign 0.5)
			       0 1 i (+ i 1) :x-options :fill :y-options :fill)

	     (let ((widget (case kind
			     (:entry
			      (let ((widget (make-instance 'gtk:entry
							   :can-focus t
							   :activates-default t)))
				(gtk:table-attach table
						  widget
						  1 2 i (+ i 1) :y-options :fill)
				widget))
			     (:area
			      (let ((sw (make-instance 'gtk:scrolled-window
						       :can-focus t
						       :hscrollbar-policy :automatic
						       :vscrollbar-policy :automatic
						       :shadow-type :in))
				    (widget (make-instance 'gtk:text-view
							   :can-focus t
							   :accepts-tab nil)))
				(gtk:container-add sw widget)
				
				(gtk:table-attach table
						  sw
						  1 2 i (+ i 1))
				widget)))))

	       (collect
		(list* slot-name widget params))))))))

(defgeneric widget-get-text (widget))
(defmethod widget-get-text ((widget gtk:entry))
  (gtk:entry-text widget))
(defmethod widget-get-text ((widget gtk:text-view))
  (gtk:text-buffer-text (gtk:text-view-buffer widget)))

(defgeneric widget-set-text (widget value))
(defmethod widget-set-text ((widget gtk:entry) value)
  (setf (gtk:entry-text widget) value))
(defmethod widget-set-text ((widget gtk:text-view) value)
  (setf (gtk:text-buffer-text (gtk:text-view-buffer widget)) value))

(defun edit-object (obj parent-window title icon conf)
  (multiple-value-bind (table ws)
      (make-table conf)
    
    (let ((dlg (make-std-dialog parent-window title icon table)))

      (iter (for (slot widget . params) in ws)
	    (when (find :required params)
	      (gobject:connect-signal widget "changed"
				      (lambda (entry)
					(gtk:dialog-set-response-sensitive 
					 dlg
					 :ok 
					 (/= 0 (length (widget-get-text entry)))))))

	    (when slot
	      (widget-set-text widget (slot-value obj slot))))

      (when (std-dialog-run dlg)
	(or
	 (iter (for (slot widget) in ws)
	       (when slot
		 (setf (slot-value obj slot) (widget-get-text widget)))
	       (collect (widget-get-text widget)))
	 t)))))

