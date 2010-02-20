(in-package :pass-storage)

(defun ask (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
			    :text message
			    :buttons :yes-no
			    :message-type :question
			    :window-position :center-on-parent
			    :transient-for parent-window
			    :use-markup nil)))
    (prog1
        (eql (gtk:dialog-run dlg) :yes)
      (gtk:object-destroy dlg))))

(defun ask-save (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
			    :text message
			    :message-type :warning
			    :window-position :center-on-parent
			    :transient-for parent-window
			    :use-markup nil)))

    (gtk:dialog-add-button dlg "gtk-discard" :reject)
    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-save" :ok)
    (gtk:set-dialog-alternative-button-order dlg (list :ok :reject :cancel))

    (prog1
        (gtk:dialog-run dlg)
      (gtk:object-destroy dlg))))

(defun say-error (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
			    :text message
			    :buttons :ok
			    :message-type :error
			    :window-position :center-on-parent
			    :transient-for parent-window
			    :use-markup nil)))
    (gtk:dialog-run dlg)
    (gtk:object-destroy dlg)))

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
    (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))

    (setf (gtk:dialog-default-response dlg) :ok)

    (gtk:container-add (gtk:dialog-content-area dlg) content)

    dlg))

(defun std-dialog-run (dlg)
  (gtk:widget-show dlg :all t)
  (prog1
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

	     (flet ((insert-label ()
		      (gtk:table-attach table
					(make-instance 'gtk:label
						       :label title
						       :xalign 0 :yalign 0.5)
					0 1 i (+ i 1) :x-options :fill :y-options :fill)))

	       (let ((widget (case kind
			       ((:entry :secret)
				(insert-label)
				(let ((widget (make-instance 'gtk:entry
							     :can-focus t
							     :activates-default t)))
				  (gtk:table-attach table
						    widget
						    1 2 i (+ i 1) :y-options :fill)
				  widget))
			       (:password
				(insert-label)
				(let ((widget (make-instance 'gtk:entry
							     :can-focus t
							     :activates-default t
							     :secondary-icon-stock "gtk-execute")))
				  (setf (gtk:entry-secondary-icon-tooltip-text widget) "Generate password")
				  (gobject:connect-signal widget "icon-release"
							  (lambda (entry pos event)
							    (when (eq pos :secondary)
							      (when (or (string= "" (gtk:entry-text entry))
									(ask (gtk:widget-toplevel entry) "Do you want to overwrite current password?"))
								(setf (gtk:entry-text entry) (generate-password))))))
				  (gtk:table-attach table
						    widget
						    1 2 i (+ i 1) :y-options :fill)
				  widget))
			       (:area
				(insert-label)
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
				  widget))
			       (:filename
				(insert-label)
				(let ((widget (make-instance 'gtk:file-chooser-button
							     :can-focus t
							     :activates-default t
							     :action :open)))
				  (gtk:table-attach table
						    widget
						    1 2 i (+ i 1) :y-options :fill)
				  widget))
			       (:boolean
				(let ((widget (make-instance 'gtk:check-button
							     :can-focus t
							     :label title
							     :activates-default t)))
				  (gtk:table-attach table
						    widget
						    0 2 i (+ i 1) :x-options :shrink :y-options :fill)
				  widget)))))

		 (collect
		  (list* slot-name widget params)))))))))

(defgeneric widget-get-text (widget))
(defmethod widget-get-text ((widget gtk:entry))
  (gtk:entry-text widget))
(defmethod widget-get-text ((widget gtk:text-view))
  (gtk:text-buffer-text (gtk:text-view-buffer widget)))
(defmethod widget-get-text ((widget gtk:file-chooser-button))
  (let ((filename (gtk:file-chooser-filename widget)))
    (when (/= 0 (length filename))
      filename)))
(defmethod widget-get-text ((widget gtk:check-button))
  (gtk:toggle-button-active widget))

(defgeneric widget-set-text (widget value))
(defmethod widget-set-text ((widget gtk:entry) value)
  (setf (gtk:entry-text widget) value))
(defmethod widget-set-text ((widget gtk:text-view) value)
  (setf (gtk:text-buffer-text (gtk:text-view-buffer widget)) value))
(defmethod widget-set-text ((widget gtk:file-chooser-button) value)
  (if value
      (setf (gtk:file-chooser-filename widget) value)
      (gtk:file-chooser-unselect-all widget)))
(defmethod widget-set-text ((widget gtk:check-button) value)
  (setf (gtk:toggle-button-active widget) value))

(defun edit-object (obj parent-window title icon conf)
  (multiple-value-bind (table ws)
      (make-table conf)

    (let ((dlg (make-std-dialog parent-window title icon table))
	  disable-ok)

      (iter (for (slot widget . params) in ws)
	    (when (find :required params)
	      (setf disable-ok (and slot
				    (= 0 (length (slot-value obj slot)))))
	      (gobject:connect-signal widget "changed"
				      (lambda (entry)
					(gtk:dialog-set-response-sensitive
					 dlg
					 :ok
					 (/= 0 (length (widget-get-text entry)))))))

	    (when (find :password params)
	      (setf (gtk:entry-visibility widget) nil))

	    (when slot
	      (widget-set-text widget (slot-value obj slot))))

      (when disable-ok
	(gtk:dialog-set-response-sensitive dlg :ok nil))

      (when (std-dialog-run dlg)
	(or
	 (iter (for (slot widget) in ws)
	       (when slot
		 (setf (slot-value obj slot) (widget-get-text widget)))
	       (collect (widget-get-text widget)))
	 t)))))

;; tree model

(defmacro-driver (FOR iter in-tree-model model children-of parent-iter)
    (let ((m (gensym))
	  (parent (gensym))
	  (p (gensym))
	  (i (gensym))
	  (kwd (if generate 'generate 'for)))
      `(progn
	 (with ,m = ,model)
	 (with ,parent = ,parent-iter)
	 (with ,p = t)
	 (with ,i = nil)
	 (,kwd ,iter next
	       (progn
		 (if ,p ; first iter
		     (progn
		       (setf ,p nil)
		       (setf ,i (if ,parent
				    (gtk:tree-model-iter-first-child ,m ,parent)
				    (gtk:tree-model-iter-first ,m))))
		     ;; else
		     (progn
		       (unless (gtk:tree-model-iter-next ,m ,i)
			 (terminate))))

		 (unless ,i
		   (terminate))
		 ,i)))))

