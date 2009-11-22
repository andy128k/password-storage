(in-package :pass-storage)

;; main window
(defvar main_window nil)
(defvar listview nil)
(defvar edit_button nil)
(defvar edit_menuitem nil)
(defvar del_button nil)
(defvar del_menuitem nil)

(defstruct app
  builder
  data)

(defun e-close ()
  (gtk:gtk-main-quit))

(defun get-selected-iter (view)
  (let ((selection (gtk:tree-view-selection view)))
    (when selection
      (gtk:tree-selection-selected selection))))

(defun get-selected-group-iter (view)
  (let ((model (gtk:tree-view-model view))
	(iter (get-selected-iter view)))
 
    (loop
       while (and iter
		  (not (is-group (gtk:tree-model-value model iter 0))))
       do (setf iter (gtk:tree-model-iter-parent model iter)))
    
    iter))

(defun listview-cursor-changed (&rest unused-rest)
  (declare (ignore unused-rest))
  (let ((s (get-selected-iter listview)))

    (setf (gtk:widget-sensitive del_button) s)
    (setf (gtk:widget-sensitive del_menuitem) s)
    (setf (gtk:widget-sensitive edit_button) s)
    (setf (gtk:widget-sensitive edit_menuitem) s)))

(defun update-row (app iter entry)
  (let ((data (app-data app)))
    (setf (gtk:tree-store-value data iter 0) entry)
    (setf (gtk:tree-store-value data iter 1) (entry-name entry))
    (setf (gtk:tree-store-value data iter 2) (entry-icon entry))
    (setf (gtk:tree-view-model listview) data)))

(defun cb-add-group (app)
  (let ((group (group-add main_window)))
    (when group
      (let ((iter (gtk:tree-store-append (app-data app)
					 (get-selected-group-iter listview))))
	(update-row app iter group)))))

(defun cb-add-item (app)
  (let ((entry (make-instance 'entry-generic)))
    (when (edit-entry entry main_window "Add entry")
      (let ((iter (gtk:tree-store-append (app-data app)
					 (get-selected-group-iter listview))))
	(update-row app iter entry)))))

(defgeneric ui-edit (parent-window o))
(defmethod ui-edit (parent-window (o entry-group))
  (group-edit parent-window o))
(defmethod ui-edit (parent-window (o entry-generic))
  (edit-entry o main_window "Edit entry"))

(defun cb-edit-entry (app)
  (let ((data (app-data app))
	(iter (get-selected-iter listview)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
	(when (ui-edit main_window entry)
	  (update-row app iter entry))))))

(defun cb-del-entry (app)
  (let ((iter (get-selected-iter listview)))
    (when (and iter
	       (ask "Do you really want to delete selected item?"))
      (let* ((data (app-data app)))
	(gtk:tree-store-remove data iter)
	(setf (gtk:tree-view-model listview) data)
	(listview-cursor-changed)))))

(defun load-data (app filename password)
  (labels ((is-tag (x)
	     (consp x))

	   (tag-name (x)
	     (if (consp (car x))
		 (caar x)
		 (car x)))

	   (tag-attributes (x)
	     (if (consp (car x))
		 (cdar x)
		 nil))

	   (tag-children (x)
	     (cdr x))

	   (tag-get-attr (x attr)
	     (iter (for (k v) on (tag-attributes x) by #'cddr)
		   (finding v such-that (eq k attr))))

	   (entry-node-get-value (entry tag-name &optional (id nil))
	     (or
	      (iter (for ch in (tag-children entry))
		    (finding (car (tag-children ch))
			     such-that (and (is-tag ch)
					    (eq (tag-name ch) tag-name)
					    (or (not id)
						(string= (tag-get-attr ch :|id|) id)))))
	      ""))

	   (load-entry (elem parent-iter)
	     (let ((type (tag-get-attr elem :|type|))
		   (iter (gtk:tree-store-append (app-data app) parent-iter)))
	       
	       (cond
		 ((string= type "folder")
		  (update-row app
			      iter
			      (make-instance 'group :name (entry-node-get-value elem :|name|)))
		  (iter (for ch in (tag-children elem))
			(parse ch iter)))
		 
		 ((string= type "generic")
		  (update-row app
			      iter
			      (make-instance 'item
					     :name (entry-node-get-value elem :|name|)
					     :login (entry-node-get-value elem :|field| "generic-username")
					     :password (entry-node-get-value elem :|field| "generic-password")
					     :url (entry-node-get-value elem :|field| "generic-hostname")
					     :email (entry-node-get-value elem :|field| "generic-email")
					     :comment (entry-node-get-value elem :|description|)))))))
	   
	   (parse (elem parent-iter)
	     (when (is-tag elem)
	       (cond
		 ;; toplevel
		 ((eq (tag-name elem) :|revelationdata|)
		  
		  (iter (for ch in (tag-children elem))
			(parse ch nil)))
		 
		 ((eq (tag-name elem) :|entry|)
		  
		  (load-entry elem parent-iter))))))
    
    (parse (load-revelation-file filename password) nil)))

#|
(defun save-data (lst filename)
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (gtk:do-tree-model lst
      (lambda (model path iter)
	(declare (ignore model path))
	(format s "~s~%" `(item :group ,(gtk:tree-model-value lst iter 0)
				:name ,(gtk:tree-model-value lst iter 1)
				:login ,(gtk:tree-model-value lst iter 2)
				:password ,(gtk:tree-model-value lst iter 3)
				:url ,(gtk:tree-model-value lst iter 4)
				:email ,(gtk:tree-model-value lst iter 5)
				:comments ,(gtk:tree-model-value lst iter 6)))))))
|#

(defun main ()
  (let* ((builder (make-instance 'gtk:builder))
	 (app (make-app :builder builder)))

    (gtk:builder-add-from-file builder "PassStorage.ui")
    
    (setf main_window (gtk:builder-get-object builder "main_window"))
    (setf listview (gtk:builder-get-object builder "listview"))
    (setf edit_button (gtk:builder-get-object builder "edit_button"))
    (setf edit_menuitem (gtk:builder-get-object builder "edit_menuitem"))
    (setf del_button (gtk:builder-get-object builder "del_button"))
    (setf del_menuitem (gtk:builder-get-object builder "del_menuitem"))
    
    (setf (app-data app) (gtk:builder-get-object builder "data"))

    (gobject:g-signal-connect listview "drag-motion"
			      (lambda (widget drag-context x y time)
				(multiple-value-bind (path pos)
				    (gtk:tree-view-get-dest-row-at-pos widget x y)
				  
				  (when (and path (or (eq pos :into-or-before) (eq pos :into-or-after)))
				    (let* ((model (gtk:tree-view-model widget))
					   (iter (gtk:tree-model-iter-by-path model path)))

				      (if (is-group (gtk:tree-store-value model iter 0))
					  (progn (gdk:drag-status drag-context :move time) nil)
					  (progn (gdk:drag-status drag-context 0 time) t)))))))

    (connect builder
	     ("on_close" e-close)
    	     ("on_listview_cursor_changed" listview-cursor-changed)
	     ("on_add_group" cb-add-group app)
	     ("on_add_button_clicked" cb-add-item app)
	     ("on_edit_button_clicked" cb-edit-entry app)
	     ("on_del_button_clicked" cb-del-entry app))
    
    ;; (load-data app "./data" "Nd3e")

    (gtk:widget-show main_window)
    (gtk:gtk-main)

    ;; (save-data lst "./data")

    ))

(export 'main)

