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
		  (not (gtk:tree-model-value model iter 0)))
       do (setf iter (gtk:tree-model-iter-parent model iter)))

    iter))

(defun listview-cursor-changed (&rest unused-rest)
  (declare (ignore unused-rest))
  (let ((s (get-selected-iter listview)))

    (setf (gtk:widget-sensitive del_button) s)
    (setf (gtk:widget-sensitive del_menuitem) s)

    (when (and s (gtk:tree-model-value (gtk:tree-view-model listview) s 0))
      (setf s nil))
    
    (setf (gtk:widget-sensitive edit_button) s)
    (setf (gtk:widget-sensitive edit_menuitem) s)))

(defun get-groups (app)
  (let (groups)
    (gtk:do-tree-model (app-data app)
      (lambda (model path iter)
	(declare (ignore path))
	(when (gtk:tree-store-value model iter 0)
	  (push (gtk:tree-store-value model iter 1) groups)
	  nil)))
    (reverse groups)))

(defun add-group (app)
  (let ((group (ask-string main_window "New group" "gtk-directory" "Group name"
			   :validate (lambda (text)
				       (when (= 0 (length text))
					 "Enter group name")))))

    (when group
      (let* ((data (app-data app))
	     (parent (get-selected-group-iter listview))
	     (iter (gtk:tree-store-append data parent)))

	(setf (gtk:tree-store-value data iter 0) t)
	(setf (gtk:tree-store-value data iter 1) group)
	(setf (gtk:tree-store-value data iter 7) "gtk-directory")
	(setf (gtk:tree-view-model listview) data)))))

(defun item->model (model iter item)
  (setf (gtk:tree-store-value model iter 0) nil)
  (setf (gtk:tree-store-value model iter 1) (item-name item))
  (setf (gtk:tree-store-value model iter 2) (item-login item))
  (setf (gtk:tree-store-value model iter 3) (item-password item))
  (setf (gtk:tree-store-value model iter 4) (item-url item))
  (setf (gtk:tree-store-value model iter 5) (item-email item))
  (setf (gtk:tree-store-value model iter 6) (item-comment item))
  (setf (gtk:tree-store-value model iter 7) "gtk-file"))

(defun add-entry (app)
  (let ((item (item-add main_window)))
    (when item
      (let* ((data (app-data app))
	     (parent (get-selected-group-iter listview))
	     (iter (gtk:tree-store-append data parent)))
	(item->model data iter item)
	(setf (gtk:tree-view-model listview) data)))))

(defun edit-entry (app)
  (let ((data (app-data app))
	(iter (get-selected-iter listview)))
    (when iter
      (let ((item (make-item
		   :name (gtk:tree-store-value data iter 1)
		   :login (gtk:tree-store-value data iter 2)
		   :password (gtk:tree-store-value data iter 3)
		   :url (gtk:tree-store-value data iter 4)
		   :email (gtk:tree-store-value data iter 5)
		   :comment (gtk:tree-store-value data iter 6))))
	(when (item-edit main_window item)
	  (item->model data iter item)
	  (setf (gtk:tree-view-model listview) data))))))

(defun del-entry (app)
  (let ((iter (get-selected-iter listview)))
    (when (and iter
	       (ask "Do you really want to delete selected item?"))
      (let* ((data (app-data app)))
	(gtk:tree-store-remove data iter)
	(setf (gtk:tree-view-model listview) data)
	(listview-cursor-changed)))))

#|
(defun load-data (lst filename)
  (let ((keyfile (glib:key-file-new)))
    (when
	(glib:key-file-load-from-file keyfile filename 0)
      (let ((groups (glib:key-file-get-groups keyfile)))
	(dolist (p groups)
	  (let ((iter (gtk:list-store-append lst))
		(val (gobject:value-new gobject:*type-string*)))
	      
	      (setf (gobject:value-string val) (glib:key-file-get-string keyfile p "group"))
	      (gtk:list-store-set lst iter 0 val)
	      
	      (setf (gobject:value-string val) (glib:key-file-get-string keyfile p "name"))
	      (gtk:list-store-set lst iter 1 val)
	      
	      (setf (gobject:value-string val) (glib:key-file-get-string keyfile p "login"))
	      (gtk:list-store-set lst iter 2 val)
	      
	      (setf (gobject:value-string val) (glib:key-file-get-string keyfile p "password"))
	      (gtk:list-store-set lst iter 3 val)

	   (setf (gobject:value-string val) (glib:key-file-get-string keyfile p "url"))
	   (gtk:list-store-set lst iter 4 val)

	   (setf (gobject:value-string val) (glib:key-file-get-string keyfile p "email"))
	   (gtk:list-store-set lst iter 5 val)

	   (setf (gobject:value-string val) (glib:key-file-get-string keyfile p "comments"))
	   (gtk:list-store-set lst iter 6 val)))))))
|#

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

				      (if (gtk:tree-store-value model iter 0)
					  (progn (gdk:drag-status drag-context :move time) nil)
					  (progn (gdk:drag-status drag-context 0 time) t)))))))

    (connect builder
	     ("on_close" e-close)
    	     ("on_listview_cursor_changed" listview-cursor-changed)
	     ("on_add_group" add-group app)
	     ("on_add_button_clicked" add-entry app)
	     ("on_edit_button_clicked" edit-entry app)
	     ("on_del_button_clicked" del-entry app))
    
    ;;(load-data lst "./data")

    (gtk:widget-show main_window)
    (gtk:gtk-main)

    ;; (save-data lst "./data")

    ))

(export 'main)

