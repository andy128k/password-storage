(in-package :pass-storage)

;; main window
(defvar main_window nil)
(defvar listview nil)
(defvar switcher nil)
(defvar edit_button nil)
(defvar edit_menuitem nil)
(defvar del_button nil)
(defvar del_menuitem nil)

(defstruct app
  builder
  data
  edit_window)

(defun ew-widgets (app)
  (let ((builder (app-builder app)))
    (list
     (gtk:builder-get-object builder "name_entry")
     (gtk:builder-get-object builder "login_entry")
     (gtk:builder-get-object builder "password_entry")
     (gtk:builder-get-object builder "url_entry")
     (gtk:builder-get-object builder "email_entry")
     (gtk:builder-get-object builder "comment_text"))))

;; get-text
(defgeneric get-text (widget))
(defmethod get-text ((widget gtk:entry))
  (gtk:entry-text widget))
(defmethod get-text ((widget gtk:text-view))
  (gtk:text-buffer-text (gtk:text-view-buffer widget)))

;; set-text
(defgeneric set-text (widget text))
(defmethod set-text ((widget gtk:entry) text)
  (setf (gtk:entry-text widget) text))
(defmethod set-text ((widget gtk:text-view) text)
  (setf (gtk:text-buffer-text (gtk:text-view-buffer widget)) text))

(defun ew-name-entry (app)
  (gtk:builder-get-object (app-builder app) "name_entry"))

(defun edit-window-clear (app)
  (loop
     for w in (ew-widgets app)
     do (set-text w ""))
  (gtk:widget-grab-focus (ew-name-entry app)))

(defun edit-window-load (app iter)
  (let ((model (app-data app)))
    (loop
       for w in (ew-widgets app)
       for i from 1
       do (setf (gtk:tree-store-value model iter i)
		(get-text w)))))

(defun edit-window-save (app iter)
  (let ((data (app-data app)))
    (loop
       for w in (ew-widgets app)
       for i from 1
       do (set-text w
		    (gtk:tree-model-value data iter i)))))

(defun name-entry-changed (app)
  (let* ((edit_window (app-edit_window app))
	 (name_entry (ew-name-entry app)))

    (gtk:dialog-set-response-sensitive edit_window
				       :ok
				       (/= 0 (length (gtk:entry-text name_entry))))))

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

(defun add-group (app)
  (let ((group (ask-string main_window "New group" "gtk-directory" "Group name")))
    (when group
      (let* ((data (app-data app))
	     (iter (gtk:tree-store-append data nil)))
	(setf (gtk:tree-store-value data iter 0) t)
	(setf (gtk:tree-store-value data iter 1) group)
	(setf (gtk:tree-store-value data iter 7) "gtk-directory")
	(setf (gtk:tree-view-model listview) data)))))

(defun add-entry (app)
  (let* ((data (app-data app))
	 (edit_window (app-edit_window app)))
    
    (edit-window-clear app)
    (setf (gtk:gtk-window-title edit_window) "Add entry")
    (when (show-modal edit_window)
      (let* ((parent (get-selected-group-iter listview))
	     (iter (gtk:tree-store-append data parent)))
	(edit-window-load app iter)
	(setf (gtk:tree-store-value data iter 7) "gtk-file")
	(setf (gtk:tree-view-model listview) data)))))

(defun edit-entry (app)
  (let* ((data (app-data app))
	 (edit_window (app-edit_window app)))

    (let ((iter (get-selected-iter listview)))
      (when iter
	(edit-window-save app iter)
	(setf (gtk:gtk-window-title edit_window) "Edit entry")
	(when (show-modal edit_window)
	  (edit-window-load app iter)
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
    (setf switcher (gtk:builder-get-object builder "group_switcher"))
    (setf edit_button (gtk:builder-get-object builder "edit_button"))
    (setf edit_menuitem (gtk:builder-get-object builder "edit_menuitem"))
    (setf del_button (gtk:builder-get-object builder "del_button"))
    (setf del_menuitem (gtk:builder-get-object builder "del_menuitem"))
    
    (setf (app-edit_window app) (gtk:builder-get-object builder "edit_window"))
    (setf (app-data app) (gtk:builder-get-object builder "data"))

    (connect builder
	     ("on_close" e-close)
    	     ("on_listview_cursor_changed" listview-cursor-changed)
	     ("on_add_group" add-group app)
	     ("on_add_button_clicked" add-entry app)
	     ("on_edit_button_clicked" edit-entry app)
	     ("on_del_button_clicked" del-entry app)
	     ("on_name_entry_changed" name-entry-changed app))
    
    ;;(load-data lst "./data")

    (gtk:widget-show main_window)
    (gtk:gtk-main)

    ;; (save-data lst "./data")

    ))

(export 'main)

