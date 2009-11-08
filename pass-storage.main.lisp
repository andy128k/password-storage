(in-package :pass-storage)

;;; test

(defvar builder nil)

;;; models
(defvar data nil)

;;; main window
(defvar main_window nil)
(defvar listview nil)
(defvar switcher nil)
(defvar edit_button nil)
(defvar edit_menuitem nil)
(defvar del_button nil)
(defvar del_menuitem nil)

;;; edit window
(defvar edit_window nil)
(defvar name_entry nil)
(defvar login_entry nil)
(defvar password_entry nil)
(defvar url_entry nil)
(defvar email_entry nil)
(defvar comment_text nil)

(defun edit-window-clear ()
  (setf (gtk:entry-text name_entry) "")
  (setf (gtk:entry-text login_entry) "")
  (setf (gtk:entry-text password_entry) "")
  (setf (gtk:entry-text url_entry) "")
  (setf (gtk:entry-text email_entry) "")
  (setf (gtk:text-buffer-text (gtk:text-view-buffer comment_text)) "")
  (gtk:widget-grab-focus name_entry))

(defun edit-window-load (model iter)
  (setf (gtk:tree-store-value model iter 1) (gtk:entry-text name_entry))
  (setf (gtk:tree-store-value model iter 2) (gtk:entry-text login_entry))
  (setf (gtk:tree-store-value model iter 3) (gtk:entry-text password_entry))
  (setf (gtk:tree-store-value model iter 4) (gtk:entry-text url_entry))
  (setf (gtk:tree-store-value model iter 5) (gtk:entry-text email_entry))
  (setf (gtk:tree-store-value model iter 6) (gtk:text-buffer-text (gtk:text-view-buffer comment_text))))

(defun edit-window-save (lst iter)
  (setf (gtk:entry-text name_entry)
	(gtk:tree-model-value lst iter 1))

  (setf (gtk:entry-text login_entry)
	(gtk:tree-model-value lst iter 2))

  (setf (gtk:entry-text password_entry)
	(gtk:tree-model-value lst iter 3))
  
  (setf (gtk:entry-text url_entry)
	(gtk:tree-model-value lst iter 4))

  (setf (gtk:entry-text email_entry)
	(gtk:tree-model-value lst iter 5))

  (setf (gtk:text-buffer-text (gtk:text-view-buffer comment_text))
	(gtk:tree-model-value lst iter 6)))

(defun name-entry-changed (&rest unused-rest)
  (declare (ignore unused-rest))
  (gtk:dialog-set-response-sensitive edit_window
				     :ok
				     (/= 0 (length (gtk:entry-text name_entry)))))

(defun e-close (&rest unused-rest)
  (declare (ignore unused-rest))
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

(defun add-group (&rest unused-rest)
  (declare (ignore unused-rest))

  (let ((group (ask-string main_window "New group" "gtk-directory" "Group name")))
    (when (and group (string/= group ""))
      (let ((iter (gtk:tree-store-append data nil)))
	(setf (gtk:tree-store-value data iter 0) t)
	(setf (gtk:tree-store-value data iter 1) group)
	(setf (gtk:tree-store-value data iter 7) "gtk-directory")
	(setf (gtk:tree-view-model listview) data)))))

(defun add-entry (&rest unused-rest)
  (declare (ignore unused-rest))

  (edit-window-clear)
  (setf (gtk:gtk-window-title edit_window) "Add entry")
  (when (show-modal edit_window)
    (let* ((parent (get-selected-group-iter listview))
	   (iter (gtk:tree-store-append data parent)))
      (edit-window-load data iter)
      (setf (gtk:tree-store-value data iter 7) "gtk-file")
      (setf (gtk:tree-view-model listview) data))))

(defun edit-entry (&rest unused-rest)
  (declare (ignore unused-rest))

  (let ((iter (get-selected-iter listview)))
    (when iter
      (edit-window-save data iter)
      (setf (gtk:gtk-window-title edit_window) "Edit entry")
      (when (show-modal edit_window)
	(edit-window-load data iter)
	(setf (gtk:tree-view-model listview) data)))))

(defun del-entry (&rest unused-rest)
  (declare (ignore unused-rest))

  (let ((iter (get-selected-iter listview)))
    (when (and iter
	       (ask "Do you really want to delete selected item?"))
      (gtk:tree-store-remove data iter)
      (setf (gtk:tree-view-model listview) data)
      (listview-cursor-changed))))

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
  (setf builder (make-instance 'gtk:builder))
  (gtk:builder-add-from-file builder "PassStorage.ui")

  (setf main_window (gtk:builder-get-object builder "main_window"))
  (setf listview (gtk:builder-get-object builder "listview"))
  (setf switcher (gtk:builder-get-object builder "group_switcher"))
  (setf edit_button (gtk:builder-get-object builder "edit_button"))
  (setf edit_menuitem (gtk:builder-get-object builder "edit_menuitem"))
  (setf del_button (gtk:builder-get-object builder "del_button"))
  (setf del_menuitem (gtk:builder-get-object builder "del_menuitem"))

  (setf edit_window (gtk:builder-get-object builder "edit_window"))
  (setf name_entry (gtk:builder-get-object builder "name_entry"))
  (setf login_entry (gtk:builder-get-object builder "login_entry"))
  (setf password_entry (gtk:builder-get-object builder "password_entry"))
  (setf url_entry (gtk:builder-get-object builder "url_entry"))
  (setf email_entry (gtk:builder-get-object builder "email_entry"))
  (setf comment_text (gtk:builder-get-object builder "comment_text"))

  (setf data (gtk:builder-get-object builder "data"))

  #|
  (let ((renderer (gtk:cell-renderer-text-new)))
    (gtk:cell-layout-clear switcher)
    (gtk:cell-layout-pack-start switcher renderer glib:*true*)
    (gtk:cell-layout-add-attribute switcher renderer "text" 0))
  |#

  (gtk:builder-connect-signals-simple builder
				      '(("on_close" e-close)
					("on_listview_cursor_changed" listview-cursor-changed)
					("on_add_group" add-group)
					("on_add_button_clicked" add-entry)
					("on_edit_button_clicked" edit-entry)
					("on_del_button_clicked" del-entry)
					("on_name_entry_changed" name-entry-changed)))
  
  ;;(load-data lst "./data")
  ;;recreate_groups(&mw)

  (gtk:widget-show main_window)
  (gtk:gtk-main)

  ;; (save-data lst "./data")
  )

(export 'main)

