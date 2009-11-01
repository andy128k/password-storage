#+sbcl
(require 'asdf)

(asdf:oos 'asdf:load-op :cl-gtk2-gtk)

;;; test

(defvar builder nil)

;;; models
(defvar lst nil)
(defvar groups nil)

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
(defvar group_entry nil)
(defvar name_entry nil)
(defvar login_entry nil)
(defvar password_entry nil)
(defvar url_entry nil)
(defvar email_entry nil)
(defvar comment_text nil)

(defun edit-window-clear ()
  (setf (gtk:entry-text group_entry) "")
  (setf (gtk:entry-text name_entry) "")
  (setf (gtk:entry-text login_entry) "")
  (setf (gtk:entry-text password_entry) "")
  (setf (gtk:entry-text url_entry) "")
  (setf (gtk:entry-text email_entry) "")
  (setf (gtk:text-buffer-text (gtk:text-view-buffer comment_text)) "")
  (gtk:widget-grab-focus group_entry))

(defun edit-window-load (lst iter)
  (setf (gtk:list-store-value lst iter 0) (gtk:entry-text group_entry))
  (setf (gtk:list-store-value lst iter 1) (gtk:entry-text name_entry))
  (setf (gtk:list-store-value lst iter 2) (gtk:entry-text login_entry))
  (setf (gtk:list-store-value lst iter 3) (gtk:entry-text password_entry))
  (setf (gtk:list-store-value lst iter 4) (gtk:entry-text url_entry))
  (setf (gtk:list-store-value lst iter 5) (gtk:entry-text email_entry))
  (setf (gtk:list-store-value lst iter 6) (gtk:text-buffer-text (gtk:text-view-buffer comment_text))))

(defun edit-window-save (lst iter)
  (setf (gtk:entry-text group_entry)
	(gtk:tree-model-value lst iter 0))

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

(defun show-modal (dlg)
  (prog2
      (gtk:widget-show dlg :all t)
      (eql (gtk:dialog-run dlg) :ok)
      (gtk:widget-hide dlg)))

(defun ask (message)
  (eql (gtk:show-message message :buttons :yes-no :message-type :warning)
       :yes))

(defun e-close (&rest unused-rest)
  (declare (ignore unused-rest))
  (gtk:gtk-main-quit))

(defun listview-cursor-changed (&rest unused-rest)
  (declare (ignore unused-rest))

  (let ((selection (gtk:tree-view-selection listview))
	(s nil))

    (when selection
      (let ((iter (gtk:tree-selection-selected selection)))
	(when iter
	  (setf s t))))

    (setf (gtk:widget-sensitive edit_button) s)
    (setf (gtk:widget-sensitive edit_menuitem) s)
    (setf (gtk:widget-sensitive del_button) s)
    (setf (gtk:widget-sensitive del_menuitem) s)))

(defun add-entry (&rest unused-rest)
  (declare (ignore unused-rest))
  (edit-window-clear)
  ;(setf (gtk:window-title edit_window) "Add entry")
  (when (show-modal edit_window)
    (let ((iter (gtk:list-store-append lst)))
      (edit-window-load lst iter)
      (setf (gtk:tree-view-model listview) lst))
    ;;recreate_groups(mw);
    ))

(defun edit-entry (&rest unused-rest)
  (declare (ignore unused-rest))
  (let ((selection (gtk:tree-view-selection listview)))
    (when selection
      (let ((iter (gtk:tree-selection-selected selection)))
	(when iter
	  (edit-window-save lst iter)
	  ;(setf (gtk:window-title edit_window) "Edit entry")
	  (when (show-modal edit_window)
	    (edit-window-load lst iter)
	    (setf (gtk:tree-view-model listview) lst)
	    ; recreate_groups(mw);
	    ))))))

(defun del-entry (&rest unused-rest)
  (declare (ignore unused-rest))
  (let ((selection (gtk:tree-view-selection listview)))
    (when selection
      (let ((iter (gtk:tree-selection-selected selection)))
	(when iter
	  (when (ask "Do you really want to delete selected item?")
	    (gtk:list-store-remove lst iter)
	    (setf (gtk:tree-view-model listview) lst)
	    ;recreate_groups(mw);
            (listview-cursor-changed)))))))
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

(defun save-data (lst filename)
  (glib:with-object
   (keyfile (glib:key-file-new))
   (let ((index 0))
     (gtk:tree-model-foreach
      lst
      (lambda (iter)
	(let ((g (format nil "~D" index)))
	  (glib:with-object
	   (val (gtk:tree-model-get lst iter 0))
	   (glib:key-file-set-string keyfile g "group" (gobject:value-string val)))
	  (glib:with-object
	   (val (gtk:tree-model-get lst iter 1))
	   (glib:key-file-set-string keyfile g "name" (gobject:value-string val)))
	  (glib:with-object
	   (val (gtk:tree-model-get lst iter 2))
	   (glib:key-file-set-string keyfile g "login" (gobject:value-string val)))
	  (glib:with-object
	   (val (gtk:tree-model-get lst iter 3))
	   (glib:key-file-set-string keyfile g "password" (gobject:value-string val)))
	  (glib:with-object
	   (val (gtk:tree-model-get lst iter 4))
	   (glib:key-file-set-string keyfile g "url" (gobject:value-string val)))
	  (glib:with-object
	   (val (gtk:tree-model-get lst iter 5))
	   (glib:key-file-set-string keyfile g "email" (gobject:value-string val)))
	  (glib:with-object
	   (val (gtk:tree-model-get lst iter 6))
	   (glib:key-file-set-string keyfile g "comments" (gobject:value-string val)))

			       (incf index)))))

   ; save
   (let ((data (glib:key-file-to-data keyfile)))
     (with-open-file (s filename :direction :output :if-exists :supersede)
		     (format s "~a" data)))))
|#

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
  (setf group_entry (gtk:builder-get-object builder "group_entry"))
  (setf name_entry (gtk:builder-get-object builder "name_entry"))
  (setf login_entry (gtk:builder-get-object builder "login_entry"))
  (setf password_entry (gtk:builder-get-object builder "password_entry"))
  (setf url_entry (gtk:builder-get-object builder "url_entry"))
  (setf email_entry (gtk:builder-get-object builder "email_entry"))
  (setf comment_text (gtk:builder-get-object builder "comment_text"))

  (setf lst (gtk:builder-get-object builder "lst"))
  (setf groups (gtk:builder-get-object builder "groups"))

  #|
  (let ((renderer (gtk:cell-renderer-text-new)))
    (gtk:cell-layout-clear switcher)
    (gtk:cell-layout-pack-start switcher renderer glib:*true*)
    (gtk:cell-layout-add-attribute switcher renderer "text" 0))
  |#

  (gtk:builder-connect-signals-simple builder
				      '(("on_close" e-close)
					("on_listview_cursor_changed" listview-cursor-changed)
					("on_add_button_clicked" add-entry)
					("on_edit_button_clicked" edit-entry)
					("on_del_button_clicked" del-entry)
					("on_name_entry_changed" name-entry-changed)))
  
  ;;(load-data lst "./data")
  ;;recreate_groups(&mw)

  (gtk:gtk-main)

  ;;(save-data lst "./data")
  )

