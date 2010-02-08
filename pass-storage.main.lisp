(in-package :pass-storage)

(defstruct app
  main-window
  data
  view
  action-group
  filter

  filename
  password
  changed)

(defun ensure-data-is-saved (app)
  (if (app-changed app)

      (case (ask-save (app-main-window app)
		      "Save changes before closing? If you don't save, changes will be permanently lost.")
	(:ok
	 (cb-save app)
	 t)
	(:reject
	 t)
	(:cancel
	 nil))

      t))

(defun e-close (app)
  (if (ensure-data-is-saved app)
      (progn
         (gtk:gtk-main-quit)
         (gtk:clipboard-clear (gtk:get-clipboard "CLIPBOARD"))
         nil)
      t))

(defun get-selected-iter (app)
  (let ((selection (gtk:tree-view-selection (app-view app))))
    (when selection
      (let ((filter-iter (gtk:tree-selection-selected selection)))
	(when filter-iter
	  (gtk:tree-model-filter-convert-iter-to-child-iter (app-filter app)
							    filter-iter))))))

(defun get-selected-group-iter (app)
  (let* ((data (app-data app))
	 (iter (get-selected-iter app)))

    (loop
       while (and iter
		  (not (is-group (gtk:tree-model-value data iter 0))))
       do (setf iter (gtk:tree-model-iter-parent data iter)))

    iter))

(defun listview-cursor-changed (app)
  (let ((s (get-selected-iter app)))
    (setf (gtk:action-sensitive (gtk:action-group-action (app-action-group app) "edit")) s)
    (setf (gtk:action-sensitive (gtk:action-group-action (app-action-group app) "delete")) s)))

(defun update-row (app iter entry)
  (let ((data (app-data app)))
    (setf (gtk:tree-store-value data iter 0) entry)
    (setf (gtk:tree-store-value data iter 1) (entry-name entry))
    (setf (gtk:tree-store-value data iter 2) (entry-icon entry))
    (gtk:tree-model-filter-refilter (app-filter app))))

(defun cb-add-item (app type)
  (let ((entry (make-instance type)))
    (when (edit-entry entry (app-main-window app) "Add")
      (let ((iter (gtk:tree-store-append (app-data app)
					 (get-selected-group-iter app))))
	(update-row app iter entry)
	(setf (app-changed app) t)))))

(defun cb-edit-entry (app)
  (let ((data (app-data app))
	(iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
	(when (edit-entry entry (app-main-window app) "Edit")
	  (update-row app iter entry)
	  (setf (app-changed app) t))))))

(defun cb-del-entry (app)
  (let ((iter (get-selected-iter app)))
    (when (and iter
	       (ask (app-main-window app) "Do you really want to delete selected item?"))
      (let* ((data (app-data app)))
	(gtk:tree-store-remove data iter)
	(listview-cursor-changed app)
	(setf (app-changed app) t)))))

(defun load-data (app xml)
  (labels ((load-entry2 (elem parent-iter)
	     (let ((type (intern (tag-get-attr elem :|type|) 'keyword))
		   (iter (gtk:tree-store-append (app-data app) parent-iter)))

	       (update-row app
			   iter
			   (load-entry type elem))

	       (when (eql type :|folder|)
		 (iter (for ch in (tag-children elem))
		       (parse ch iter)))))

	   (parse (elem parent-iter)
	     (when (is-tag elem)
	       (cond
		 ;; toplevel
		 ((eq (tag-name elem) :|revelationdata|)

		  (iter (for ch in (tag-children elem))
			(parse ch nil)))

		 ((eq (tag-name elem) :|entry|)

		  (load-entry2 elem parent-iter))))))

    (parse xml nil)))

(defmacro tree-foreach-collect (iter model parent-iter &body body)
  `(let ((,iter (if ,parent-iter
		    (gtk:tree-model-iter-first-child ,model ,parent-iter)
		    (gtk:tree-model-iter-first ,model))))
     (when ,iter
       (loop
	  collect (progn ,@body)
	  while (gtk:tree-model-iter-next ,model ,iter)))))

(defun save-data (app filename)
  (let ((data (app-data app)))

    (labels ((traverse (parent-iter)
	       (tree-foreach-collect iter data parent-iter
				     (let ((entry (gtk:tree-model-value data iter 0)))
				       (if (is-group entry)
					   (append (save-entry entry) (traverse iter))
					   (save-entry entry))))))

      (let ((xml (list* (list :|revelationdata| :|version| "0.4.11" :|dataversion| "1")
			(traverse nil))))

	(unless (app-password app)
	  (let ((password (edit-object nil (app-main-window app) "Enter password" "ps-pass-storage"
				       '((nil "Password" :entry :required :password)))))
	    (unless password
	      (return-from save-data))
	    (setf (app-password app) (car password))))

	(save-revelation-file filename (app-password app) xml)
	(setf (app-changed app) nil)))))

(defun cb-new (app)
  (when (ensure-data-is-saved app)
    (setf (app-changed app) nil)
    (gtk:tree-store-clear (app-data app))
    (setf (app-filename app) nil)
    (setf (app-password app) nil)))

(defun open-file (app filename)
  (loop
     for password = (edit-object nil (app-main-window app) "Enter password" "ps-pass-storage"
				 '((nil "Password" :entry :required :password)))

     while password

     do (handler-case
	    (let ((xml (load-revelation-file filename (car password))))
	      (gtk:tree-store-clear (app-data app))
	      (load-data app xml)
	      (setf (app-filename app) filename)
	      (setf (app-password app) (car password))
	      (setf (app-changed app) nil)
	      (return-from open-file))
	  (error (e)
	    (declare (ignore e))
	    (say-error (app-main-window app) "Can't open this file.")))))

(defun cb-open (app)
  (when (ensure-data-is-saved app)
    (let ((dlg (make-instance 'gtk:file-chooser-dialog
			      :action :open
			      :title "Open file"
			      :window-position :center-on-parent
			      :transient-for (app-main-window app))))

      (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
      (gtk:dialog-add-button dlg "gtk-open" :ok)
      (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))
      (setf (gtk:dialog-default-response dlg) :ok)

      (when (std-dialog-run dlg)
	(open-file app (gtk:file-chooser-filename dlg))))))

(defun cb-save-as (app)
  (let ((dlg (make-instance 'gtk:file-chooser-dialog
			    :action :save
			    :title "Save file"
			    :window-position :center-on-parent
			    :transient-for (app-main-window app))))

    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-save" :ok)
    (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))
    (setf (gtk:dialog-default-response dlg) :ok)

    (when (std-dialog-run dlg)
      (setf (app-filename app) (gtk:file-chooser-filename dlg))
      (save-data app (gtk:file-chooser-filename dlg)))))

(defun cb-save (app)
  (if (app-filename app)
      (save-data app (app-filename app))
      (cb-save-as app)))

(defun cb-preferences (app)
  (edit-object *config* (app-main-window app) "Preferences" "gtk-preferences"
	       '((default-file "Default path" :filename)
		 (search-in-secrets "Search in secrets (passwords)" :boolean))))

(defun cb-copy-name (app)
  (let ((data (app-data app))
	(iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
	(gtk:clipboard-set-text
	 (gtk:get-clipboard "CLIPBOARD")
	 (entry-get-name entry))))))

(defun cb-copy-password (app)
  (let ((data (app-data app))
	(iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
	(gtk:clipboard-set-text
	 (gtk:get-clipboard "CLIPBOARD")
	 (entry-get-password entry))))))

(defun cb-about (app)
  (let ((dlg (make-instance 'gtk:about-dialog
			    :window-position :center-on-parent
			    :transient-for (app-main-window app)
			    :authors '("Andrey Kutejko <andy128k@gmail.com>")
			    :copyright "Copyright 2009, Andrey Kutejko"
			    :logo (gtk:widget-render-icon (app-main-window app) "ps-pass-storage" :dialog "")
			    :program-name "PassStorage"
			    :version "0.10.2.8"
			    :website "http://andy128k.github.com/PassStorage")))
    (gtk:dialog-run dlg)
    (gtk:object-destroy dlg)))

(defun make-menu (group &rest actions)
  (let ((menu (make-instance 'gtk:menu)))
    (iter (for action in actions)
	  (if action
	      (gtk:menu-shell-append menu (gtk:action-create-menu-item (gtk:action-group-action group action)))
	      (gtk:menu-shell-append menu (make-instance 'gtk:menu-item :visible t))))
    menu))

(defun make-icon-set (paths)
  (let ((set (make-instance 'gtk:icon-set)))
    (iter (for path in paths)
	  (let ((source (make-instance 'gtk:icon-source)))
	    (setf (gtk:icon-source-filename source) (namestring path))
	    (gtk:icon-set-add-source set source)))
    set))

(defmacro lambda-u (&body body)
  (let ((p (gensym)))
    `(lambda (&rest ,p)
       (declare (ignore ,p))
       (handler-case
	   (progn ,@body)
	 (error (c)
	   ;; TODO: use parent-window
	   (say-error nil (format nil "~A" c)))))))

(defmacro create-action (group action-params &optional accel func)
  `(let ((action (make-instance 'gtk:action ,@action-params)))
     (gtk:action-group-add-action ,group action :accelerator ,accel)
     ,(when func
       `(gobject:connect-signal action "activate" ,func))))

(defun entry-icon-by-class (class)
  (let ((dummy-entry (make-instance class)))
    (entry-icon dummy-entry)))

(defun entry-title-by-class (class)
  (let ((dummy-entry (make-instance class)))
    (entry-title dummy-entry)))

;; search
(defgeneric entry-satisfies (entry model iter text))

(defmethod entry-satisfies ((entry entry-group) model iter text)
  (or
   (entry-has-text entry
		   text
		   :look-at-secrets (config-search-in-secrets *config*))

   (iter (for i in-tree-model model children-of iter)
	 (thereis (entry-satisfies (gtk:tree-store-value model i 0) model i text)))))

(defmethod entry-satisfies (entry model iter text)
  (entry-has-text entry
		  text
		  :look-at-secrets (config-search-in-secrets *config*)))

(defun main ()
  ;; TODO: initialize random

  (load-config)

  ;; stock icons
  (let ((icons (make-hash-table :test 'equal))
	(icons-directory (make-pathname
			  :defaults (directory-namestring (cl-binary-location:location))
			  :directory (append (pathname-directory (directory-namestring (cl-binary-location:location))) '("icons")))))
    ;; find all icons
    (fad:walk-directory
     icons-directory
     (lambda (fn)
       (push fn (gethash (pathname-name fn) icons))))

    ;; register stock icons
    (let ((factory (make-instance 'gtk:icon-factory)))
      (iter (for (icon-name files) in-hashtable icons)
	    (gtk:icon-factory-add factory
				  (concatenate 'string "ps-" icon-name)
				  (make-icon-set files)))

      (gtk:icon-factory-add-default factory)))

  (let* ((data (make-instance 'gtk:tree-store :column-types '("GObject" "gchararray" "gchararray")))
	 (app (make-app
	       :data data
	       :filter (make-instance 'gtk:tree-model-filter :child-model data)
	       :action-group (make-instance 'gtk:action-group :name "action-group")))
	 (ui (make-instance 'gtk:ui-manager)))

    (let ((action-group (app-action-group app)))
      (create-action action-group (:name "file-menu" :label "_File"))
      (create-action action-group (:name "new" :stock-id "gtk-new") "<Control>n" (lambda-u (cb-new app)))
      (create-action action-group (:name "open" :stock-id "gtk-open") "<Control>o" (lambda-u (cb-open app)))
      (create-action action-group (:name "save" :stock-id "gtk-save") "<Control>s" (lambda-u (cb-save app)))
      (create-action action-group (:name "save-as" :stock-id "gtk-save-as") nil (lambda-u (cb-save-as app)))
      (create-action action-group (:name "quit" :stock-id "gtk-quit") "<Control>q" (lambda-u (e-close app)))

      (create-action action-group (:name "edit-menu" :label "_Edit"))
      (create-action action-group (:name "find" :label "_Find") "<Control>f")
      (create-action action-group (:name "copy-name" :label "Copy _name") "<Control>c" (lambda-u (cb-copy-name app)))
      (create-action action-group (:name "copy-password" :label "Copy pass_word") "<Control><Shift>c" (lambda-u (cb-copy-password app)))
      (create-action action-group (:name "preferences" :stock-id "gtk-preferences") nil (lambda-u (cb-preferences app)))

      (create-action action-group (:name "entry-menu" :label "E_ntry"))

      (loop
	 for class1 in (list 'entry-group
			     'entry-generic
			     'entry-creditcard
			     'entry-cryptokey
			     'entry-database
			     'entry-door
			     'entry-email
			     'entry-ftp
			     'entry-phone
			     'entry-shell
			     'entry-website)
	 do
	   (let ((class class1))
	     (create-action action-group
			    (:name (format nil "add-~(~A~)" class)
			     :stock-id (entry-icon-by-class class)
			     :label (format nil "Add ~A" (entry-title-by-class class)))
			    nil
			    (lambda-u (cb-add-item app class)))))

      (create-action action-group (:name "edit" :stock-id "gtk-edit" :sensitive nil) nil (lambda-u (cb-edit-entry app)))
      (create-action action-group (:name "delete" :stock-id "gtk-delete" :sensitive nil) nil (lambda-u (cb-del-entry app)))

      (create-action action-group (:name "help-menu" :label "_Help"))
      (create-action action-group (:name "about" :stock-id "gtk-about") nil (lambda-u (cb-about app)))

      (gtk:ui-manager-insert-action-group ui action-group 0))

    (gtk:ui-manager-add-ui-from-string ui
"<ui>
  <menubar name='menubar'>
    <menu action='file-menu'>
      <menuitem action='new'/>
      <menuitem action='open'/>
      <menuitem action='save'/>
      <menuitem action='save-as'/>
      <separator/>
      <menuitem action='quit'/>
    </menu>
    <menu action='edit-menu'>
      <menuitem action='find'/>
      <separator/>
      <menuitem action='copy-name'/>
      <menuitem action='copy-password'/>
      <separator/>
      <menuitem action='preferences'/>
    </menu>
    <menu action='entry-menu'>
      <menuitem action='add-entry-group'/>
      <separator/>
      <menuitem action='add-entry-generic'/>
      <menuitem action='add-entry-creditcard'/>
      <menuitem action='add-entry-cryptokey'/>
      <menuitem action='add-entry-database'/>
      <menuitem action='add-entry-door'/>
      <menuitem action='add-entry-email'/>
      <menuitem action='add-entry-ftp'/>
      <menuitem action='add-entry-phone'/>
      <menuitem action='add-entry-shell'/>
      <menuitem action='add-entry-website'/>
      <separator/>
      <menuitem action='edit'/>
      <menuitem action='delete'/>
    </menu>
    <menu action='help-menu'>
      <menuitem action='about'/>
    </menu>
  </menubar>
  <toolbar name='toolbar'>
    <toolitem action='edit'/>
    <toolitem action='delete'/>
  </toolbar>
</ui>")

    (gtk:let-ui

     (gtk:gtk-window
      :var main-window
      :title "PassStorage"
      :window-position :center
      :default-width 600
      :default-height 450
      (gtk:v-box

       (:expr (gtk:ui-manager-widget ui "/menubar"))
       :expand nil
       :position 0

       (:expr (gtk:ui-manager-widget ui "/toolbar"))
       :expand nil
       :position 1

       (gtk:h-box
	:border-width 4
	:spacing 8
	(gtk:label :label "Find:")
	:expand nil
	(gtk:entry
	 :var search-entry)
	:expand nil
	)
       :expand nil
       :position 2

       (gtk:scrolled-window
	:can-focus t
	:hscrollbar-policy :automatic
	:vscrollbar-policy :automatic
	:shadow-type :in
	(gtk:tree-view
	 :var view
	 :can-focus t
	 :model (app-filter app)
	 :headers-visible nil
	 :reorderable t
	 :search-column 1)
	:position 3)))

     (gtk:toolbar-insert (gtk:ui-manager-widget ui "/toolbar")
			 (make-instance 'gtk:menu-tool-button
					:stock-id "gtk-add"
					:label "Add entry"
					:related-action (gtk:action-group-action (app-action-group app) "add-entry-generic")
					:menu (apply #'make-menu
						     (list
						      (app-action-group app)
						      "add-entry-group"
						      nil
						      "add-entry-generic"
						      "add-entry-creditcard"
						      "add-entry-cryptokey"
						      "add-entry-database"
						      "add-entry-door"
						      "add-entry-email"
						      "add-entry-ftp"
						      "add-entry-phone"
						      "add-entry-shell"
						      "add-entry-website")))
			 0)

     (let ((col (make-instance 'gtk:tree-view-column :sizing :autosize))
	   (rnd1 (make-instance 'gtk:cell-renderer-pixbuf :stock-size 1))
	   (rnd2 (make-instance 'gtk:cell-renderer-text)))
       (gtk:tree-view-column-pack-start col rnd1 :expand nil)
       (gtk:tree-view-column-add-attribute col rnd1 "stock-id" 2)
       (gtk:tree-view-column-pack-start col rnd2 :expand t)
       (gtk:tree-view-column-add-attribute col rnd2 "text" 1)
       (gtk:tree-view-append-column view col))

     (gtk:tree-model-filter-set-visible-function (app-filter app)
						 (lambda (model iter)
						   (when iter
						     (let ((entry (gtk:tree-store-value model iter 0)))
						       (when entry
							 (entry-satisfies entry model iter (gtk:entry-text search-entry)))))))

     (gobject:connect-signal (gtk:action-group-action (app-action-group app) "find") "activate"
			     (lambda-u
			      (gtk:widget-grab-focus search-entry)))
     (gobject:connect-signal search-entry "changed"
			     (lambda-u
			      (gtk:tree-model-filter-refilter (app-filter app))
			      (gtk:tree-view-expand-all (app-view app))))

     (setf (gtk:gtk-window-icon main-window)
	   (gtk:widget-render-icon main-window "ps-pass-storage" :dialog ""))

     (setf (app-main-window app) main-window)
     (setf (app-view app) view))

    (gobject:connect-signal (app-main-window app) "delete-event" (lambda-u (e-close app)))

    (gobject:connect-signal (app-view app) "cursor-changed" (lambda-u (listview-cursor-changed app)))
    (gobject:connect-signal (app-view app) "drag-motion"
			    (lambda (widget drag-context x y time)
			      (multiple-value-bind (path pos)
				  (gtk:tree-view-get-dest-row-at-pos widget x y)

				(when (and path (or (eq pos :into-or-before) (eq pos :into-or-after)))
				  (let* ((model (gtk:tree-view-model widget))
					 (iter (gtk:tree-model-iter-by-path model path)))

				    (if (is-group (gtk:tree-store-value model iter 0))
					(progn (gdk:gdk-drag-status drag-context :move time) nil)
					(progn (gdk:gdk-drag-status drag-context 0 time) t)))))))

    (gobject:connect-signal (app-view app) "row-activated"
			    (lambda-u
			     (let* ((data (app-data app))
				    (view (app-view app))
				    (selection (gtk:tree-view-selection view)))

			       (when selection
				 (let ((filter-iter (gtk:tree-selection-selected selection)))
				   (when filter-iter
				     (let* ((iter (gtk:tree-model-filter-convert-iter-to-child-iter (app-filter app) filter-iter))
					    (entry (gtk:tree-store-value data iter 0)))
				       (if (is-group entry)
					   (let ((path (gtk:tree-model-path (app-filter app) filter-iter)))
					     (if (gtk:tree-view-row-expanded-p view path)
						 (gtk:tree-view-collapse-row view path)
						 (gtk:tree-view-expand-row view path)))
					   (cb-edit-entry app)))))))))

    (gtk:gtk-window-add-accel-group (app-main-window app) (gtk:ui-manager-accel-group ui))

    (gtk:widget-show (app-main-window app))

    (let ((default-file (config-default-file *config*)))
      (when default-file
	(gtk:gtk-main-add-timeout 1
				  (lambda ()
				    (gdk:gdk-threads-enter)
				    (open-file app default-file)
				    (gdk:gdk-threads-leave)
				    nil))))

    (gtk:gtk-main)

    (save-config)))

(export 'main)

(defun main-and-quit ()
  (main)
  #+sbcl(sb-ext:quit)
  #+clozure(ccl:quit))

(export 'main-and-quit)

