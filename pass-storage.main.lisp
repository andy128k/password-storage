(in-package :pass-storage)

(defstruct app
  main-window
  data
  view
  action-new
  action-open
  action-save
  action-save-as
  action-quit
  action-add-group
  action-add-generic
  action-add-creditcard
  action-add-cryptokey
  action-add-database
  action-edit
  action-delete
  action-about)

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

(defun listview-cursor-changed (app)
  (let ((s (get-selected-iter (app-view app))))
    (setf (gtk:action-sensitive (app-action-edit app)) s)
    (setf (gtk:action-sensitive (app-action-delete app)) s)))

(defun update-row (app iter entry)
  (let ((data (app-data app)))
    (setf (gtk:tree-store-value data iter 0) entry)
    (setf (gtk:tree-store-value data iter 1) (entry-name entry))
    (setf (gtk:tree-store-value data iter 2) (entry-icon entry))
    (setf (gtk:tree-view-model (app-view app)) data)))

(defun cb-add-group (app)
  (let ((entry (make-instance 'entry-group)))
    (when (edit-entry entry (app-main-window app) "Add")
      (let ((iter (gtk:tree-store-append (app-data app)
					 (get-selected-group-iter (app-view app)))))
	(update-row app iter entry)))))

(defun cb-add-item (app type)
  (let ((entry (make-instance type)))
    (when (edit-entry entry (app-main-window app) "Add")
      (let ((iter (gtk:tree-store-append (app-data app)
					 (get-selected-group-iter (app-view app)))))
	(update-row app iter entry)))))

(defun cb-edit-entry (app)
  (let ((data (app-data app))
	(iter (get-selected-iter (app-view app))))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
	(when (edit-entry entry (app-main-window app) "Edit")
	  (update-row app iter entry))))))

(defun cb-del-entry (app)
  (let ((iter (get-selected-iter (app-view app))))
    (when (and iter
	       (ask "Do you really want to delete selected item?"))
      (let* ((data (app-data app)))
	(gtk:tree-store-remove data iter)
	(setf (gtk:tree-view-model (app-view app)) data)
	(listview-cursor-changed app)))))

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
			      (make-instance 'entry-group :name (entry-node-get-value elem :|name|)))
		  (iter (for ch in (tag-children elem))
			(parse ch iter)))

		 ((string= type "generic")
		  (update-row app
			      iter
			      (make-instance 'entry-generic
					     :name (entry-node-get-value elem :|name|)
					     :description (entry-node-get-value elem :|description|)
					     :username (entry-node-get-value elem :|field| "generic-username")
					     :password (entry-node-get-value elem :|field| "generic-password")
					     :hostname (entry-node-get-value elem :|field| "generic-hostname")))))))

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

(defun make-menu (&rest actions)
  (let ((menu (make-instance 'gtk:menu)))
    (iter (for action in actions)
	  (if action
	      (gtk:menu-shell-append menu (gtk:action-create-menu-item action))
	      (gtk:menu-shell-append menu (make-instance 'gtk:menu-item :visible t))))
    menu))

(defun main ()
  (let ((app (make-app
	      :data 	             (make-instance 'gtk:tree-store :column-types '("GObject" "gchararray" "gchararray"))
	      :action-new            (make-instance 'gtk:action :stock-id "gtk-new")
	      :action-open           (make-instance 'gtk:action :stock-id "gtk-open")
	      :action-save           (make-instance 'gtk:action :stock-id "gtk-save")
	      :action-save-as        (make-instance 'gtk:action :stock-id "gtk-save-as")
	      :action-quit           (make-instance 'gtk:action :stock-id "gtk-quit")
	      :action-add-group      (make-instance 'gtk:action :stock-id "gtk-directory" :label "_Add group")
	      :action-add-generic    (make-instance 'gtk:action :stock-id "gtk-file" :label "Add generic _entry")
	      :action-add-creditcard (make-instance 'gtk:action :icon-name "stock_creditcard" :label "Add _credit card")
	      :action-add-cryptokey  (make-instance 'gtk:action :icon-name "stock_keyring" :label "Add c_rypto key")
	      :action-add-database   (make-instance 'gtk:action :icon-name "stock_data-sources" :label "Add _database")
	      :action-edit           (make-instance 'gtk:action :stock-id "gtk-edit" :sensitive nil)
	      :action-delete         (make-instance 'gtk:action :stock-id "gtk-delete" :sensitive nil)
	      :action-about          (make-instance 'gtk:action :stock-id "gtk-about"))))

    (gtk:let-ui

     (gtk:gtk-window
      :var main-window
      :title "PassStorage"
      :window-position :center
      :default-width 600
      :default-height 450
      (gtk:v-box

       (gtk:menu-bar

	(gtk:menu-item
	 :visible t
	 :label "_File"
	 :use-underline t
	 :submenu (make-menu
		   (app-action-new app)
		   (app-action-open app)
		   (app-action-save app)
		   (app-action-save-as app)
		   nil
		   (app-action-quit app)))

	(gtk:menu-item
	 :visible t
	 :label "_Edit"
	 :use-underline t
	 :submenu (make-menu
		   (app-action-add-group app)
		   nil
		   (app-action-add-generic app)
		   (app-action-add-creditcard app)
		   (app-action-add-cryptokey app)
		   (app-action-add-database app)
		   nil
		   (app-action-edit app)
		   (app-action-delete app)))

	(gtk:menu-item
	 :visible t
	 :label "_Help"
	 :use-underline t
	 :submenu (make-menu (app-action-about app))))
       :expand nil
       :position 0

       (gtk:toolbar
	(gtk:menu-tool-button
	 :stock-id "gtk-add"
	 :label "Add entry"
	 :related-action (app-action-add-generic app)
	 :menu (make-menu
		(app-action-add-group app)
		nil
		(app-action-add-generic app)
		(app-action-add-creditcard app)
		(app-action-add-cryptokey app)
		(app-action-add-database app)))

	(:expr (gtk:action-create-tool-item (app-action-edit app)))
	(:expr (gtk:action-create-tool-item (app-action-delete app))))

       :expand nil
       :position 1

       (gtk:scrolled-window
	:can-focus t
	:hscrollbar-policy :automatic
	:vscrollbar-policy :automatic
	:shadow-type :in
	(gtk:tree-view
	 :var view
	 :can-focus t
	 :model (app-data app)
	 :headers-visible nil
	 :reorderable t
	 :search-column 1)
	:position 2)))

     (let ((col (make-instance 'gtk:tree-view-column :sizing :autosize))
	   (rnd1 (make-instance 'gtk:cell-renderer-pixbuf :stock-size 1))
	   (rnd2 (make-instance 'gtk:cell-renderer-text)))
       (gtk:tree-view-column-pack-start col rnd1 :expand nil)
       (gtk:tree-view-column-add-attribute col rnd1 "icon-name" 2)
       (gtk:tree-view-column-pack-start col rnd2 :expand t)
       (gtk:tree-view-column-add-attribute col rnd2 "text" 1)
       (gtk:tree-view-append-column view col))

     (setf (app-main-window app) main-window)
     (setf (app-view app) view))

    (gobject:connect-signal (app-main-window app) "destroy" (lambda-u (e-close)))
    (gobject:connect-signal (app-action-quit app) "activate" (lambda-u (e-close)))

    (gobject:connect-signal (app-view app) "cursor-changed" (lambda-u (listview-cursor-changed app)))
    (gobject:connect-signal (app-view app) "drag-motion"
			    (lambda (widget drag-context x y time)
			      (multiple-value-bind (path pos)
				  (gtk:tree-view-get-dest-row-at-pos widget x y)

				(when (and path (or (eq pos :into-or-before) (eq pos :into-or-after)))
				  (let* ((model (gtk:tree-view-model widget))
					 (iter (gtk:tree-model-iter-by-path model path)))

				    (if (is-group (gtk:tree-store-value model iter 0))
					(progn (gdk:drag-status drag-context :move time) nil)
					(progn (gdk:drag-status drag-context 0 time) t)))))))

    (gobject:connect-signal (app-action-add-group app)      "activate" (lambda-u (cb-add-group app)))
    (gobject:connect-signal (app-action-add-generic app)    "activate" (lambda-u (cb-add-item app 'entry-generic)))
    (gobject:connect-signal (app-action-add-creditcard app) "activate" (lambda-u (cb-add-item app 'entry-creditcard)))
    (gobject:connect-signal (app-action-add-cryptokey app)  "activate" (lambda-u (cb-add-item app 'entry-cryptokey)))
    (gobject:connect-signal (app-action-add-database app)   "activate" (lambda-u (cb-add-item app 'entry-database))) 
    (gobject:connect-signal (app-action-edit app)           "activate" (lambda-u (cb-edit-entry app)))
    (gobject:connect-signal (app-action-delete app)         "activate" (lambda-u (cb-del-entry app)))

    ;; (load-data app "./data" "Nd3e")

    (gtk:widget-show (app-main-window app))
    (gtk:gtk-main)

    ;; (save-data lst "./data")

    ))

(export 'main)

