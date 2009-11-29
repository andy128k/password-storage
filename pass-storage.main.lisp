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
       ,@body)))

(defun make-add-entry-action (func class)
  (let* ((dummy-entry (make-instance class))
	 (action (make-instance 'gtk:action
				:stock-id (entry-icon dummy-entry)
				:label (format nil "Add ~A" (entry-title dummy-entry)))))
    
    (gobject:connect-signal action "activate" (lambda-u (funcall func class)))
    action))

(defun main ()

  ;; stock icons
  (let ((icons (make-hash-table :test 'equal))
	(icons-directory (make-pathname
			  :defaults (directory-namestring cl-binary-location:*location*)
			  :directory (append (pathname-directory (directory-namestring cl-binary-location:*location*)) '("icons")))))
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

  (let* ((app (make-app
	       :data 	       (make-instance 'gtk:tree-store :column-types '("GObject" "gchararray" "gchararray"))
	       :action-new     (make-instance 'gtk:action :stock-id "gtk-new")
	       :action-open    (make-instance 'gtk:action :stock-id "gtk-open")
	       :action-save    (make-instance 'gtk:action :stock-id "gtk-save")
	       :action-save-as (make-instance 'gtk:action :stock-id "gtk-save-as")
	       :action-quit    (make-instance 'gtk:action :stock-id "gtk-quit")
	       :action-edit    (make-instance 'gtk:action :stock-id "gtk-edit" :sensitive nil)
	       :action-delete  (make-instance 'gtk:action :stock-id "gtk-delete" :sensitive nil)
	       :action-about   (make-instance 'gtk:action :stock-id "gtk-about")))

	 (add-actions (mapcar (lambda (class)
				(when class
				  (make-add-entry-action
				   (lambda (class) (cb-add-item app class))
				   class)))
			      (list 'entry-group
				    nil
				    'entry-generic
				    'entry-creditcard
				    'entry-cryptokey
				    'entry-database
				    'entry-door
				    'entry-email
				    'entry-ftp
				    'entry-phone
				    'entry-shell
				    'entry-website))))

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
	 :submenu (apply #'make-menu `(,@add-actions
				       nil
				       ,(app-action-edit app)
				       ,(app-action-delete app))))

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
	 :related-action (third add-actions) ; add-generic
	 :menu (apply #'make-menu add-actions))

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
       (gtk:tree-view-column-add-attribute col rnd1 "stock-id" 2)
       (gtk:tree-view-column-pack-start col rnd2 :expand t)
       (gtk:tree-view-column-add-attribute col rnd2 "text" 1)
       (gtk:tree-view-append-column view col))

     (setf (gtk:gtk-window-icon main-window)
	   (gtk:widget-render-icon main-window "ps-stock-pass-storage" :dialog ""))

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

    (gobject:connect-signal (app-action-edit app)   "activate" (lambda-u (cb-edit-entry app)))
    (gobject:connect-signal (app-action-delete app) "activate" (lambda-u (cb-del-entry app)))

    (load-data app "./data" "Nd3e")

    (gtk:widget-show (app-main-window app))
    (gtk:gtk-main)

    ;; (save-data lst "./data")

    ))

(export 'main)

