(in-package :pass-storage)

(defstruct app
  main-window
  data
  view
  column
  check-renderer
  current-icon
  current-title
  current-description
  current-view
  actions-common
  actions-edit
  actions-merge
  filter
  search-entry
  statusbar

  filename
  password
  changed)

(defun set-status (app message)
  (let ((statusbar (app-statusbar app)))
    (gtk:statusbar-pop statusbar 0)
    (gtk:statusbar-push statusbar 0 message)))

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

      (let ((current-model (gtk:tree-view-model (app-view app))))
        (cond

          ((eq current-model (app-data app))
           (let ((iter (gtk:tree-selection-selected selection)))
             (when iter
               (values iter
                       (gtk:tree-model-path (app-data app) iter)))))

          ((eq current-model (app-filter app))
           (let ((filter-iter (gtk:tree-selection-selected selection)))
             (when filter-iter
               (values (gtk:tree-model-filter-convert-iter-to-child-iter (app-filter app)
                                                                         filter-iter)
                       (gtk:tree-model-path (app-filter app) filter-iter))))))))))

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
    (loop
       for action in '("copy-name"
                       "copy-password"
                       "edit"
                       "delete")
       do
         (setf (gtk:action-sensitive (gtk:action-group-action (app-actions-edit app) action)) s))

    (let ((entry (and s (gtk:tree-model-value (app-data app) s 0))))
      (loop
         for class in (list 'entry-generic
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
           (setf (gtk:action-sensitive (gtk:action-group-action (app-actions-edit app) (format nil "convert-to-~(~A~)" class)))
                 (and s
                      (not (is-group entry))
                      (not (eql (find-class class) (class-of entry)))))))

    (if s
        (let ((entry (gtk:tree-store-value (app-data app) s 0)))
          (setf (gtk:image-stock (app-current-icon app))
                (entry-icon entry))

          (setf (gtk:label-label (app-current-title app))
                (format nil "<big><b>~A</b></big>"
                        (markup-escape-text (entry-name entry))))

          (setf (gtk:label-label (app-current-description app))
                (entry-description entry))

          (setf (gtk:label-label (app-current-view app))
                (entry-to-markup entry :show-secrets (config-show-secrets-on-preview *config*))))

        ;; else
        (setf (gtk:image-stock (app-current-icon app)) ""
              (gtk:label-label (app-current-title app)) ""
              (gtk:label-label (app-current-description app)) ""
              (gtk:label-label (app-current-view app)) ""))))

(defun update-row (data iter entry)
  (setf (gtk:tree-store-value data iter 0) entry)
  (setf (gtk:tree-store-value data iter 1) (entry-name entry))
  (setf (gtk:tree-store-value data iter 2) (entry-icon entry)))

(defun select-iter (app iter)
  (let* ((current-model (gtk:tree-view-model (app-view app)))
         (iter-to-select
          (cond
            ((eq current-model (app-data app))
             iter)
            ((eq current-model (app-filter app))
             (gtk:tree-model-filter-convert-child-iter-to-iter (app-filter app) iter)))))
    (when iter-to-select
      (let ((path-to-select (gtk:tree-model-path current-model iter-to-select)))
        (gtk:tree-view-expand-to-path (app-view app) path-to-select)
        (gtk:tree-view-set-cursor (app-view app) path-to-select)))))

(defun cb-add-item (app type)
  (let ((entry (make-instance type)))
    (when (edit-entry entry (app-main-window app) "Add")
      (let ((iter (gtk:tree-store-append (app-data app)
                                         (get-selected-group-iter app))))
        (update-row (app-data app) iter entry)
        (gtk:tree-model-filter-refilter (app-filter app))
        (select-iter app iter)
        (set-status app "New entry was added")
        (setf (app-changed app) t)))))

(defun cb-edit-entry (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
        (when (edit-entry entry (app-main-window app) "Edit")
          (update-row (app-data app) iter entry)
          (gtk:tree-model-filter-refilter (app-filter app))
          (listview-cursor-changed app)
          (set-status app "Entry was changed")
          (setf (app-changed app) t))))))

(defun cb-convert-entry (app dest-class)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
        (update-row (app-data app) iter (copy-entry entry dest-class))
        (gtk:tree-model-filter-refilter (app-filter app))
        (set-status app "Entry has changed type")
        (setf (app-changed app) t)
        (listview-cursor-changed app)))))

(defun cb-del-entry (app)
  (let ((iter (get-selected-iter app)))
    (when (and iter
               (ask (app-main-window app) "Do you really want to delete selected item?"))
      (let* ((data (app-data app)))
        (gtk:tree-store-remove data iter)
        (listview-cursor-changed app)
        (set-status app "Entry was deleted")
        (setf (app-changed app) t)))))

(defun cb-uncheck-all (app)
  (labels ((uncheck (model parent)
             (iter (for i in-tree-model model children-of parent)
                   (setf (gtk:tree-store-value model i 3) nil)
                   (uncheck model i))))
    (uncheck (app-data app) nil)
    (set-status app "Unchecked all items")))

(defun cb-merge (app)
  (let ((checked
         (labels ((collect-checked (model parent path)
                    (iter (for i in-tree-model model children-of parent)
                          (let ((entry (gtk:tree-model-value model i 0)))
                            (appending (collect-checked model i (cons (entry-name entry) path)))
                            (when (gtk:tree-store-value model i 3)
                              (collect (list entry (reverse path))))))))
           (collect-checked (app-data app) nil nil))))

    (when (< (length checked) 2)
      (say-info (app-main-window app) "Nothing to merge. Select few items and try again.")
      (return-from cb-merge))

    (unless (ask (app-main-window app)
                 (with-output-to-string (str)
                   (format str "Do you want to merge following items?~%")
                   (iter (for (entry path) in checked)
                         (format str "~%~{~A / ~}~A" path (entry-name entry)))))
      (return-from cb-merge))

    ;; delete entries
    (labels ((delete-checked (model parent)
               (let ((i (if parent
                            (gtk:tree-model-iter-first-child model parent)
                            (gtk:tree-model-iter-first model))))
                 (when i
                   (iter (while
                             (if (gtk:tree-store-value model i 3)
                                 (gtk:tree-store-remove model i)
                                 (progn
                                   (delete-checked model i)
                                   (gtk:tree-model-iter-next model i)))))))))
      (delete-checked (app-data app) nil))

    ;; create new entry
    (let ((result (make-instance (or (equal-together (mapcar (lambda (c) (class-of (first c))) checked))
                                     (find-class 'entry-generic)))))

      (iter (for (entry path) in checked)
            (join-entry result path entry))

      (setf (entry-name result)
            (let ((names (mapcar (lambda (c) (entry-name (first c))) checked)))
              (or (equal-together names)
                  (format nil "~A ~{ and ~A~}" (car names) (cdr names)))))

      ;; TODO: detect common path
      (let ((iter (gtk:tree-store-append (app-data app) nil)))
        (update-row (app-data app) iter result)
        (gtk:tree-model-filter-refilter (app-filter app))
        (select-iter app iter)
        (set-status app "New entry was created by merging")
        (setf (app-changed app) t)))))

(defun make-data ()
  (make-instance 'gtk:tree-store :column-types '("GObject" "gchararray" "gchararray" "gboolean")))

(defun load-data (filename parent-window)
  (labels ((parse-entry (elem data parent-iter)
             (let ((type (intern (tag-get-attr elem :|type|) 'keyword))
                   (iter (gtk:tree-store-append data parent-iter)))
               (update-row data
                           iter
                           (load-entry type elem))
               (when (eql type :|folder|)
                 (iter (for ch in (tag-children elem))
                       (parse ch data iter)))))

           (parse (elem data parent-iter)
             (when (is-tag elem)
               (cond
                 ;; toplevel
                 ((eq (tag-name elem) :|revelationdata|)
                  (iter (for ch in (tag-children elem))
                        (parse ch data nil)))
                 ((eq (tag-name elem) :|entry|)
                  (parse-entry elem data parent-iter))))))

  (loop
     for password = (edit-object nil parent-window "Enter password" "ps-pass-storage"
                                 '((nil "Password" :entry :required :password)))

     while password

     do (handler-case
            (let ((xml (load-revelation-file filename (car password)))
                  (data (make-data)))
              (parse xml data nil)
              (return-from load-data (values data
                                             (car password))))
          (error (e)
            (declare (ignore e))
            (say-error parent-window "Can't open this file."))))))

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
        (set-status app "File was saved")
        (setf (app-changed app) nil)))))

(defun cb-new (app)
  (when (ensure-data-is-saved app)
    (setf (app-changed app) nil)
    (gtk:tree-store-clear (app-data app))
    (setf (gtk:toggle-action-active (gtk:action-group-action (app-actions-common app) "merge-mode"))
          nil)
    (setf (app-filename app) nil)
    (setf (app-password app) nil)
    (listview-cursor-changed app)
    (set-status app "New file was created")))

(defun set-data (app data)
  (setf (app-data app) data
        (gtk:tree-view-model (app-view app)) data
        (app-filter app) (make-instance 'gtk:tree-model-filter :child-model data))
  (set-filter-function app))

(defun open-file (app filename)
  (multiple-value-bind (data password)
      (load-data filename (app-main-window app))
    (when data
      (set-data app data)
      (setf (app-filename app) filename)
      (setf (app-password app) password)
      (setf (app-changed app) nil)
      (set-status app "File was opened"))))

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

(defun merge-file-by-name (app data)
  (labels ((find-entry (name parent-iter)
             (iter (for i in-tree-model (app-data app) children-of parent-iter)
                   (for entry = (gtk:tree-model-value (app-data app) i 0))
                   (if (is-group entry)
                       (appending (find-entry name i))
                       (when (string-equal (entry-name entry) name)
                         (collecting entry)))))

           (merge-entry (src-iter)
             (let ((entry (gtk:tree-model-value data src-iter 0)))
               (if (is-group entry)
                   (iter (for i in-tree-model data children-of src-iter)
                         (merge-entry i))
                   (progn
                     (let ((dst-entries (find-entry (entry-name entry) nil)))
                       (if dst-entries
                           (join-entry (first dst-entries) nil entry)
                           (update-row (app-data app) (gtk:tree-store-append (app-data app) nil) entry))))))))

    (iter (for i in-tree-model data children-of nil)
          (merge-entry i))))

(defun append-file (app data)
  (labels ((append-entry (src-iter dst-iter)
             (let ((entry (gtk:tree-model-value data src-iter 0))
                   (iter (gtk:tree-store-append (app-data app) dst-iter)))
               (update-row (app-data app) iter entry)
               (when (is-group entry)
                 (iter (for i in-tree-model data children-of src-iter)
                       (append-entry i iter))))))

    (iter (for i in-tree-model data children-of nil)
          (append-entry i nil))))

(defun cb-merge-file (app)
  (let ((r (edit-object nil (app-main-window app) "Merge file" "ps-pass-storage"
                        '((nil ("Merge entries by name (ignore groups)" "Append file") :choice)
                          (nil "File to merge" :filename :required)))))
    (when r
      (destructuring-bind (mode filename) r
        (let ((data (load-data filename (app-main-window app))))
          (when data
            (ecase mode
              (0 (merge-file-by-name app data))
              (1 (append-file app data)))))))))

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

(defun cb-change-password (app)
  (let ((parent-window (app-main-window app)))
    (loop
       (let ((passwords (edit-object nil parent-window "Change password" "ps-pass-storage"
                                     '((nil "Password" :entry :required :password)
                                       (nil "Confirm" :entry :required :password)))))
         (unless passwords
           (return-from cb-change-password))

         (when (string= (first passwords) (second passwords))
           (setf (app-password app) (first passwords))
           (setf (app-changed app) t)
           (return-from cb-change-password))

         (say-warning parent-window "Entered passwords are not identical")))))

(defun cb-preferences (app)
  (edit-object *config* (app-main-window app) "Preferences" "gtk-preferences"
               '((default-file "Default path" :filename)
                 (search-in-secrets "Search in secrets (passwords)" :boolean)
                 (show-secrets-on-preview "Show secrets (passwords) on preview panel" :boolean))))

(defun cb-copy-name (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter

      (let* ((entry (gtk:tree-store-value data iter 0))
             (name (entry-get-name entry)))
        (when name

          (gtk:clipboard-set-text
           (gtk:get-clipboard "CLIPBOARD")
           name)
          (set-status app "Name was copied to clipboard"))))))

(defun cb-copy-password (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter

      (let* ((entry (gtk:tree-store-value data iter 0))
             (password (entry-get-password entry)))
        (when password

          (gtk:clipboard-set-text
           (gtk:get-clipboard "CLIPBOARD")
           password)
          (set-status app "Secret (password) was copied to clipboard"))))))

(defun cb-about (app)
  (let ((dlg (make-instance 'gtk:about-dialog
                            :window-position :center-on-parent
                            :transient-for (app-main-window app)
                            :authors '("Andrey Kutejko <andy128k@gmail.com>")
                            :copyright "Copyright 2009, 2010, Andrey Kutejko"
                            :logo (gtk:widget-render-icon (app-main-window app) "ps-pass-storage" :dialog "")
                            :program-name "PassStorage"
                            :version *ps-version*
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

(defmacro create-toggle-action (group action-params &optional accel func)
  `(let ((action (make-instance 'gtk:toggle-action ,@action-params)))
     (gtk:action-group-add-action ,group action :accelerator ,accel)
     ,(when func
            `(gobject:connect-signal action "toggled" ,func))))

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

(defun set-filter-function (app)
  (gtk:tree-model-filter-set-visible-function
   (app-filter app)
   (lambda (model iter)
     (when iter
       (let ((entry (gtk:tree-store-value model iter 0)))
         (when entry
           (entry-satisfies entry model iter (gtk:entry-text (app-search-entry app)))))))))

(defun location-prefix ()
  (let ((path (pathname-directory (directory-namestring (cl-binary-location:location)))))
    (if (and path
             (string-equal "bin" (car (last path))))
        (butlast path)
        path)))

(defun set-mode (app)
  (if (gtk:toggle-action-active (gtk:action-group-action (app-actions-common app) "merge-mode"))
      (setf (gtk:action-group-visible (app-actions-edit app)) nil
            (gtk:action-group-visible (app-actions-merge app)) t)
      (setf (gtk:action-group-visible (app-actions-edit app)) t
            (gtk:action-group-visible (app-actions-merge app)) nil))
  (gtk:tree-view-column-queue-resize (app-column app)))

(defun main ()

  (glib:random-set-seed (get-universal-time))

  (load-config)

  ;; stock icons
  (let ((icons (make-hash-table :test 'equal))
        (icons-directory (make-pathname
                          :defaults (directory-namestring (cl-binary-location:location))
                          :directory (append (location-prefix) '("share" "pixmaps" "pass-storage")))))
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

  (let* ((data (make-instance 'gtk:tree-store :column-types '("GObject" "gchararray" "gchararray" "gboolean")))
         (app (make-app
               :data data
               :filter (make-instance 'gtk:tree-model-filter :child-model data)
               :actions-common (make-instance 'gtk:action-group :name "actions-common")
               :actions-edit (make-instance 'gtk:action-group :name "actions-edit")
               :actions-merge (make-instance 'gtk:action-group :name "actions-merge")))
         (ui (make-instance 'gtk:ui-manager)))

    (let ((actions-common (app-actions-common app))
          (actions-edit (app-actions-edit app))
          (actions-merge (app-actions-merge app)))
      (create-action actions-common (:name "file-menu" :label "_File"))
      (create-action actions-common (:name "new" :stock-id "gtk-new") "<Control>n" (lambda-u (cb-new app)))
      (create-action actions-common (:name "open" :stock-id "gtk-open") "<Control>o" (lambda-u (cb-open app)))
      (create-action actions-common (:name "merge-file" :label "_Merge file") nil (lambda-u (cb-merge-file app)))
      (create-action actions-common (:name "save" :stock-id "gtk-save") "<Control>s" (lambda-u (cb-save app)))
      (create-action actions-common (:name "save-as" :stock-id "gtk-save-as") nil (lambda-u (cb-save-as app)))
      (create-action actions-common (:name "quit" :stock-id "gtk-quit") "<Control>q" (lambda-u (e-close app)))

      (create-action actions-common (:name "edit-menu" :label "_Edit"))
      (create-action actions-common (:name "find" :label "_Find") "<Control>f")
      (create-action actions-edit (:name "copy-name" :label "Copy _name" :sensitive nil) "<Control>c" (lambda-u (cb-copy-name app)))
      (create-action actions-edit (:name "copy-password" :label "Copy pass_word" :sensitive nil) "<Control><Shift>c" (lambda-u (cb-copy-password app)))
      (create-action actions-common (:name "change-password" :label "Change _password") nil (lambda-u (cb-change-password app)))
      (create-toggle-action actions-common (:name "merge-mode" :label "_Merge mode" :stock-id "ps-stock-merge-mode") nil (lambda-u (set-mode app)))
      (create-action actions-common (:name "preferences" :stock-id "gtk-preferences") nil (lambda-u (cb-preferences app)))

      (create-action actions-common (:name "entry-menu" :label "E_ntry"))

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
             (create-action actions-edit
                            (:name (format nil "add-~(~A~)" class)
                                   :stock-id (entry-icon-by-class class)
                                   :label (format nil "Add ~A" (entry-title-by-class class)))
                            nil
                            (lambda-u (cb-add-item app class)))))

      (create-action actions-edit (:name "edit" :stock-id "gtk-edit" :sensitive nil) nil (lambda-u (cb-edit-entry app)))
      (create-action actions-edit (:name "convert" :label "Convert"))

      (loop
         for class1 in (list 'entry-generic
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
             (create-action actions-edit
                            (:name (format nil "convert-to-~(~A~)" class)
                                   :stock-id (entry-icon-by-class class)
                                   :label (format nil "to ~A" (entry-title-by-class class))
                                   :sensitive nil)
                            nil
                            (lambda-u (cb-convert-entry app class)))))

      (create-action actions-edit (:name "delete" :stock-id "gtk-delete" :sensitive nil) nil (lambda-u (cb-del-entry app)))

      (create-action actions-merge (:name "uncheck-all" :label "Uncheck all") nil (lambda-u (cb-uncheck-all app)))
      (create-action actions-merge (:name "merge" :label "Merge" :stock-id "ps-stock-merge") nil (lambda-u (cb-merge app)))

      (create-action actions-common (:name "help-menu" :label "_Help"))
      (create-action actions-common (:name "about" :stock-id "gtk-about") nil (lambda-u (cb-about app)))

      (gtk:ui-manager-insert-action-group ui actions-common 0)
      (gtk:ui-manager-insert-action-group ui actions-edit 1)
      (gtk:ui-manager-insert-action-group ui actions-merge 2))

    (gtk:ui-manager-add-ui-from-string ui
                                       "<ui>
  <popup accelerators='true'>
    <menuitem action='copy-name'/>
    <menuitem action='copy-password'/>
    <separator/>
    <menuitem action='add-entry-group'/>
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
    <menu action='convert'>
      <menuitem action='convert-to-entry-generic'/>
      <menuitem action='convert-to-entry-creditcard'/>
      <menuitem action='convert-to-entry-cryptokey'/>
      <menuitem action='convert-to-entry-database'/>
      <menuitem action='convert-to-entry-door'/>
      <menuitem action='convert-to-entry-email'/>
      <menuitem action='convert-to-entry-ftp'/>
      <menuitem action='convert-to-entry-phone'/>
      <menuitem action='convert-to-entry-shell'/>
      <menuitem action='convert-to-entry-website'/>
    </menu>
    <menuitem action='delete'/>
  </popup>
</ui>")

    (gtk:let-ui

     (gtk:gtk-window
      :var main-window
      :title "PassStorage"
      :window-position :center
      :default-width 600
      :default-height 450
      (gtk:v-box

       (:expr (create-menubar ui
                              (menubar ()
                                       (menu ("file-menu")
                                             (menuitem "new")
                                             (menuitem "open")
                                             (menuitem "save")
                                             (menuitem "save-as")
                                             (separator)
                                             (menuitem "merge-file")
                                             (separator)
                                             (menuitem "quit"))
                                       (menu ("edit-menu")
                                             (menuitem "find")
                                             (separator)
                                             (menuitem "copy-name")
                                             (menuitem "copy-password")
                                             (separator)
                                             (menuitem "change-password")
                                             (separator)
                                             (menuitem "uncheck-all")
                                             (menuitem "merge-mode")
                                             (separator)
                                             (menuitem "preferences"))
                                       (menu ("entry-menu")
                                             (menuitem "add-entry-group")
                                             (separator)
                                             (menuitem "add-entry-generic")
                                             (menuitem "add-entry-creditcard")
                                             (menuitem "add-entry-cryptokey")
                                             (menuitem "add-entry-database")
                                             (menuitem "add-entry-door")
                                             (menuitem "add-entry-email")
                                             (menuitem "add-entry-ftp")
                                             (menuitem "add-entry-phone")
                                             (menuitem "add-entry-shell")
                                             (menuitem "add-entry-website")
                                             (separator)
                                             (menuitem "edit")
                                             (menu ("convert")
                                                   (menuitem "convert-to-entry-generic")
                                                   (menuitem "convert-to-entry-creditcard")
                                                   (menuitem "convert-to-entry-cryptokey")
                                                   (menuitem "convert-to-entry-database")
                                                   (menuitem "convert-to-entry-door")
                                                   (menuitem "convert-to-entry-email")
                                                   (menuitem "convert-to-entry-ftp")
                                                   (menuitem "convert-to-entry-phone")
                                                   (menuitem "convert-to-entry-shell")
                                                   (menuitem "convert-to-entry-website"))
                                             (menuitem "delete")
                                             (menuitem "merge"))
                                       (menu ("help-menu")
                                             (menuitem "about")))))
       :expand nil
       :position 0

       (gtk:toolbar
        (:expr (make-instance 'gtk:menu-tool-button
                              :stock-id "gtk-add"
                              :label "Add entry"
                              :related-action (gtk:action-group-action (app-actions-edit app) "add-entry-generic")
                              :menu (make-menu (app-actions-edit app)
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
        (:expr (gtk:action-create-tool-item (gtk:action-group-action (app-actions-edit app) "edit")))
        (:expr (gtk:action-create-tool-item (gtk:action-group-action (app-actions-edit app) "delete")))
        (:expr (gtk:action-create-tool-item (gtk:action-group-action (app-actions-merge app) "merge")))
        (gtk:separator-tool-item)
        (:expr (gtk:action-create-tool-item (gtk:action-group-action (app-actions-common app) "merge-mode")))
        (gtk:separator-tool-item :draw nil) :expand t
        (gtk:tool-item
         (gtk:entry
          :var search-entry
          :primary-icon-stock "gtk-find"
          :secondary-icon-stock "gtk-clear")))
       :expand nil
       :position 1

       (gtk:h-paned
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
          :search-column 1
          (gtk:tree-view-column
           :var col
           :sizing :autosize
           (gtk:cell-renderer-toggle
            :var check-renderer
            :visible nil)
           :expand nil
           (gtk:cell-renderer-pixbuf
            :stock-size 1)
           :expand nil
           :attribute ("stock-id" 2)
           (gtk:cell-renderer-text)
           :expand t
           :attribute ("text" 1))))
        (gtk:v-box
         :width-request 40
         (gtk:image
          :yalign 1.0
          :var current-icon)
         (gtk:label
          :use-markup t
          :var current-title)
         :expand nil
         (gtk:label
          :ypad 10
          :var current-description)
         :expand nil
         (gtk:label
          :use-markup t
          :yalign 0.0
          :var current-view))
        :resize nil)
       :position 2

       (gtk:statusbar
        :var statusbar
        :has-resize-grip t)
       :expand nil
       :position 3))

     (setf (app-search-entry app) search-entry)

     (set-filter-function app)

     (gobject:connect-signal (gtk:action-group-action (app-actions-common app) "find") "activate"
                             (lambda-u
                              (gtk:widget-grab-focus search-entry)))

     (gobject:connect-signal search-entry "changed"
                             (lambda-u
                              (if (string= "" (gtk:entry-text search-entry))
                                  (progn
                                    (set-status app "View filter was reset.")
                                    (setf (gtk:tree-view-model (app-view app)) (app-data app))
                                    (setf (gtk:tree-view-reorderable (app-view app)) t)
                                    (gtk:tree-view-collapse-all (app-view app)))
                                  (progn
                                    (set-status app "View is filtered.")
                                    (setf (gtk:tree-view-model (app-view app)) (app-filter app))
                                    (setf (gtk:tree-view-reorderable (app-view app)) nil)
                                    (gtk:tree-model-filter-refilter (app-filter app))
                                    (gtk:tree-view-expand-all (app-view app))))
                              (listview-cursor-changed app)))

     (gobject:connect-signal search-entry "icon-release"
                             (lambda (entry pos event)
                               (declare (ignore event))
                               (when (eq pos :secondary)
                                 (setf (gtk:entry-text entry) "")

                                 (set-status app "View filter was reset.")
                                 (setf (gtk:tree-view-model (app-view app)) (app-data app))
                                 (setf (gtk:tree-view-reorderable (app-view app)) t)
                                 (gtk:tree-view-collapse-all (app-view app))

                                 (listview-cursor-changed app))))

     (gobject:connect-signal check-renderer "toggled"
                             (lambda (renderer path)
                               (declare (ignore renderer))
                               (let* ((current-model (gtk:tree-view-model (app-view app)))
                                      (iter (gtk:tree-model-iter-from-string current-model path)))
                                 (when (and (eq current-model (app-filter app)) iter)
                                   (setf iter (gtk:tree-model-filter-convert-iter-to-child-iter (app-filter app) iter)))

                                 (when iter
                                   (setf (gtk:tree-store-value (app-data app) iter 3)
                                         (not (gtk:tree-store-value (app-data app) iter 3)))))))

     (gtk:tree-view-column-set-cell-data-function
      col check-renderer
      (lambda (col check-renderer model iter)
        (declare (ignore col model))
        (if (gtk:toggle-action-active (gtk:action-group-action (app-actions-common app) "merge-mode"))
            ;; then
            (let ((current-model (gtk:tree-view-model (app-view app))))
              (setf (gtk:cell-renderer-visible check-renderer)
                    (not (is-group (gtk:tree-store-value current-model iter 0))))
              (setf (gtk:cell-renderer-toggle-active check-renderer)
                    (gtk:tree-store-value current-model iter 3)))
            ;; else
            (setf (gtk:cell-renderer-visible check-renderer) nil))))

     (setf (gtk:gtk-window-icon main-window)
           (gtk:widget-render-icon main-window "ps-pass-storage" :dialog ""))

     (setf (app-main-window app) main-window)
     (setf (app-view app) view)
     (setf (app-column app) col)
     (setf (app-check-renderer app) check-renderer)
     (setf (app-current-icon app) current-icon)
     (setf (app-current-title app) current-title)
     (setf (app-current-description app) current-description)
     (setf (app-current-view app) current-view)
     (setf (app-statusbar app) statusbar)

     (gtk:widget-grab-focus search-entry))

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
                             (multiple-value-bind (iter path)
                                 (get-selected-iter app)

                               (when iter

                                 (let* ((data (app-data app))
                                        (view (app-view app))
                                        (entry (gtk:tree-store-value data iter 0)))

                                   (if (is-group entry)
                                       (if (gtk:tree-view-row-expanded-p view path)
                                           (gtk:tree-view-collapse-row view path)
                                           (gtk:tree-view-expand-row view path))
                                       (cb-edit-entry app)))))))

    (gobject:connect-signal (app-view app) "button-press-event"
                            (lambda (view event)
                              (when (= 3 (gdk:event-button-button event))
                                (let ((path (gtk:tree-view-get-path-at-pos view
                                                                           (round (gdk:event-button-x event))
                                                                           (round (gdk:event-button-y event)))))
                                  (gtk:widget-grab-focus view)
                                  (when path
                                    (gtk:tree-view-set-cursor view path))
                                  (gtk:menu-popup (gtk:ui-manager-widget ui "/popup")
                                                  :button (gdk:event-button-button event)
                                                  :activate-time (gdk:event-button-time event)))
                                t)))

    (gobject:connect-signal (app-view app) "popup-menu"
                            (lambda (view)
                              (gtk:widget-grab-focus view)
                              (gtk:menu-popup (gtk:ui-manager-widget ui "/popup")
                                              :activate-time (gdk:event-get-time nil))
                              t))

    #+win32
    (progn
      (setf (gtk:about-dialog-global-url-hook)
            (lambda (dialog uri)
              (declare (ignore dialog))
              (win32-open-uri uri)))

      (gobject:connect-signal (app-current-view app) "activate-link"
                              (lambda (label uri)
                                (declare (ignore label))
                                (win32-open-uri uri))))

    (gtk:gtk-window-add-accel-group (app-main-window app) (gtk:ui-manager-accel-group ui))
    (gtk:widget-show (app-main-window app))
    (set-mode app)

    (let ((default-file (or
                         (first (cli-options))
                         (config-default-file *config*))))
      (when (and default-file (probe-file default-file))
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

