(in-package :pass-storage)

(defun ask (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
                            :text message
                            :buttons :yes-no
                            :title "PassStorage"
                            :message-type :question
                            :window-position :center-on-parent
                            :transient-for parent-window
                            :use-markup nil)))
    (prog1
        (eql (gtk:dialog-run dlg) :yes)
      (gtk:widget-hide dlg :all t))))

(defun ask-save (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
                            :text message
                            :title "PassStorage"
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
      (gtk:widget-hide dlg :all t))))

(defun say-error (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
                            :text message
                            :buttons :ok
                            :title "PassStorage"
                            :message-type :error
                            :window-position :center-on-parent
                            :transient-for parent-window
                            :use-markup nil)))
    (gtk:dialog-run dlg)
    (gtk:widget-hide dlg :all t)))

(defun say-warning (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
                            :text message
                            :buttons :ok
                            :title "PassStorage"
                            :message-type :warning
                            :window-position :center-on-parent
                            :transient-for parent-window
                            :use-markup nil)))
    (gtk:dialog-run dlg)
    (gtk:widget-hide dlg :all t)))

(defun say-info (parent-window message)
  (let ((dlg (make-instance 'gtk:message-dialog
                            :text message
                            :buttons :ok
                            :title "PassStorage"
                            :message-type :info
                            :window-position :center-on-parent
                            :transient-for parent-window
                            :use-markup nil)))
    (gtk:dialog-run dlg)
    (gtk:widget-hide dlg :all t)))

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
                                                            (declare (ignore event))
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
                                                    0 2 i (+ i 1) :x-options :fill :y-options :fill)
                                  widget))
                               (:choice
                                (let* ((box (make-instance 'gtk:v-box :spacing 4))
                                       (buttons
                                        (iter (for tt in title)
					      (for index from 0)
                                              (let ((button (make-instance 'gtk:radio-button
                                                                           :can-focus t
                                                                           :label tt
                                                                           :activates-default t
									   :user-data (cffi:make-pointer index))))
                                                (gtk:box-pack-start box button)
                                                (collecting button)))))
                                  (iter (for button in buttons)
                                        (setf (gtk:radio-button-group button) (remove button buttons)))
                                  (setf (gtk:toggle-button-active (first buttons)) t)

                                  (gtk:table-attach table
                                                    box
                                                    0 2 i (+ i 1) :x-options :fill :y-options :fill)
                                  (first buttons))))))

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
(defmethod widget-get-text ((widget gtk:radio-button))
  (let ((group (gtk:radio-button-group widget)))
    (iter (for button in group)
          (when (gtk:toggle-button-active button)
            (return-from widget-get-text
	      (cffi:pointer-address (gtk:gtk-object-user-data button)))))))

(defgeneric widget-changed (widget handler))
(defmethod widget-changed ((widget gtk:entry) handler)
  (gobject:connect-signal widget "changed" handler))
(defmethod widget-changed ((widget gtk:file-chooser-button) handler)
  (gobject:connect-signal widget "file-set" handler))

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
              (setf disable-ok (or (not slot)
                                   (= 0 (length (slot-value obj slot)))))
              (widget-changed widget
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
(defun get-first-child (model parent)
  (if parent
      (gtk:tree-model-iter-first-child model parent)
      (gtk:tree-model-iter-first model)))

(defmacro-driver (for iter in-tree-model model children-of parent-iter)
    (let ((m (gensym))
          (i (gensym))
          (kwd (if generate 'generate 'for)))
      `(progn
         (with ,m = ,model)
         (with ,i = nil)
         (,kwd ,iter next
	       (progn
		 (setf ,i
		       (if-first-time
			(get-first-child ,m ,parent-iter)
			(when (gtk:tree-model-iter-next ,m ,i)
			  i)))
		 (if ,i
		     ,i
		     (terminate)))))))

(defmacro-driver (for iter in-tree-model model parents-of node-iter)
    (let ((m (gensym))
          (i (gensym))
          (kwd (if generate 'generate 'for)))
      `(progn
         (with ,m = ,model)
         (with ,i = ,node-iter)
         (,kwd ,iter next
	       (progn
		 (setf ,i
		       (if-first-time
			,i
			(progn
			  (gtk:tree-model-iter-parent ,m ,i))))
		 (if ,i
		     ,i
		     (terminate)))))))

(defun cli-options ()
  "list of tokens passed in at the cli"
  #+:sbcl (rest sb-ext:*posix-argv*)
  #+:ccl (rest ccl:*command-line-argument-list*)
  #+:clisp (rest ext:*args*)
  #+:lispworks (rest system:*line-arguments-list*)
  #+:cmu (rest extensions:*command-line-words*))

(defun optional-attribute (name value)
  (if value
      (format nil " ~A='~A'" name value)
      ""))

(defun build-menubar (node)
  (destructuring-bind (&key name action) (second node)
    (values
     (format nil "<menubar~A~A>~{~A~}</menubar>"
	     (optional-attribute "name" name)
	     (optional-attribute "action" action)
	     (iter (for n in (cddr node))
		   (collect
		    (ecase (car n)
		      (menuitem (build-menuitem n))
		      (separator (build-separator n))
		      (menu (build-menu n))))))
     name
     action)))

(defun build-menuitem (node)
  (destructuring-bind (action &key name position always-show-image) (cdr node)
    (format nil "<menuitem action='~A'~A~A~A/>"
	    action
	    (optional-attribute "name" name)
	    (optional-attribute "position" position)
	    (optional-attribute "always-show-image" always-show-image))))

(defun build-separator (node)
  (destructuring-bind (&key action name expand) (cdr node)
    (format nil "<separator~A~A~A/>"
	    (optional-attribute "action" action)
	    (optional-attribute "name" name)
	    (optional-attribute "expand" expand))))

(defun build-menu (node)
  (destructuring-bind (action &key name position) (second node)
    (format nil "<menu action='~A'~A~A>~{~A~}</menu>" 
	    action
	    (optional-attribute "name" name)
	    (optional-attribute "position" position)
	    (iter (for n in (cddr node))
		  (collect
		   (ecase (car n)
		     (menuitem (build-menuitem n))
		     (separator (build-separator n))
		     (menu (build-menu n))))))))

(defmacro create-menubar (ui-manager menu)
  (multiple-value-bind (xml name)
      (build-menubar menu)
    (let ((path
	   (if name
	       (format nil "/~A" name)
	       "/menubar")))
      `(progn
	 (gtk:ui-manager-add-ui-from-string ,ui-manager
					    ,(format nil "<ui>~A</ui>" xml))
	 (gtk:ui-manager-widget ,ui-manager ,path)))))

#+win32
(progn

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library shell32-dll
        (:windows "shell32.dll")))

  (cffi:use-foreign-library shell32-dll)

  (cffi:defcfun ("ShellExecuteA" shell-execute) :pointer
    (hwnd :pointer)
    (operation :string)
    (file :string)
    (parameters :pointer)
    (directory :pointer)
    (show-cmd :int))

  (defun win32-open-uri (uri)
    (shell-execute
     (cffi:null-pointer)
     "open"
     uri
     (cffi:null-pointer)
     (cffi:null-pointer)
     5))) ;; SW_SHOW

(defun markup-escape-text (text)
  (with-output-to-string (str)
    (iter (for ch in-string text)
          (let ((code (char-code ch)))
            (case ch
              (#\& (format str "&amp;"))
              (#\< (format str "&lt;"))
              (#\> (format str "&gt;"))
              (#\' (format str "&apos;"))
              (#\" (format str "&quot;"))
              (t
               (if (or (<= 1 code 8)
                       (<= 11 code 12)
                       (<= 14 code 31)
                       (<= 127 code 132)
                       (<= 144 code 159))
                   (format str "&#x~X;" code)
                   (format str "~A" ch))))))))

(defun equal-together (data)
  (let ((e1 (car data)))
    (when (every (lambda (e) (equal e e1)) (cdr data))
      e1)))

