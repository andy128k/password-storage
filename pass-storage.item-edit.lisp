(in-package :pass-storage)

(defun make-item-window (parent-window title)
  (gtk:let-ui
   (gtk:table
    :var table
    :border-width 5
    :n-rows 6
    :n-columns 2
    :column-spacing 8
    :row-spacing 8
    
      ;; name
      (gtk:label :label "Name" :xalign 0 :yalign 0) :left 0 :right 1 :top 0 :bottom 1 :x-options :fill :y-options :fill
      (gtk:entry :var entry-name :can-focus t :activates-default t) :left 1 :right 2 :top 0 :bottom 1 :y-options :fill

      ;; login
      (gtk:label :label "Login" :xalign 0 :yalign 0) :left 0 :right 1 :top 1 :bottom 2 :x-options :fill :y-options :fill
      (gtk:entry :var entry-login :can-focus t :activates-default t) :left 1 :right 2 :top 1 :bottom 2 :y-options :fill

      ;; password
      (gtk:label :label "Password" :xalign 0 :yalign 0) :left 0 :right 1 :top 2 :bottom 3 :x-options :fill :y-options :fill
      (gtk:entry :var entry-password :can-focus t :activates-default t) :left 1 :right 2 :top 2 :bottom 3 :y-options :fill

      ;; url
      (gtk:label :label "URL" :xalign 0 :yalign 0) :left 0 :right 1 :top 3 :bottom 4 :x-options :fill :y-options :fill
      (gtk:entry :var entry-url :can-focus t :activates-default t) :left 1 :right 2 :top 3 :bottom 4 :y-options :fill

      ;; email
      (gtk:label :label "E-mail" :xalign 0 :yalign 0) :left 0 :right 1 :top 4 :bottom 5 :x-options :fill :y-options :fill
      (gtk:entry :var entry-email :can-focus t :activates-default t) :left 1 :right 2 :top 4 :bottom 5 :y-options :fill

      ;; comment
      (gtk:label :label "Comment" :xalign 0 :yalign 0) :left 0 :right 1 :top 5 :bottom 6 :x-options :fill :y-options :fill
      (gtk:scrolled-window
       :can-focus t
       :hscrollbar-policy :automatic
       :vscrollbar-policy :automatic
       :shadow-type :in
       (gtk:text-view :var text-view-comment :can-focus t :accepts-tab nil))
      :left 1 :right 2 :top 5 :bottom 6)
     
   (let ((dlg (make-std-dialog parent-window title "gtk-file" table)))

     (gobject:connect-signal entry-name "changed"
			     (lambda (entry)
			       (gtk:dialog-set-response-sensitive 
				dlg
				:ok 
				(/= 0 (length (gtk:entry-text entry))))))

     (values dlg
	     entry-name
	     entry-login
	     entry-password
	     entry-url
	     entry-email
	     text-view-comment))))

(defun item-add (parent-window)
  (multiple-value-bind
	(dlg
	 entry-name
	 entry-login
	 entry-password
	 entry-url
	 entry-email
	 text-view-comment)
      (make-item-window parent-window "Add entry")

    (when (std-dialog-run dlg)
      (make-instance 'item
		     :name (gtk:entry-text entry-name)
		     :login (gtk:entry-text entry-login)
		     :password (gtk:entry-text entry-password)
		     :url (gtk:entry-text entry-url)
		     :email (gtk:entry-text entry-email)
		     :comment (gtk:text-buffer-text (gtk:text-view-buffer text-view-comment))))))

(defun item-edit (parent-window item)
  (multiple-value-bind
	(dlg
	 entry-name
	 entry-login
	 entry-password
	 entry-url
	 entry-email
	 text-view-comment)
      (make-item-window parent-window "Edit entry")
    
    (setf (gtk:entry-text entry-name) (item-name item))
    (setf (gtk:entry-text entry-login) (item-login item))
    (setf (gtk:entry-text entry-password) (item-password item))
    (setf (gtk:entry-text entry-url) (item-url item))
    (setf (gtk:entry-text entry-email) (item-email item))
    (setf (gtk:text-buffer-text (gtk:text-view-buffer text-view-comment)) (item-comment item))

    (when (std-dialog-run dlg)
      (setf (item-name item) (gtk:entry-text entry-name))
      (setf (item-login item) (gtk:entry-text entry-login))
      (setf (item-password item) (gtk:entry-text entry-password))
      (setf (item-url item) (gtk:entry-text entry-url))
      (setf (item-email item) (gtk:entry-text entry-email))
      (setf (item-comment item) (gtk:text-buffer-text (gtk:text-view-buffer text-view-comment)))
      t)))

