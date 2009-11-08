(in-package :pass-storage)

(defun make-item-window (parent-window)
  (gtk:let-ui
   (gtk:gtk-window
    :var dlg
    :border-width 8
    :modal t
    :resizable t
    :window-position :center-on-parent
    :icon-name "gtk-file"
    :type-hint :dialog
    :skip-taskbar_hint t
    :skip-pager-hint t
    :gravity :center
    :transient-for parent-window
    
    (gtk:v-box
     :spacing 8
     
     (gtk:table
      :border-width 0
      :n-rows 6
      :n-columns 2
      :column-spacing 8
      :row-spacing 8
      
      ;; name
      (gtk:label :label "Name" :xalign 0 :yalign 0) :left 0 :right 1 :top 0 :bottom 1 :x-options :fill :y-options :fill
      (gtk:entry :var entry-name :can-focus t) :left 1 :right 2 :top 0 :bottom 1 :y-options :fill

      ;; login
      (gtk:label :label "Login" :xalign 0 :yalign 0) :left 0 :right 1 :top 1 :bottom 2 :x-options :fill :y-options :fill
      (gtk:entry :var entry-login :can-focus t) :left 1 :right 2 :top 1 :bottom 2 :y-options :fill

      ;; password
      (gtk:label :label "Password" :xalign 0 :yalign 0) :left 0 :right 1 :top 2 :bottom 3 :x-options :fill :y-options :fill
      (gtk:entry :var entry-password :can-focus t) :left 1 :right 2 :top 2 :bottom 3 :y-options :fill

      ;; url
      (gtk:label :label "URL" :xalign 0 :yalign 0) :left 0 :right 1 :top 3 :bottom 4 :x-options :fill :y-options :fill
      (gtk:entry :var entry-url :can-focus t) :left 1 :right 2 :top 3 :bottom 4 :y-options :fill

      ;; email
      (gtk:label :label "E-mail" :xalign 0 :yalign 0) :left 0 :right 1 :top 4 :bottom 5 :x-options :fill :y-options :fill
      (gtk:entry :var entry-email :can-focus t) :left 1 :right 2 :top 4 :bottom 5 :y-options :fill

      ;; comment
      (gtk:label :label "Comment" :xalign 0 :yalign 0) :left 0 :right 1 :top 5 :bottom 6 :x-options :fill :y-options :fill
      (gtk:scrolled-window
       :can-focus t
       :hscrollbar-policy :automatic
       :vscrollbar-policy :automatic
       :shadow-type :in
       (gtk:text-view :var text-view-comment :can-focus t :accepts-tab nil))
      :left 1 :right 2 :top 5 :bottom 6)
     :expand t
     
     (gtk:h-button-box
      :layout-style :end

      (gtk:button
       :var btn_cancel
       :label "gtk-cancel"
       :can-focus t
       :can-default t
       :receives-default nil
       :use-stock t)
      :expand nil :fill nil :position 0

      (gtk:button
       :var btn_ok
       :label "gtk-ok"
       :sensitive nil
       :can-focus t
       :can-default t
       :receives-default nil
       :use-stock t)
      :expand nil :fill nil :position 1)
     :expand nil))

   (gobject:connect-signal entry-name "changed"
			   (lambda (entry)
			     (setf (gtk:widget-sensitive btn_ok)
				   (/= 0 (length (gtk:entry-text entry))))))

   (gobject:connect-signal dlg "destroy"
			   (lambda (u)
			     (declare (ignore u))
			     (gtk:gtk-main-quit)))
   
   (gobject:connect-signal btn_cancel "clicked"
			   (lambda (u)
			     (declare (ignore u))
			     (gtk:gtk-main-quit)))
   
   (gobject:connect-signal btn_ok "clicked"
			   (lambda (u)
			     (declare (ignore u))
			     (setf (gtk:gtk-object-user-data dlg) (cffi:make-pointer 1))
			     (gtk:gtk-main-quit)))

   (values dlg
	   entry-name
	   entry-login
	   entry-password
	   entry-url
	   entry-email
	   text-view-comment)))

(defun item-add (parent-window)
  (multiple-value-bind
	(dlg
	 entry-name
	 entry-login
	 entry-password
	 entry-url
	 entry-email
	 text-view-comment)
      (make-item-window parent-window)
  
    (setf (gtk:gtk-window-title dlg) "Add entry")

    (when (window-run dlg)
      (make-item
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
      (make-item-window parent-window)

    (setf (gtk:gtk-window-title dlg) "Edit entry")
    
    (setf (gtk:entry-text entry-name) (item-name item))
    (setf (gtk:entry-text entry-login) (item-login item))
    (setf (gtk:entry-text entry-password) (item-password item))
    (setf (gtk:entry-text entry-url) (item-url item))
    (setf (gtk:entry-text entry-email) (item-email item))
    (setf (gtk:text-buffer-text (gtk:text-view-buffer text-view-comment)) (item-comment item))

    (when (window-run dlg)
      (setf (item-name item) (gtk:entry-text entry-name))
      (setf (item-login item) (gtk:entry-text entry-login))
      (setf (item-password item) (gtk:entry-text entry-password))
      (setf (item-url item) (gtk:entry-text entry-url))
      (setf (item-email item) (gtk:entry-text entry-email))
      (setf (item-comment item) (gtk:text-buffer-text (gtk:text-view-buffer text-view-comment)))
      t)))

