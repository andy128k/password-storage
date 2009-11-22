(in-package :pass-storage)

(defun make-item-window (parent-window title)
  (gtk:let-ui
   (gtk:table
    :var table
    :border-width 5
    :n-rows 5
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

      ;; comment
      (gtk:label :label "Comment" :xalign 0 :yalign 0) :left 0 :right 1 :top 4 :bottom 5 :x-options :fill :y-options :fill
      (gtk:scrolled-window
       :can-focus t
       :hscrollbar-policy :automatic
       :vscrollbar-policy :automatic
       :shadow-type :in
       (gtk:text-view :var text-view-comment :can-focus t :accepts-tab nil))
      :left 1 :right 2 :top 4 :bottom 5)
     
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
	     text-view-comment))))

(defmethod edit-entry ((entry entry-generic) parent-window title)
  (multiple-value-bind
	(dlg
	 entry-name
	 entry-login
	 entry-password
	 entry-url
	 text-view-comment)
      (make-item-window parent-window title)
    
    (setf (gtk:entry-text entry-name) (entry-name entry))
    (setf (gtk:entry-text entry-login) (generic-username entry))
    (setf (gtk:entry-text entry-password) (generic-password entry))
    (setf (gtk:entry-text entry-url) (generic-hostname entry))
    (setf (gtk:text-buffer-text (gtk:text-view-buffer text-view-comment)) (entry-description entry))

    (when (std-dialog-run dlg)
      (setf (entry-name entry) (gtk:entry-text entry-name))
      (setf (generic-username entry) (gtk:entry-text entry-login))
      (setf (generic-password entry) (gtk:entry-text entry-password))
      (setf (generic-hostname entry) (gtk:entry-text entry-url))
      (setf (entry-description entry) (gtk:text-buffer-text (gtk:text-view-buffer text-view-comment)))
      t)))

