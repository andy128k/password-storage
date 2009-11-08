(in-package :pass-storage)

(defun show-modal (dlg)
  (prog2
      (gtk:widget-show dlg :all t)
      (eql (gtk:dialog-run dlg) :ok)
      (gtk:widget-hide dlg)))

(defun ask (message)
  (eql (gtk:show-message message :buttons :yes-no :message-type :warning)
       :yes))

(defun ask-string (parent-wnd title icon caption)
  (let ((dialog (make-instance 'gtk:dialog)))
    (setf (gtk:gtk-window-title dialog) title)
    (setf (gtk:gtk-window-icon-name dialog) icon)
    (setf (gtk:gtk-window-modal dialog) t)
    (setf (gtk:gtk-window-transient-for dialog) parent-wnd)
    (setf (gtk:gtk-window-window-position dialog) :center-on-parent)
    (setf (gtk:container-border-width dialog) 8)

    (gtk:dialog-add-button dialog "gtk-cancel" :cancel)
    (gtk:dialog-add-button dialog "gtk-ok" :ok)
    (setf (gtk:dialog-default-response dialog) :ok)

    (let ((hbox (make-instance 'gtk:h-box)))
      (setf (gtk:container-border-width hbox) 8)
      (setf (gtk:box-spacing hbox) 8)

      (gtk:container-add 
       (gtk:dialog-content-area dialog)
       hbox)

      (let ((label (make-instance 'gtk:label))
	    (entry (make-instance 'gtk:entry)))
	
	(setf (gtk:label-label label) caption)
	(gtk:box-pack-start hbox label :expand nil)
	(gtk:box-pack-start hbox entry :expand t)

	(when (show-modal dialog)
	  (gtk:entry-text entry))))))

