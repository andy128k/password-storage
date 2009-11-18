(in-package :pass-storage)

(defun edit-group-dlg (parent-window title &key (value ""))
  (ask-string parent-window title "gtk-directory" "Group name"
	      :start-value value
	      :validate (lambda (text)
			  (when (= 0 (length text))
			    "Enter group name"))))

(defun group-add (parent-window)
  (let ((name (edit-group-dlg parent-window "Add group")))
    (when name
      (make-instance 'group :name name))))
    
(defun group-edit (parent-window group)
  (let ((name (edit-group-dlg parent-window "Edit group" :value (group-name group))))
    (when name
      (setf (group-name group) name)
      t)))

