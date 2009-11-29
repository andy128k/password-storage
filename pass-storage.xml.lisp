(in-package :pass-storage)

(defun is-tag (xml-node)
  (consp xml-node))

(defun tag-name (xml-node)
  (if (consp (car xml-node))
      (caar xml-node)
      (car xml-node)))

(defun tag-attributes (xml-node)
  (if (consp (car xml-node))
      (cdar xml-node)
      nil))

(defun tag-children (xml-node)
  (cdr xml-node))

(defun tag-get-attr (xml-node attr)
  (iter (for (k v) on (tag-attributes xml-node) by #'cddr)
	(finding v such-that (eq k attr))))

(defun entry-node-get-value (xml-node tag-name &optional (id nil))
  (or
   (iter (for ch in (tag-children xml-node))
	 (finding (car (tag-children ch))
		  such-that (and (is-tag ch)
				 (eq (tag-name ch) tag-name)
				 (or (not id)
				     (string= (tag-get-attr ch :|id|) id)))))
   ""))

