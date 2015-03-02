(defun lookup-gear (item-name field-name table-name &optional from-end)
  (let* ((table (cdr (assoc table-name fccg::+gear+)))
	 (column-number
	  (if (stringp field-name)
	      (position field-name (first table) :test #'equalp)
	      (position-if field-name (first table))))
	 (row (find item-name (rest table) :key #'car :from-end from-end :test #'equalp)))
    (and row column-number
	 (elt row column-number))))
