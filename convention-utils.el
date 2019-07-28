(defun convention-util-recursive-assoc-cdr (key-tuple alist)
  "Returns an element from a nested ALIST, whose hierarchical address is
   described with a key-tuple, which is a tuple of alist keys"
  (let ((key (car key-tuple)))
    (if (equal (length key-tuple) 1)
        (cdr (assoc key alist))
      (convention-util-recursive-assoc-cdr
       (cdr key-tuple)
       (cdr (assoc key alist))))))


(defun convention-is-sql (image-or-container-name)
  "Returns a boolean indicating whether the LANG corresponds
   to a database engine"
  (let ((lang (convention-guess-lang-from-image-or-container-name image-or-container-name)))
    (member lang '("postgres"
                   "mysql"
                   "mssql"
                   "mariadb"))))




(provide 'convention-utils)
