(require 'cl)
(require 'amd-util)

;;; Readin and writing the header

(setq amd--header-regexp
      (let* ((g< "\\(?:")
            (>g "\\)")
            (string (concat g< "\"[^\"]*\"\\|'[^']*'" >g))
            (identifier "[_$[:alnum:]]+")
            (ws* "[\n[:space:]]*")
            (strings? (concat g< string
                              g< ws* "," ws* string >g "*" >g "?"))
            (identifiers? (concat g< identifier
                                  g< ws* "," ws* identifier >g "*" >g "?")))
        (concat "^define" ws* "(" ws* "\\(\\[" ws* strings? ws* "\\]\\)" ws* ","
                ws* "function" ws* "\\((" ws* identifiers? ws* ")\\)" ws* "{")))

(defun amd--find-header ()
  "Find the AMD header and parse it"
  (goto-char (point-min))
  (if (search-forward-regexp amd--header-regexp nil t)
      (let ((raw-ids (match-string 1))
            (raw-vars (match-string 2)) 
            (id-region (list (match-beginning 1) (match-end 1)))
            (var-region (list (match-beginning 2) (match-end 2))))
        (let ((ids (amd--parse-id-list raw-ids))
              (vars (amd--parse-var-list raw-vars)))
          (amd--make-header ids vars id-region var-region)))))

(defun amd--parse-id-list (string)
  (mapcar (lambda (s) (amd--strip-parens s))
          (amd--split-list string)))

(defun amd--parse-var-list (string)
  (amd--split-list string))

(defun amd--split-list (string)
  (mapcar (lambda (s) (amd--trim s))
          (split-string (amd--strip-parens string) "," t)))

(defun amd--strip-parens (string)
  (amd--trim (substring (amd--trim string) 1 -1)))

(defun amd--trim (string)
  (let ((ws* "[\n[:space:]]*"))
    (replace-regexp-in-string ws* ""
                              (replace-regexp-in-string ws* "" string))))

(defun amd--write-header (header)
  (let ((id-region (amd--header-id-region header))
        (var-region (amd--header-var-region header)))
    (let ((new-id-start (amd--region-start id-region))
          (new-var-start (- (amd--region-start var-region)
                            (amd--region-length id-region))))
      (amd--delete-region var-region)
      (amd--delete-region id-region)
      (goto-char new-var-start)
      (insert (concat "(" (mapconcat 'identity
                                       (amd--header-vars header)
                                       ", ") ")"))
      (goto-char new-id-start)
      (insert (concat "[\n" (mapconcat (lambda (id) (concat "    \"" id "\""))
                                       (amd--header-ids header)
                                       ",\n") "\n]")))))

(defun amd--write-empty-header ()
  (goto-char (point-min))
  (insert "define([],function(){\n")
  (goto-char (point-max))
  (insert "\n});"))

(defun amd--find-or-create-header ()
  (let ((header (amd--find-header)))
    (if header
        header
      (progn (amd--write-empty-header)
             (amd--find-header)))))

;;; Header data structure

(defun amd--make-header (ids vars id-region var-region)
  (if (equal (length ids) (length vars))
      (let ((theTable (make-hash-table :test 'equal)))
        (progn (mapcar* (lambda (id var) (puthash id var theTable))
                        ids vars)
               (list theTable id-region var-region)))
    (identity nil)))

(defun amd--header-table (header)
  (car header))

(defun amd--header-id-region (header)
  (car (cdr header)))

(defun amd--header-var-region (header)
  (car (cdr (cdr header))))

(defun amd--header-ids (header)
  "Return a sorted list of all module ids."
  (sort (hash-table-keys (amd--header-table header))
        'string<))

(defun amd--header-vars (header)
  "Return a list of all variable names in the same order as amd--header-ids."
  (hash-table-values (amd--header-table header)
                     (amd--header-ids header)))

(defun amd--header-add (dep var header)
  "Add a module ID and return the new or already existing variable name."
  (let* ((table (amd--header-table header))
         (curvar (gethash dep table)))
    (if curvar
        curvar
      (let ((newvar (amd--header-unique-var (amd--header-safe-var var) header)))
        (puthash dep newvar table)
        newvar))))

(defun amd--header-safe-var (var)
  (replace-regexp-in-string "[^[:alnum:]_$]+" "" var))

(defun amd--header-unique-var (var header)
  (unique-string var (amd--header-vars header)))

(defun amd--header-var-by-id (id header)
  "Return the variable name for a module id or nil."
  (gethash id (amd--header-table header)))

(defun amd--header-id-by-var (var header)
  "Return the variable name for a module id or nil."
  (hash-table-find var (amd--header-table header)))

(defun amd--header-del-id (id header)
  (remhash id (amd--header-table header)))

(defun amd--header-del-var (var header)
  (remhash (amd--header-id-by-var var header)
           (amd--header-table header)))

;;; Region data structure

(defun amd--make-region (start end)
  (list start end))

(defun amd--region-start (region)
  (car region))

(defun amd--region-end (region)
  (cadr region))

(defun amd--region-length (region)
  (- (amd--region-end region) (amd--region-start region)))

(defun amd--delete-region (region)
  (delete-region (amd--region-start region) (amd--region-end region)))
(provide 'amd-header)
