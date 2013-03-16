(require 'cl)

(defvar amd--header-regexp "^define\\s-*(\\s-*\\(\\[[^]]*\\]\\)\\s-*,\\s-*function\\s-*\\(([^)]*)\\)\\s-*{")

(defun cadr (l)
  (car (cdr l)))

(defun amd--strip-parens (string)
  (substring string 1 -1))

(defun amd--trim (string)
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun amd--parse-var-list (string)
  (mapcar (lambda (s) (amd--trim s))
          (split-string (amd--trim (amd--strip-parens (amd--trim string))) "," t)))

(defun amd--parse-id-list (string)
  (mapcar (lambda (s) (amd--trim (amd--strip-parens (amd--trim s))))
          (split-string (amd--trim (amd--strip-parens (amd--trim string))) "," t)))

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

(defun amd--header-ids (header)
  (let ((table (amd--header-table header))
        (result ()))
    (maphash (lambda (kk vv)
               (setq result (cons kk result)))
             table)
    (sort result 'string<)))

(defun amd--header-vars (header)
  (mapcar (lambda (id) (gethash id (amd--header-table header)))
          (amd--header-ids header)))

(defun amd--header-add (id var header)
  (puthash id var (amd--header-table header)))

(defun amd--header-var-by-id (id header)
  (gethash id (amd--header-table header)))

(defun amd--header-id-by-var (var header)
  (let (id)
    (maphash (lambda (kk vv)
               (if (equals vv var)
                 (setq id kk)))
             (amd--header-table header))
    (identity id)))

(defun amd--header-del-id (id header)
  (remhash id (amd--header-table header)))

(defun amd--header-del-var (var header)
  (remhash (amd--header-id-by-var var header)
           (amd--header-table header)))

(defun amd--write-header (header)
  (print header)
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

(defun amd--write-empty-header ()
  (indent-region (point-min) (point-max) nil)
  (goto-char (point-min))
  (insert "define([],function(){\n")
  (goto-char (point-max))
  (insert "\n});"))

(defun amd--find-or-create-header ()
  (let ((header (amd--find-header)))
    (if header
      (identity header)
      (progn (amd--write-empty-header)
             (amd--find-header)))))

(provide 'amd-manip)
