;;; Resources

(defun resource-split (resource)
  "Return array with resource parts, empty only at begin or end."
  (let* ((start-slash (and (> (length resource) 0)
                           (equal "/" (substring resource 0 1))))
         (end-slash (and (> (length resource) 0)
                         (equal "/" (substring resource -1 nil))))
         (parts (split-string resource "/" t)))
    (when start-slash
      (setq parts (cons "" parts)))
    (when (or end-slash (= 1 (length parts)))
      (setcdr (last parts) (cons "" nil)))
    parts))

(defun resource-join (parts)
  (mapconcat 'identity parts "/"))

(defun resource-normalize (resource)
  "Normalize a path resource, keeping only relative components at the beginning."
  (let* ((result nil))
    (mapcar (lambda (part)
              (cond ((equal "." part)
                     (if (not result)
                         (setq result (cons part result))))
                    ((equal ".." part)
                     (if (or (not result)
                             (equal ".." (car result))
                             (equal "" (car result)))
                         (setq result (cons part result))
                       (if (equal "." (car result))
                           (setq result (cons part (cdr result)))
                         (setq result (cdr result)))))
                    (t (setq result (cons part result)))))
            (resource-split resource))
    (resource-join (reverse result))))

(defun resource-relativize (resource reference)
  (let ((res-parts (resource-split resource))
        (ref-parts (resource-split (resource-directory reference))))
    (while (equal (car res-parts) (car ref-parts))
      (setq res-parts (cdr res-parts))
      (setq ref-parts (cdr ref-parts)))
    (normalize (concat (resource-join (if (> (length ref-parts) 1)
                                      (make-list (- (length ref-parts) 1)
                                                 "../")
                                    (list "./")))
                       (resource-join res-parts)))))

(defun resource-absolute (resource reference)
  (if (resource-relative-p resource)
      (resource-normalize (concat (resource-directory reference)
                                  resource))
    resource))

(defun resource-relative-p (string)
  (string-match "^\\.\\.?/" string))

(defun resource-directory (resource)
  (let ((parts (resource-split resource)))
    (setcar (last parts) "")
    (resource-join parts)))

;;; Files

(defun file-name-absolute filename directory
  (if (string-match "^\\." path)
      (expand-file-name (concat (file-name-directory current-path) "/" path))))

;;; List

(defun cadr (l)
  (car (cdr l)))

;;; Hash table

(defun hash-table-keys (table)
  "Return keys of the hash table in a list"
  (let ((keys))
    (maphash (lambda (kk vv)
               (setq keys (cons kk keys)))
             table)
    keys))

(defun hash-table-values (table &optional keys)
  "Return values of the hash table in a list, if keys are given only for those keys"
  (if keys
      (mapcar (lambda (key) (gethash key table))
              keys)
    (let ((values))
      (maphash (lambda (kk vv)
                 (setq values (cons vv values)))
               table)
      values)))

(defun hash-table-find (value table)
  "Return the key belonging to a value or nil."
  (catch 'found
    (maphash (lambda (kk vv)
               (if (equal vv value)
                   (throw 'found kk)))
             table)))

;;; String

(defun unique-string (string strings)
  "Make string unique so that it does not exists in strings by appending a counter."
  (let ((counter 1)
        (unique-string string))
    (while (member unique-string strings)
      (setq counter (1+ counter))
      (setq unique-string (format (concat string "%d") counter)))
    unique-string))

(defun camelize (string)
  (let ((parts (split-string string "-")))
    (concat (car parts)
            (mapconcat 'identity
                       (mapcar (lambda (part) (upcase-initials part))
                               (cdr parts)) ""))))

(defun string-sans-prefix (prefix string)
  (let ((regexp (concat "^" (regexp-quote prefix) "\\(.*\\)$")))
    (if (string-match regexp string)
        (match-string 1 string))))

(provide 'amd-util)
