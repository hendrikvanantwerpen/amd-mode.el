;;; Path

(defun normalize (string)
  "Normalize a path string, keeping only relative components at the beginning."
  (let* ((result nil))
    (mapcar (lambda (part)
              (cond ((equal "." part)
                     (if (not result)
                         (setq result (cons part result))))
                    ((equal ".." part)
                     (if (or (not result) (equal ".." (car result)))
                         (setq result (cons part result))
                       (if (equal "." (car result))
                           (setq result (cons part (cdr result)))
                         (setq result (cdr result)))))
                    (t (setq result (cons part result)))))
            (split-string string "/" t))
    (mapconcat 'identity (reverse result) "/")))

(defun relative-p (string)
  (string-match "^\\.\\.?/" string))

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

(provide 'amd-util)
