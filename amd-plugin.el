; These three functions are the main interface
; For every plugin type, a specific version has to
; be implemented. The main commands will call these
; for any conversions.

(defun amd-plugin-file-to-id (file)
  "For the given file, return an AMD id")

(defun amd-plugin-id-to-var (id)
  "For the given id, return a variable name")

(defun amd-plugin-id-to-file (id)
  "For the given id, return a file")

; The standard js functions

(setq amd--dependency-regexp
      (let ((identifier "[[:alnum:]-_]+"))
        (concat "\\(?:\\(" identifier "\\)!\\)?"
                "\\(" identifier "\\(?:/" identifier "\\)*\\)"
                "\\(?:\\.\\(" identifier "\\)\\)?"

(defun amd--var-from-id (id)
  (let ((parts (split-string (car (last (split-string id "/"))) "-")))
    (concat (car parts)
            (mapconcat 'identity
                       (mapcar (lambda (part) (upcase-initials part))
                               (cdr parts)) ""))))

(provide 'amd-plugin)
