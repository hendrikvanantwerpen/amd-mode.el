(require 'amd-util)

; The interface

(setq amd-dep-norm-handlers (make-hash-table :test 'equal))
(setq amd-dep-to-var-handlers (make-hash-table :test 'equal))
(setq amd-dep-to-file-handlers (make-hash-table :test 'equal))
(setq amd-dep-from-file-handlers nil)

(setq amd-dep-module-id-re
      "^\\(?:[[:alnum:]-_\.]+/\\)*\\([[:alnum:]-_]+\\)$")

(setq amd-dep-re
      "^\\(?:\\(\\(?:[[:alnum:]-_\.]+/\\)*[[:alnum:]-_]+\\)!\\)?\\([^!]+\\)$")

(defun amd-dep-create (plugin resource)
  (let ((dep (cons plugin resource)))
    (let* ((plugin-handler (gethash nil amd-dep-norm-handlers))
           (resource-handler (gethash plugin amd-dep-norm-handlers))
           (norm-plugin (if (and plugin plugin-handler)
                            (funcall plugin-handler plugin)
                          plugin))
           (norm-resource (if resource-handler
                              (funcall resource-handler resource)
                            resource)))
      (if norm-resource
          (cons norm-plugin norm-resource)))))

(defun amd-deps-from-file (file)
  "For the given file, return possible AMD dependencies"
  (let ((deps nil))
    (mapcar (lambda (handler)
              (let ((dep (funcall handler file)))
                (if dep
                    (setq deps (cons dep deps)))))
            amd-dep-from-file-handlers)
    deps))

(defun amd-dep-to-var (dep)
  "For the given dependency, return a variable name"
  (let ((handler (gethash (amd-dep-plugin dep) amd-dep-to-var-handlers)))
    (if handler
        (funcall handler (amd-dep-resource dep)))))

(defun amd-dep-to-file (dep)
  "For the given dependency, return a file")

; Dependency

(defun amd-dep-parse (string)
  (if (string-match amd-dep-re string)
      (let* ((plugin (match-string 1 string))
             (resource (match-string 2 string)))
        (amd-dep-create plugin resource))))

(defun amd-dep-format (dep)
  (let ((plugin (amd-dep-plugin dep))
        (resource (amd-dep-resource dep)))
    (if (not plugin)
        resource
      (concat plugin "!" resource))))

(defun amd-dep-plugin (dep)
  (car dep))

(defun amd-dep-resource (dep)
  (cdr dep))

; The standard js functions

(defun amd-dep-module-norm (resource)
  (if (string-match amd-dep-module-id-re resource)
      (normalize resource)))

(defun amd-dep-module-from-file (file)
  (let ((fullfile (expand-file-name file)))
    (if (string-match "^.*\\.js$" file)
        (amd-dep-create nil (file-name-sans-extension file)))))

(defun amd-dep-module-to-var (resource)
  (if (string-match amd-dep-module-id-re resource)
      (let ((name (match-string 1 resource)))
        (camelize name))))

(puthash nil (lambda (resource) (amd-dep-module-norm resource)) amd-dep-norm-handlers)
(puthash nil (lambda (resource) (amd-dep-module-to-var resource)) amd-dep-to-var-handlers)
(add-to-list 'amd-dep-from-file-handlers (lambda (file) (amd-dep-module-from-file file)))

; text plugin

(defun amd-dep-text-plugin-norm (resource)
  (expand-file-name resource))

(defun amd-dep-text-plugin-from-file (file)
  (let ((fullfile (expand-file-name file)))
    (amd-dep-create "text" fullfile)))

(defun amd-dep-text-plugin-to-var (resource)
  (camelize (concat (file-name-sans-extension (file-name-nondirectory resource))
                    "-" (file-name-extension resource))))

(puthash "text" (lambda (resource) (amd-dep-text-plugin-norm resource)) amd-dep-norm-handlers)
(puthash "text" (lambda (resource) (amd-dep-text-plugin-to-var resource)) amd-dep-to-var-handlers)
(add-to-list 'amd-dep-from-file-handlers (lambda (file) (amd-dep-text-plugin-from-file file)))

(provide 'amd-dep)
