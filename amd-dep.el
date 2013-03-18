(require 'amd-util)

; The interface

(setq amd-dep-create-handlers (make-hash-table :test 'equal))
(setq amd-dep-to-var-handlers (make-hash-table :test 'equal))
(setq amd-dep-to-file-handlers (make-hash-table :test 'equal))
(setq amd-dep-from-file-handlers nil)

(setq amd-dep-re
      "^\\(?:\\(\\(?:[[:alnum:]-_\.]+/\\)*[[:alnum:]-_]+\\)!\\)?\\([^!]+\\)$")

(defun amd-dep-create (plugin resource)
  (let ((dep (cons plugin resource)))
    (let* ((module-create (gethash nil amd-dep-create-handlers))
           (plugin-create (gethash plugin amd-dep-create-handlers))
           (final-plugin (if (and plugin module-create)
                            (funcall module-create plugin)
                          plugin))
           (final-resource (if plugin-create
                              (funcall plugin-create resource)
                            resource)))
      (if final-resource
          (cons final-plugin final-resource)))))

(defun amd-deps-from-file (file)
  "For the given file, return possible AMD dependencies"
  (let ((deps nil))
    (mapcar (lambda (plugin-from-file)
              (let ((dep (funcall plugin-from-file file)))
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

; Handling of MAD modules

(defun amd-dep-module-create (resource)
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

(puthash nil (lambda (resource) (amd-dep-module-create resource)) amd-dep-create-handlers)
(puthash nil (lambda (resource) (amd-dep-module-to-var resource)) amd-dep-to-var-handlers)
(add-to-list 'amd-dep-from-file-handlers (lambda (file) (amd-dep-module-from-file file)))

; Handling for the text plugin

(setq amd-dep-module-id-re
      "^\\(?:[[:alnum:]-_\.]+/\\)*\\([[:alnum:]-_]+\\)$")

(defun amd-dep-text-plugin-create (resource)
  (expand-file-name resource))

(defun amd-dep-text-plugin-from-file (file)
  (let ((fullfile (expand-file-name file)))
    (amd-dep-create "text" fullfile)))

(defun amd-dep-text-plugin-to-var (resource)
  (camelize (concat (file-name-sans-extension (file-name-nondirectory resource))
                    "-" (file-name-extension resource))))

(puthash "text" (lambda (resource) (amd-dep-text-plugin-create resource)) amd-dep-create-handlers)
(puthash "text" (lambda (resource) (amd-dep-text-plugin-to-var resource)) amd-dep-to-var-handlers)
(add-to-list 'amd-dep-from-file-handlers (lambda (file) (amd-dep-text-plugin-from-file file)))

(provide 'amd-dep)
