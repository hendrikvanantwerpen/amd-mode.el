(require 'js-pkg)

; The interface

(setq amd-dep--create-handlers nil)
(setq amd-dep--to-var-handlers nil)
(setq amd-dep--to-files-handlers nil)
(setq amd-dep--from-file-handlers nil)

(defvar amd-dep--re
  "^\\(?:\\(\\(?:[[:alnum:]-_\.]+/\\)*[[:alnum:]-_]+\\)!\\)?\\([^!]+\\)$")

(defun amd-dep-create (plugin resource)
  (let ((dep (cons plugin resource)))
    (let* ((module-create (amd-dep--assoc
                           nil amd-dep--create-handlers))
           (plugin-create (amd-dep--assoc
                           plugin amd-dep--create-handlers))
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
            amd-dep--from-file-handlers)
    deps))

(defun amd-dep-to-var (dep)
  "For the given dependency, return a variable name"
  (setq dep (amd-dep-parse dep))
  (let ((handler (amd-dep--assoc (amd-dep-plugin dep)
                                amd-dep--to-var-handlers)))
    (if handler
        (funcall handler (amd-dep-resource dep)))))

(defun amd-dep-to-files (dep)
  "For the given dependency, return a file"
  (setq dep (amd-dep-parse dep))
  (let ((handler (amd-dep--assoc (amd-dep-plugin dep)
                                 amd-dep--to-files-handlers)))
    (if handler
        (funcall handler (amd-dep-resource dep)))))

; Dependency

(defun amd-dep-parse (dep-or-string)
  (if (listp dep-or-string)
      dep-or-string
    (when (string-match amd-dep--re dep-or-string)
      (let* ((plugin (match-string 1 dep-or-string))
             (resource (match-string 2 dep-or-string)))
        (amd-dep-create plugin resource)))))

(defun amd-dep-format (dep)
  (setq dep (amd-dep-parse dep))
  (let ((plugin (amd-dep-plugin dep))
        (resource (amd-dep-resource dep)))
    (if (not plugin)
        resource
      (concat plugin "!" resource))))

(defun amd-dep-plugin (dep)
  (setq dep (amd-dep-parse dep))
  (car dep))

(defun amd-dep-resource (dep)
  (setq dep (amd-dep-parse dep))
  (cdr dep))

(defun amd-dep-register-plugin (plugin &optional create
                                       to-var from-file to-files)
  (when create (add-to-list 'amd-dep--create-handlers
                            (cons plugin create)))
  (when to-var (add-to-list 'amd-dep--to-var-handlers
                            (cons plugin to-var)))
  (when to-files (add-to-list 'amd-dep--to-files-handlers
                              (cons plugin to-files)))
  (when from-file (add-to-list 'amd-dep--from-file-handlers
                               from-file)))

; Handling of AMD modules

(setq amd-dep--module-id-re
      "^\\(?:[[:alnum:]-_\.]+/\\)*\\([[:alnum:]-_]+\\)$")

(defun amd-dep--module-create (resource)
  (if (string-match amd-dep--module-id-re resource)
      (js-pkg-res-id-normalize resource)))

(defun amd-dep--module-from-file (file)
  (let ((resource (js-pkg-file-to-res file)))
    (if (and resource
             (string-match "^\\(.*\\)\\.js$" resource))
        (let ((module (match-string 1 resource)))
          (amd-dep-create nil module)))))

(defun amd-dep--module-to-files (module)
  (let ((resource (concat module ".js")))
    (js-pkg-res-to-files resource)))

(defun amd-dep--module-to-var (resource)
  (if (string-match amd-dep--module-id-re resource)
      (let ((name (match-string 1 resource)))
        (amd-dep--camelize name))))

(amd-dep-register-plugin nil
  (lambda (resource) (amd-dep--module-create resource))
  (lambda (resource) (amd-dep--module-to-var resource))
  (lambda (file) (amd-dep--module-from-file file))
  (lambda (resource) (amd-dep--module-to-files resource)))

; Handling for the text plugin

(defun amd-dep--text-plugin-create (resource)
  (js-pkg-res-id-normalize resource))

(defun amd-dep--text-plugin-from-file (file)
  (let ((resource (js-pkg-file-to-res file)))
    (when resource
      (amd-dep-create "text" resource))))

(defun amd-dep--text-plugin-to-files (resource)
  (js-pkg-res-to-files resource))

(defun amd-dep--text-plugin-to-var (resource)
  (amd-dep--camelize (concat (file-name-sans-extension
                     (file-name-nondirectory resource))
                    "-" (file-name-extension resource))))

(amd-dep-register-plugin "text"
  (lambda (resource) (amd-dep--text-plugin-create resource))
  (lambda (resource) (amd-dep--text-plugin-to-var resource))
  (lambda (file) (amd-dep--text-plugin-from-file file))
  (lambda (resource) (amd-dep--text-plugin-to-files resource)))

; Util function

(defun amd-dep--camelize (string)
  (let ((parts (split-string string "-")))
    (concat (car parts)
            (mapconcat 'identity
                       (mapcar (lambda (part) (upcase-initials part))
                               (cdr parts)) ""))))

(defun amd-dep--assoc (key list)
  (cdr (assoc key list)))

(provide 'amd-dep)
