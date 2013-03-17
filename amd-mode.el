(require 'ido)

;;; Requires

(require 'amd-header)
(require 'amd-dep)
(require 'amd-package)
(require 'amd-util)

;;; Commands

(define-minor-mode amd-mode "AMD mode

This mode is intended to provide easier management of
dependencies in an AMD style Javascript module."

  nil " AMD" nil)

(defun amd-init ()
  "Make current buffer into an AMD module."
  (interactive)
  (save-excursion
    (if (not (amd--find-header))
        (amd--write-empty-header)
      (message "AMD header already exists."))))

(defun amd-add-dep ()
  "Add AMD dependency to header"
  (interactive)
  (save-excursion
    (let* ((depstr (read-string "Give AMD dependency: "))
           (dep (amd-dep-parse depstr)))
      (if (not dep)
          (message "Dependency %s has invalid format." depstr)
        (amd-write-dep-to-header dep)))))

(defun amd-add-file ()
  "Add AMD dependency to header by file"
  (interactive)
  (save-excursion
    (let ((new-file (read-file-name "Select dependency file: ")))
      (if (if (not (file-exists-p new-file))
            (if (y-or-n-p "File does not exist, do you want to create it? ")
                (with-current-buffer (find-file-other-window new-file)
                  (set-buffer-modified-p t)
                  t))
          t)
          (let* ((deps (amd-deps-from-file new-file))
                 (count (length deps))
                 (dep (cond ((= 0 count)
                             (message "No options found for this file.")
                             nil)
                            ((= 1 count)
                             (car deps))
                            (t
                             (let ((depstr
                                    (completing-read "Select import: "
                                                     (mapcar 'amd-dep-format deps))))
                               (if (and depstr (not (equal "" depstr)))
                                   (amd-dep-parse depstr)))))))
            (if (not dep)
                (message "Failed to add dependency.")
              (amd-write-dep-to-header dep)))))))

(defun amd-write-dep-to-header (dep)
  (let* ((var (or (amd-dep-to-var dep)
                  (read-string "No auto-name, specify variable name: ")))
         (header (amd--find-or-create-header))
         (final-var (amd--header-add (amd-dep-format dep) var header)))
    (amd--write-header header)
    (message "Dependency %s available as %s" (amd-dep-format dep) final-var)))
    
(defun amd-goto ()
  "Open one of the dependencies"
  (interactive))

(provide 'amd-mode)
