(require 'ido)

;;; Requires

(require 'amd-header)
(require 'amd-package)
(require 'amd-plugin)

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

(defun amd-add-id ()
  "Add AMD module dependency header by ID"
  (interactive)
  (save-excursion
    (let ((new-id (read-string "Give AMD module id: ")))
      (let ((header (amd--find-or-create-header)))
        (amd--header-add new-id (amd--var-from-id new-id) header)
        (amd--write-header header)))))

(defun amd-add-file ()
  "Add AMD module dependency to header by file"
  (interactive)
  (save-excursion
    (let ((new-file (read-file-name "Select AMD module file: ")))
      (if (if (not (file-exists-p new-file))
            (if (y-or-n-p "AMD module does not exist, do you want to create it? ")
                (with-current-buffer (find-file-other-window new-file)
                  (if (not (amd--find-header))
                      (amd--write-empty-header))
                  t)))
          (let ((header (amd--find-or-create-header)))
            (amd--header-add new-file (amd--var-from-id new-file) header)
            (amd--write-header header))))))

(defun amd-goto ()
  "Open one of the dependencies"
  (interactive))

(provide 'amd-mode)
