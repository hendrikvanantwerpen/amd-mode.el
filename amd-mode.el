(require 'cl)
(require 'ido)

;;; Requires

(require 'amd-manip)

;;; In development

(defun amd--var-from-id (id)
  (let ((parts (split-string (car (last (split-string id "/"))) "-")))
    (concat (car parts)
            (mapconcat 'identity
                       (mapcar (lambda (part) (upcase-initials part))
                               (cdr parts)) ""))))

;;; Commands

(define-minor-mode amd-mode "AMD mode

This mode is intended to provide easier management of
dependencies in an AMD style Javascript module."

  nil " AMD" nil)

(defun amd-init ()
  "Make current buffer into an AMD module."
  (interactive)
  (if (amd--find-header)
    (message "AMD header already exists.")
    (amd--write-empty-header)))

(defun amd-add-id ()
  "Add AMD depenency to header"
  (interactive)
  (let ((new-id (read-string "Give AMD module id: ")))
    (let ((header (amd--find-or-create-header)))
      (amd--header-add new-id (amd--var-from-id new-id) header)
      (amd--write-header header))))

(provide 'amd-mode)
