;;; Requires

(require 'dash)
(require 'ido)
(require 'thingatpt)

(require 'amd-header)
(require 'amd-dep)

;;; Commands

(defvar amd-key-map (make-sparse-keymap))
(define-key amd-key-map (kbd "C-c a i") 'amd-init)
(define-key amd-key-map (kbd "C-c a d") 'amd-add-dep)
(define-key amd-key-map (kbd "C-c a f") 'amd-add-file)
(define-key amd-key-map (kbd "C-c a p") 'amd-register-pkg)
(define-key amd-key-map (kbd "C-c a g") 'amd-goto)

(define-minor-mode amd-mode "AMD mode

This mode is intended to provide easier management of
dependencies in an AMD style Javascript module."

  nil " AMD" amd-key-map)

(defun amd-init ()
  "Make current buffer into an AMD module."
  (interactive)
  (save-excursion
    (if (amd-header-read)
        (message "AMD header already exists.")
      (amd-header-write (amd-header-create)))))

(defun amd-add-dep ()
  "Add AMD dependency to header"
  (interactive)
  (save-excursion
    (let* ((depstr (read-string "Give AMD dependency: "))
           (dep (amd-dep-parse depstr)))
      (if (not dep)
          (message "Dependency %s has invalid format." depstr)
        (amd--add-dep-to-header dep)))))

(defun amd-add-file ()
  "Add AMD dependency to header by file."
  (interactive)
  (save-excursion
    (let ((new-file (ido-read-file-name "Select dependency file: ")))
      (let* ((deps (amd-deps-from-file new-file))
             (count (length deps))
             (dep (cond ((= 0 count)
                         (message "No way to add file. Maybe file is not part of a package?")
                         nil)
                        ((= 1 count)
                         (car deps))
                        (t
                         (let ((depstr
                                (ido-completing-read
                                 "Select import: "
                                 (-map 'amd-dep-format deps))))
                           (if (and depstr (not (equal "" depstr)))
                               (amd-dep-parse depstr)))))))
        (if (not dep)
            (message "Failed to add dependency.")
          (amd--add-dep-to-header dep)
          (unless (file-exists-p new-file)
            (when (y-or-n-p "File does not exist, create? ")
              (with-current-buffer (find-file-other-window new-file)
                (amd-header-write (amd-header-create) t)
                (save-buffer)))))))))

(defun amd-goto ()
  "Open one of the dependencies"
  (interactive)
  (let ((header (amd-header-read)))
    (if (not header)
        (message "No AMD header found.")
      (let* ((deps (amd-header-deps header))
             (depstrs (-map 'amd-dep-format deps))
             (depstr (ido-completing-read
                      "Open dependency: " depstrs))
             (dep (amd-dep-parse depstr)))
        (if (not dep)
            (message "Failed to parse dependency %s." depstr)
          (let* ((files (amd-dep-to-files dep))
                 (count (length files)))
            (cond ((= count 0)
                   (message "No file found for dependency %s." depstr))
                  ((= count 1)
                   (find-file-other-window (nth 0 files)))
                  (t
                   (let ((file (ido-completing-read
                                "Select from options: " files)))
                     (when file
                       (find-file-other-window (nth 0 files))))))))))))

(defun amd-register-pkg ()
  "Add a package from a directory"
  (interactive)
  (let ((directory (ido-read-directory-name
                    "Select package directory: ")))
    (unless (js-pkg-info-by-file directory)
      (when (y-or-n-p "Package doesn't exist, create?")
        (unless (file-exists-p directory)
          (make-directory directory t))
        (let ((name (read-string "Give package name: "
                                 (file-name-nondirectory
                                  (directory-file-name directory))))
              (version (semver-parse
                        (read-string "Give package version: " "0.0.0"))))
          (when (and name version)
            (js-pkg-info-write
             (js-pkg-info-create name version directory))))))))

(defun amd--add-dep-to-header (dep)
  (let* ((var (read-string "Specify variable name: "
                           (amd-dep-to-var dep)))
         (var-or-nil (when (not (s-blank? var)) var))
         (header (or (amd-header-read)
                     (amd-header-create)))
         (final-var (amd-header-add dep var-or-nil header)))
    (amd-header-write header t)
    (message "Dependency '%s' added as variable '%s'"
             (amd-dep-format dep) final-var)))
    
(provide 'amd-mode)
