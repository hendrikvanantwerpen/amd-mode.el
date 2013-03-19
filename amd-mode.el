;;; Requires

(require 'ido)

(require 'amd-header)
(require 'amd-dep)
(require 'amd-package)
(require 'amd-util)

;;; Commands

(defvar amd-key-map (make-sparse-keymap))
(define-key amd-key-map (kbd "C-c a i") 'amd-init)
(define-key amd-key-map (kbd "C-c a d") 'amd-add-dep)
(define-key amd-key-map (kbd "C-c a f") 'amd-add-file)
(define-key amd-key-map (kbd "C-c a p") 'amd-add-pkg)
(define-key amd-key-map (kbd "C-c a g") 'amd-goto)

(define-minor-mode amd-mode "AMD mode

This mode is intended to provide easier management of
dependencies in an AMD style Javascript module."

  nil " AMD" amd-key-map)

(defun amd-init ()
  "Make current buffer into an AMD module."
  (interactive)
  (save-excursion
    (if (not (amd-find-header))
        (amd-write-empty-header)
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
    (let ((new-file (ido-read-file-name "Select dependency file: ")))
      (when (if (not (file-exists-p new-file))
                (when (y-or-n-p "File does not exist, create? ")
                  (with-current-buffer (find-file-other-window new-file)
                    (save-buffer)
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
                                  (ido-completing-read
                                   "Select import: "
                                   (mapcar 'amd-dep-format deps))))
                             (if (and depstr (not (equal "" depstr)))
                                 (amd-dep-parse depstr)))))))
          (if (not dep)
              (message "Failed to add dependency.")
            (amd-write-dep-to-header dep)))))))

(defun amd-write-dep-to-header (dep)
  (let* ((var (or (amd-dep-to-var dep)
                  (read-string "No auto-name, specify variable name: ")))
         (header (amd-find-or-create-header))
         (final-var (amd-header-add (amd-dep-format dep) var header)))
    (amd-write-header header)
    (message "Dependency %s available as %s" (amd-dep-format dep) final-var)))
    
(defun amd-add-pkg ()
  "Add a package from a directory"
  (interactive)
  (let ((directory (ido-read-directory-name
                    "Select package directory: ")))
    (when (if (not (file-exists-p directory))
              (when (y-or-n-p "Directory doesn't exist, create?")
                (make-directory directory t)
                t)
            t)
      (let ((name (amd-package-read-json directory)))
        (when (not name)
          (setq name (read-string "No package name found, specify: "
                                  (or (amd-package-name directory)
                                      (amd-package-guess-name directory))))
          (when (y-or-n-p "No package.json found, create? ")
            (amd-package-write-json directory name)))
        (amd-package-add name directory)))))

(defun amd-goto ()
  "Open one of the dependencies"
  (interactive)
  (let ((header (amd-find-header)))
    (if header
        (let* ((depstrs (amd-header-ids header))
               (depstr (ido-completing-read "Open dependency: " depstrs))
               (dep (amd-dep-parse depstr)))
           (if dep
               (let ((file (amd-dep-to-file dep)))
                 (if file
                     (find-file-other-window file)
                   (message "No file found for dependecy %s." depstr)))
             (message "Failed to parse dependency %s." depstr)))
      (message "No AMD header found."))))

(provide 'amd-mode)
