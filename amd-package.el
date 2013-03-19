; Package functions

; A list of (package . directory) is maintained for lookups.
; * Only package.json for now
; * Warn if packages are overwritten?

(require 'amd-util)

(defvar amd-package-table (make-hash-table :test 'equal))
(puthash "dojo" "~/gamelab/qed.svn/Dev/trunk/client/dojo" amd-package-table)
(puthash "dijit" "~/gamelab/qed.svn/Dev/trunk/client/dijit/" amd-package-table)
(puthash "dijit/dgrid" "~/dgrid/" amd-package-table)

(defun amd-package-resource-from-file (file &optional current-file)
  "Return a resource for the given file, possible relative of current file, or nil."
  (let* ((pkg-res (amd-package-and-resource-from-file file))
         (cur-dir (expand-file-name (if current-file
                                        (file-name-directory current-file)
                                      default-directory)))
         (cur-pkg-res (amd-package-and-resource-from-file cur-dir)))
    (if (or (not cur-pkg-res)
            (not (equal (car pkg-res) (car cur-pkg-res))))
        (cdr pkg-res)
      (resource-relativize (cdr pkg-res)
                           (resource-directory (cdr cur-pkg-res))))))

(defun amd-package-and-resource-from-file (file)
  "For a given file, find the package and the resource"
  (let ((absolute-file (expand-file-name file))
        (package nil)
        (resource nil))
    (amd-package-find-and-add absolute-file)
    (mapcar (lambda (name)
              (let ((directory (amd-package-directory name)))
                (when directory
                  (let ((relative
                         (string-sans-prefix directory absolute-file)))
                    (when relative
                      (setq package name)
                      (setq resource (resource-normalize
                                      (concat name "/" relative))))))))
            (amd-package-names-sorted))
    (when resource
      (cons package resource))))

(defun amd-package-resource-to-file (resource &optional current-file)
  "for a given package path, return the file"
  (let* ((cur-dir (expand-file-name (if current-file
                                        (file-name-directory current-file)
                                      default-directory)))
         (current-resource (amd-package-and-resource-from-file cur-dir))
         (absolute-resource (amd-package-absolute-resource
                             resource (cdr current-resource)))
         (file nil))
    (when absolute-resource
      (mapcar (lambda (name)
                (let ((relative (string-sans-prefix name
                                                    absolute-resource)))
                  (if relative
                      (setq file
                            (expand-file-name
                             (concat (amd-package-directory name)
                                     "/" relative))))))
              (amd-package-names-sorted)))
    file))

(defun amd-package-names-sorted ()
  "Return all known package names in alphabetical order."
  (sort (hash-table-keys amd-package-table) 'string<))

(defun amd-package-name (directory)
  "Given a directory, return the package it represents or nil. Only matches when exactly the same."
  (let ((directory (expand-file-name directory))))
    (hash-table-find directory amd-package-table))

(defun amd-package-directory (name)
  "Given a package name, return an absolute path or nil."
  (let ((directory (gethash name amd-package-table)))
    (if directory
        (file-name-as-directory (expand-file-name directory)))))

(defun amd-package-find-and-add (file)
  "Find package for file and add to package table."
  (let ((result (amd-package-find file)))
    (when result
      (amd-package-add (car result) (cdr result)))))

(defun amd-package-add (name directory)
  (let* ((current-name (amd-package-name directory))
         (current-directory (amd-package-directory name)))
    (if (or (and current-name
                 (not (equal current-name name)))
            (and current-directory
                 (not (equal current-directory
                             (expand-file-name directory)))))
        (message "Package %s at %s conflicts with found %s at %s."
                 current-name current-directory
                 name directory)
      (puthash name directory amd-package-table)
      (cons name directory))))

(defun amd-package-find (file)
  (let ((reverse-parts
         (reverse (resource-split (file-name-directory
                                   (expand-file-name file))))))
    (catch 'found
      (while reverse-parts
        (let* ((search-dir (expand-file-name
                            (concat (resource-join
                                     (reverse reverse-parts))
                                    "/")))
               (name (amd-package-read-json search-dir)))
          (if (not name)
              (setq reverse-parts (cdr reverse-parts))
            (throw 'found (cons name search-dir))))))))

(defun amd-package-absolute-resource (resource reference)
  (let ((absolute-resource resource))
    (when reference
      (setq absolute-resource (resource-absolute resource reference)))
    (if (not (resource-relative-p absolute-resource))
        absolute-resource
      (message "Resource %s still relative, outside package?"
               absolute-resource)
      nil)))

(defvar amd-package-name-re "\"name\":[[:space:]\n]*\"\\([^\"]+\\)\"")

(defun amd-package-read-json (directory)
  "Detect package.json, read name or guess from folder or return nil."
  (let ((package-file (concat (expand-file-name directory) "package.json"))
        (name nil))
    (when (file-exists-p package-file)
      (let ((name (amd-package-guess-name directory)))
        (with-current-buffer (find-file-noselect package-file)
          (when (search-forward-regexp amd-package-name-re nil t)
              (setq name (match-string 1)))
          (kill-buffer))
        name))))

(defun amd-package-write-json (directory name)
  "Detect package.json, read name or guess from folder or return nil."
  (let ((package-file (concat (expand-file-name directory) "package.json")))
    (if (file-exists-p package-file)
        (message "Found existing package.json, not overwriting.")
      (with-current-buffer (find-file-noselect package-file)
        (insert (format "{\n    \"name\": \"%s\"\n}" name))
        (save-buffer)))))

(defun amd-package-guess-name (directory)
  (file-name-nondirectory (directory-file-name directory)))

(provide 'amd-package)
