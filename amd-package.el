; Package functions

; A list of (package . directory) is maintained for lookups.
; * Only package.json for now
; * Warn if packages are overwritten?

(require 'amd-util)

(setq amd-package-table (make-hash-table :test 'equal))
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
      (relativize (cdr pkg-res)
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
  (let* ((current-resource (when current-file
                             (amd-package-and-resource-from-file
                              current-file)))
         (absolute-resource
          (amd-package-absolute-resource
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

(defun amd-package-directory (name)
  "Given a package name, return an absolute path or nil."
  (let ((directory (gethash name amd-package-table)))
    (if directory
        (file-name-as-directory (expand-file-name directory)))))

(setq amd-package-name-re "\"name\":[[:space:]\n]*\"\\([^\"]+\\)\"")

(defun amd-package-find-and-add (file)
  "Find package for file and add to package table."
  (let* ((result (amd-package-find file))
         (name (car result))
         (directory (cdr result))
         (current-directory (amd-package-directory name)))
    (if (and (and result current-directory)
             (not (equal directory current-directory)))
        (message "Previous package %s at %s found at new location %s."
                 name current-directory directory)
      (puthash name directory amd-package-table)
      result)))

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
               (search-file (concat search-dir "package.json")))
          (if (not (file-exists-p search-file))
              (setq reverse-parts (cdr reverse-parts))
            (let ((name (car reverse-parts)))
              (with-current-buffer (find-file-noselect search-file)
                (if (search-forward-regexp amd-package-name-re nil t)
                    (setq name (match-string 1)))
                (kill-buffer))
              (throw 'found (cons name search-dir)))))))))

(defun amd-package-absolute-resource (resource reference)
  (let ((absolute-resource resource))
    (when reference
      (setq absolute-resource (resource-absolute resource reference)))
    (if (not (resource-relative-p absolute-resource))
        absolute-resource
      (message "Resource %s still relative, outside package?"
               absolute-resource)
      nil)))

(provide 'amd-package)
