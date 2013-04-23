;;; amd-mode.el --- Emacs AMD/RequireJS minor mode

;; Copyright 2013 Hendrik van Antwerpen

;; Author: Hendrik van Antwerpen <hendrik@van-antwerpen.net>
;; Version: 0.2.0
;; Package-Requires: ((js2-mode "?.?.?") (js-pkg "0.2.0") (semver "0.2.0") (dash "1.1.0") (s "1.3.1") (ido))

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;     http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Code:

(require 'dash)
(require 'semver)
(require 'js-pkg)
(require 'ido)
(require 'thingatpt)
(require 's)

(require 'amd-header)
(require 'amd-dep)
(require 'amd-util)

;; Minor mode & Commands

(defvar amd-key-map (make-sparse-keymap))
(define-key amd-key-map (kbd "C-c a i") 'amd-init)
(define-key amd-key-map (kbd "C-c a d") 'amd-add-dep)
(define-key amd-key-map (kbd "C-c a f") 'amd-add-file)
(define-key amd-key-map (kbd "C-c a p") 'amd-register-pkg)
(define-key amd-key-map (kbd "C-c a g") 'amd-goto)
(define-key amd-key-map (kbd "C-c a G") 'amd-goto-other-window)
(define-key amd-key-map (kbd "C-c a x") 'amd-remove)

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
      (amd-header-write (amd-header-create) t))))

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
  "Open one of the dependencies in this window."
  (interactive)
  (let ((file (amd--get-goto-file)))
    (when file
      (find-file file))))

(defun amd-goto-other-window ()
  "Open one of the dependencies in a different window."
  (interactive)
  (let ((file (amd--get-goto-file)))
    (when file
      (find-file-other-window file))))

(defun amd--get-goto-file ()
  "Return the file after selecting a dependency or nil."
  (let ((header (amd-header-read)))
    (if (not header)
        (message "No AMD header found.")
      (let* ((deps (amd-header-deps header))
             (depstrs (-map 'amd-dep-format deps))
             (dep-at-point (amd--dep-at-point header))
             (suggested (when (and dep-at-point
                                   (member dep-at-point depstrs))
                          dep-at-point))
             (depstr (ido-completing-read
                      "Open dependency: " depstrs
                      nil t suggested))
             (dep (amd-dep-parse depstr)))
        (if (not dep)
            (progn
              (message "Failed to parse dependency %s." depstr)
              nil)
          (let* ((files (amd-dep-to-files dep))
                 (count (length files)))
            (cond ((= count 0)
                   (message "No file found for dependency %s." depstr)
                   nil)
                  ((= count 1)
                   (nth 0 files))
                  (t
                   (ido-completing-read "Select from options: " files)))))))))

(defun amd-remove ()
  "Remove one of the dependencies"
  (interactive)
  (save-excursion
    (let ((header (amd-header-read)))
      (if (not header)
          (message "No AMD header found.")
        (let* ((deps (amd-header-deps header))
               (depstrs (-map 'amd-dep-format deps))
               (dep-at-point (amd--dep-at-point header))
               (suggested (when (member dep-at-point depstrs)
                            dep-at-point))
               (depstr (ido-completing-read
                        "Remove dependency: " depstrs
                        nil t suggested))
               (dep (amd-dep-parse depstr)))
          (if (not dep)
              (message "Failed to parse dependency %s." depstr)
            (amd-header-del-dep dep header)
            (amd-header-write header)))))))

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

(defun amd--dep-at-point (header)
  "Return a dep string for the current point or nil."
  (let ((var nil)
        (depstr nil))
    (setq var (amd--js2-var-at-point))
    (unless var
      (setq depstr (amd--js2-string-at-point)))
    (unless (or depstr var)
      (setq var (word-at-point)))
    (or depstr
        (when var
          (let ((dep (amd-header-dep-by-var var header)))
            (when dep (amd-dep-format dep)))))))

(defun amd--add-dep-to-header (dep)
  "Add dependency to the header, possibly creating the header."
  (let* ((pre-header (amd-header-read))
         (pre-var (and pre-header (amd-header-var-by-dep dep pre-header))))
    (if pre-var
        (message "Dependency '%s' already exists as variable '%s'"
                 (amd-dep-format dep) pre-var)
      (let* ((var (read-string "Specify variable name: "
                               (amd-dep-to-var dep)))
             (var-or-nil (when (not (s-blank? var)) var))
             (header (or pre-header (amd-header-create)))
             (final-var (amd-header-add dep var-or-nil header)))
        (amd-header-write header t)
        (message "Dependency '%s' added as variable '%s'"
                 (amd-dep-format dep) final-var)))))

(defun amd-config-default ()
  (add-hook 'js2-mode-hook
            (lambda () (amd-mode)))
  (add-hook 'js2-post-parse-callbacks
            'amd-externs-setup))

(defun amd-externs-setup ()
  (add-to-list 'js2-additional-externs "define")
  (add-to-list 'js2-additional-externs "require"))

(provide 'amd-mode)
;;; amd-mode.el ends here
