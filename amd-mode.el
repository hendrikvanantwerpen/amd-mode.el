;;; amd-mode.el --- Emacs AMD/RequireJS minor mode

;; Copyright 2013 Hendrik van Antwerpen

;; Author: Hendrik van Antwerpen <hendrik@van-antwerpen.net>
;; Version: 0.2.0
;; Package-Requires: ((js-pkg "0.2.0") (semver "0.2.0") (dash "1.1.0") (s "1.3.1") (thingatpnt) (ido))

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

;; Minor mode & Commands

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
             (dep-at-point (amd--dep-at-point header))
             (suggested (when (member dep-at-point depstrs)
                          dep-at-point))
             (depstr (ido-completing-read
                      "Open dependency: " depstrs
                      nil t suggested))
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

(defun amd--dep-at-point (header)
  "Return a dep string for the current point."
  (when (equal major-mode 'js2-mode)
    (let ((node (js2-node-at-point)))
      (when node
        (cond ((equal (js2-node-short-name node) "js2-name-node")
               (let ((dep (amd-header-dep-by-var (js2-node-string node) header)))
                 (when dep (amd-dep-format dep))))
              ((equal (js2-node-short-name node) "js2-string-node")
               (substring (js2-node-string node) 1 -1)))))))

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
    
;; Header manipulation

(setq amd-header--re
  (let*
      ((g< "\\(?:")
       (>g "\\)")
       (string (concat g< "\"[^\"]*\"\\|'[^']*'" >g))
       (identifier "[_$[:alnum:]]+")
       (ws* "[\n[:space:]]*")
       (strings? (concat g< string
                         g< ws* "," ws* string >g "*" >g "?"))
       (identifiers? (concat g< identifier
                             g< ws* "," ws* identifier >g "*" >g "?")))
    (concat "^define" ws* "(" ws* "\\(\\[" ws* strings? ws* "\\]\\)" ws*
            "," ws* "function" ws* "\\((" ws* identifiers? ws* ")\\)" ws*
            "{")))

(defun amd-header-read ()
  "Read header from current buffer or nil."
  (let ((match (amd-header--match)))
    (when match
      (let ((deps (nth 1 match))
            (vars (nth 2 match)))
      (amd-header-create deps vars)))))

(defun amd-header-write (header &optional add-if-new)
  "Write the header to the current buffer (replace existing one)."
  (let ((match (amd-header--match)))
    (when (or match
              add-if-new)
      (save-excursion
        (if match
            (delete-region (car (nth 0 match))
                           (cdr (nth 0 match)))
          ; how can we indent the existing code?
          (goto-char (point-max))
          (insert "\n});\n")
          (goto-char (point-min)))
        (insert (amd-header--format header))))))

(defun amd-header-create (&optional deps vars)
  "Create a new header with the given dependencies and variable names (expected in corresponding order)."
  (setq deps (-map 'amd-dep-parse deps))
  (setq vars (-take (length deps) vars))
  (setq vars (append vars (-repeat (- (length deps) (length vars)) nil)))
  (let ((h (list 'amd-header nil)))
    (amd-header--set-depvars (-zip deps vars) h)
    h))

(defun amd-header-add (dep var header)
  "Add the dependency to the header with name var (can be nil)."
  (setq dep (amd-dep-parse dep))
  (unless (amd-header-dep-by-var dep header)
    (let* ((unique-var (and var
                            (amd-header--unique-var
                             (amd-header--safe-var var)
                             (amd-header-vars header))))
           (depvar (cons dep unique-var)))
      (amd-header--set-depvars (cons depvar
                                     (amd-header-depvars header))
                               header)
      unique-var)))

(defun amd-header-del-dep (dep header)
  "Remove teh given dependency from the header."
  (setq dep (amd-dep-parse dep))
  (amd-header--set-depvars
   (-remove (lambda (dv)
              (equal (car dv) dep))
            (amd-header-depvars header))))

(defun amd-header-del-var (var header)
  "Delete the dependency with name var (cannot be nil)."
  (when var
    (amd-header--set-depvars
     (-remove (lambda (v)
                (equal (cdr v) var))
              (amd-header-depvars header)))))

(defun amd-header--set-depvars (depvars header)
  "Replace existing depvars with the given one.

This keeps everything in sorted order because anonymous dependencies have to come last."
  (setq depvars (sort depvars 'amd-header--depvar<))
  (setcar (nthcdr 1 header) depvars))

(defun amd-header--depvar< (depvar with-respect-to)
  "Order dependencies, anonymous ones last, without plugin first."
  (< (amd-header--depvar-compare depvar with-respect-to) 0))
    
(defun amd-header--depvar-compare (depvar with-respect-to)
  "Compare to depvars, no var after var, no plugin before plugin, otherwise lexical."
  (let ((obj-dep (car depvar))
        (obj-var (cdr depvar))
        (wrt-dep (car with-respect-to))
        (wrt-var (cdr with-respect-to))
        (d 0))
    (when (zerop d)
      (setq d (- (if wrt-var 1 0)
                 (if obj-var 1 0)))
      )
    (when (zerop d)
      (setq d (- (if (amd-dep-plugin obj-dep) 1 0)
                 (if (amd-dep-plugin wrt-dep) 1 0)))
      )
    (when (and (zerop d)
               (amd-dep-plugin obj-dep))
      (setq d (compare-strings (amd-dep-plugin obj-dep) nil nil
                               (amd-dep-plugin wrt-dep) nil nil))
      (when (equal d t) (setq d 0))
      )
    (when (zerop d)
      (setq d (compare-strings (amd-dep-resource obj-dep) nil nil
                               (amd-dep-resource wrt-dep) nil nil))
      (when (equal d t) (setq d 0))
      )
  d))

(defun amd-header-depvars (header)
  "Return alist of (dep . var). Var part can be nil."
  (nth 1 header))

(defun amd-header-deps (header)
  "Return list of deps. Order corresponds to amd-header-vars."
  (-map 'car (amd-header-depvars header)))

(defun amd-header-vars (header)
  "Return list of vars. Order corresponds to amd-header-deps. This list can be shorter than the one from amd-header-vars."
  (-take-while 'identity
               (-map 'cdr (amd-header-depvars header))))

(defun amd-header-var-by-dep (dep header)
  "Return the var belonging to this dep or nil."
  (setq dep (amd-dep-parse dep))
  (cdr (assoc dep (amd-header-depvars header))))

(defun amd-header-dep-by-var (var header)
  "Return the dep belonging to this var or nil."
  (when var
    (car (rassoc var (amd-header-depvars header)))))

(defun amd-header--match ()
  "Find the AMD header and parse it. Returns '((start end) deps vars)."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp amd-header--re nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (raw-deps (match-string 1))
            (raw-vars (match-string 2)))
        (let ((deps (amd-header--parse-deps raw-deps))
              (vars (amd-header--parse-vars raw-vars)))
          (list (cons start end) deps vars))))))

(defun amd-header--parse-deps (string)
  "Splits string and parses them to a list of deps."
  (-map (lambda (s)
          (amd-dep-parse (amd-header--strip-parens s)))
        (amd-header--parse-list string)))

(defun amd-header--parse-vars (string)
  "Splits the string into a list of variable names"
  (amd-header--parse-list string))

(defun amd-header--parse-list (string)
  "Split a comma seperated list."
  (-map (lambda (s) (s-trim s))
        (s-split "," (amd-header--strip-parens string) t)))

(defun amd-header--strip-parens (string)
  "Strip two outermost non-whitespace characters and remove inner whitespace as well."
  (s-trim (substring (s-trim string) 1 -1)))

(defun amd-header--format (header)
  "Format the given header to a string."
  (concat
   "define([\n"
   (s-join ",\n" (mapcar (lambda (s)
                           (concat "    \"" s "\""))
                         (-map 'amd-dep-format
                               (amd-header-deps header))))
   "\n],function("
   (s-join ", " (amd-header-vars header))
   ") {\n"))

(defun amd-header--safe-var (var)
  "Return the string with unsafe characters removed."
  (let ((parts (s-split "-"
                        (replace-regexp-in-string
                         "[^[:alnum:]_\$]+" "-" var))))
    (when parts
      (concat
       (car parts)
       (s-join "" (-map (lambda (s)
                          (concat (s-upcase (s-left 1 s))
                                  (substring s 1)))
                        (cdr parts)))))))
  
(defun amd-header--unique-var (string strings)
  "Make string unique so that it does not exists in strings by appending a counter."
  (let ((counter 1)
        (unique-string string))
    (while (member unique-string strings)
      (setq counter (1+ counter))
      (setq unique-string (format (concat string "%d") counter)))
    unique-string))

;; AMD dependency and plugin handling

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
      "^\\(?:[[:alnum:]-_\.]+/\\)*\\([[:alnum:]-_\.]+\\)$")

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
        (s-lower-camel-case name))))

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
  (s-lower-camel-case
   (concat (file-name-sans-extension
            (file-name-nondirectory resource))
           "-" (file-name-extension resource))))

(amd-dep-register-plugin "text"
  (lambda (resource) (amd-dep--text-plugin-create resource))
  (lambda (resource) (amd-dep--text-plugin-to-var resource))
  (lambda (file) (amd-dep--text-plugin-from-file file))
  (lambda (resource) (amd-dep--text-plugin-to-files resource)))

; Handling for the dojo/text plugin

(defun amd-dep--dojo-text-plugin-create (resource)
  (js-pkg-res-id-normalize resource))

(defun amd-dep--dojo-text-plugin-from-file (file)
  (let ((resource (js-pkg-file-to-res file)))
    (when resource
      (amd-dep-create "dojo/text" resource))))

(defun amd-dep--dojo-text-plugin-to-files (resource)
  (js-pkg-res-to-files resource))

(defun amd-dep--dojo-text-plugin-to-var (resource)
  (s-lower-camel-case
   (concat (file-name-sans-extension
            (file-name-nondirectory resource))
           "-" (file-name-extension resource))))

(amd-dep-register-plugin "dojo/text"
  (lambda (resource) (amd-dep--dojo-text-plugin-create resource))
  (lambda (resource) (amd-dep--dojo-text-plugin-to-var resource))
  (lambda (file) (amd-dep--dojo-text-plugin-from-file file))
  (lambda (resource) (amd-dep--dojo-text-plugin-to-files resource)))

; Util function

(defun amd-dep--assoc (key list)
  (cdr (assoc key list)))

;; Provide package symbol

(provide 'amd-mode)

;;; amd-mode.el ands here
