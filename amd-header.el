;;; amd-mode.el --- Emacs AMD/RequireJS minor mode

;; Copyright 2013 Hendrik van Antwerpen

;; Author: Hendrik van Antwerpen <hendrik@van-antwerpen.net>
;; Version: 0.2.0
;; Package-Requires: ((dash "1.1.0") (s "1.3.1"))

;; This file is not part of GNU Emacs.

;; This file is part of amd-mode.

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
(require 's)
(require 'amd-util)

(setq amd-header--re
  (let*
      ((g< "\\(?:")
       (>g "\\)")
       (string (concat g< "\"[^\"]*\"\\|'[^']*'" >g))
       (strings? (concat (amd--seplist string) "?"))
       (identifiers? (concat (amd--seplist amd--js-id) "?")))
    (concat "^define" amd--ws*
            "(" amd--ws* "\\(\\[" amd--ws* strings? amd--ws* "\\]\\)"
            amd--ws* "," amd--ws*
            "function" amd--ws*
            "\\((" amd--ws* identifiers? amd--ws* ")\\)"
            amd--ws* "{")))

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
    (save-excursion
      (cond (match
             (delete-region (car (nth 0 match))
                            (cdr (nth 0 match)))
             (goto-char (car (nth 0 match)))
             (insert (amd-header--format header)))
            (add-if-new
             (let ((body-min nil)
                   (body-max nil))
               (goto-char (point-min))
               (insert (amd-header--format header))      
               (setq body-min (point))
               (insert "\n")
               (goto-char (point-max))
               (when (and (eobp) (not (bolp)))
                 (insert "\n"))
               (setq body-max (point))
               (insert "});\n")
               (print body-min)
               (print body-max)
               (indent-region body-min body-max)))))))

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
  (unless (amd-header-var-by-dep dep header)
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
            (amd-header-depvars header))
   header))

(defun amd-header-del-var (var header)
  "Delete the dependency with name var (cannot be nil)."
  (when var
    (amd-header--set-depvars
     (-remove (lambda (v)
                (equal (cdr v) var))
              (amd-header-depvars header))
     header)))

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
            (raw-deps (match-string-no-properties 1))
            (raw-vars (match-string-no-properties 2)))
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
   "define(["
   (s-join "," (mapcar (lambda (s)
                           (concat "\n    \"" s "\""))
                         (-map 'amd-dep-format
                               (amd-header-deps header))))
   "\n], function("
   (s-join ", " (amd-header-vars header))
   ") {"))

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

(provide 'amd-header)
;;; amd-header.el ends here
