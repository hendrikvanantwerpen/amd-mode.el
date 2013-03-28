(require 'dash)
(require 's)
(require 'amd-dep)

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
      (amd-header-create (nth 1 match)
                         (nth 2 match)))))

(defun amd-header-write (header &optional add-if-new)
  (let ((match (amd-header--match)))
    (when (or match
              add-if-new)
      (if match
          (delete-region (car (nth 0 match))
                         (cdr (nth 0 match)))
        ; how can we indent the existing code?
        (goto-char (point-max))
        (insert "\n});\n")
        (goto-char (point-min)))
      (insert (amd-header--format header)))))

(defun amd-header-create (&optional deps vars)
  (setq deps (-map 'amd-dep-parse deps))
  (let ((h (list 'amd-header nil)))
    (amd-header--set-depvars (-zip deps vars) h)
    h))

(defun amd-header-add (dep var header)
  (setq dep (amd-dep-parse dep))
  (unless (amd-header-dep-by-var dep header)
    (let ((unique-var (amd-header--unique-var
                       (amd-header--safe-var var)
                       (amd-header-vars header))))
      (amd-header--set-depvars (cons (cons dep unique-var)
                                     (amd-header-depvars header))
                               header)
      unique-var)))

(defun amd-header-del-dep (dep header)
  (setq dep (amd-dep-parse dep))
  (amd-header--set-depvars
   (-remove (lambda (dv)
              (equal (car dv) dep))
            (amd-header-depvars header))
   header))

(defun amd-header-del-var (var header)
  (amd-header--set-depvars
   (-remove (lambda (v)
              (equal (cdr v) var))
            (amd-header-depvars header))
   header))

(defun amd-header--set-depvars (depvars header)
  "Replace existing depvars with the given one."
  (setcar (nthcdr 1 header) depvars))

(defun amd-header-depvars (header)
  "Return alist of (dep . var)"
  (nth 1 header))

(defun amd-header-deps (header)
  "Return list of deps. Order corresponds to amd-header-vars."
  (-map 'car (amd-header-depvars header)))

(defun amd-header-vars (header)
  "Return list of vars. Order corresponds to amd-header-deps."
  (-map 'cdr (amd-header-depvars header)))

(defun amd-header-var-by-dep (dep header)
  "Return the var belonging to this dep or nil."
  (setq dep (amd-dep-parse dep))
  (cdr (assoc dep (amd-header-depvars header))))

(defun amd-header-dep-by-var (var header)
  "Return the dep belonging to this var or nil."
  (car (rassoc var (amd-header-depvars header))))

(defun amd-header--match ()
  "Find the AMD header and parse it"
  (goto-char (point-min))
  (when (search-forward-regexp amd-header--re nil t)
    (let ((start (match-beginning 0))
          (end (match-end 0))
          (raw-deps (match-string 1))
          (raw-vars (match-string 2)))
      (let ((deps (amd-header--parse-deps raw-deps))
            (vars (amd-header--parse-vars raw-vars)))
        (list (cons start end) deps vars)))))

(defun amd-header--parse-deps (string)
  (-map (lambda (s)
          (amd-dep-parse (amd-header--strip-parens s)))
        (amd-header--parse-list string)))

(defun amd-header--parse-vars (string)
  (amd-header--parse-list string))

(defun amd-header--parse-list (string)
  (-map (lambda (s) (s-trim s))
        (s-split "," (amd-header--strip-parens string) t)))

(defun amd-header--strip-parens (string)
  (s-trim (substring (s-trim string) 1 -1)))

(defun amd-header--format (header)
  (let ((depvars (amd-header-depvars header)))
    (setq depvars (-map (lambda (depvar)
                          (cons (amd-dep-format (car depvar))
                                (cdr depvar)))
                          depvars))
    (setq depvars (sort depvars
                        (lambda (obj wrt)
                          (string< (car obj) (car wrt)))))
    (concat "define([\n"
            (s-join ",\n" (mapcar (lambda (s)
                                    (concat "    \"" s "\""))
                                  (-map 'car depvars)))
            "\n],function("
            (s-join ", " (-map 'cdr depvars))
            ") {\n")))

(defun amd-header--safe-var (var)
  "Return the string with unsafe characters removed."
  (s-replace "[^[:alnum:]_$]+" "" var))

(defun amd-header--unique-var (string strings)
  "Make string unique so that it does not exists in strings by appending a counter."
  (let ((counter 1)
        (unique-string string))
    (while (member unique-string strings)
      (setq counter (1+ counter))
      (setq unique-string (format (concat string "%d") counter)))
    unique-string))

(provide 'amd-header)
