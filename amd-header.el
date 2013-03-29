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
      (let ((deps (nth 1 match))
            (vars (nth 2 match)))
      (amd-header-create deps vars)))))

(defun amd-header-write (header &optional add-if-new)
  "Write the header to the current buffer (replace existing one)."
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
