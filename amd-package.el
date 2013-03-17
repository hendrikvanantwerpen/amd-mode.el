; Package functions

; A list of (package . directory) is maintained for lookups.
; * Only package.json for now
; * Warn if packages are overwritten?

(defun amd-package-from-file (file)
  "For a given file, find the package or let the user input and return the package path")

(defun amd-package-to-file (id)
  "for a given package path, return the file")

(provide 'amd-package)
