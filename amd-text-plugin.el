;;; amd-text-plugin.el --- Emacs AMD/RequireJS Text plugin

;; Copyright 2013 Hendrik van Antwerpen

;; Author: Hendrik van Antwerpen <hendrik@van-antwerpen.net>
;; Version: 0.2.0
;; Package-Requires: ((js-pkg "0.2.0") (s "1.3.1"))

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

(require 'js-pkg)
(require 's)

(defun amd-dep--text-plugin-create (resource)
  (js-pkg-res-id-normalize resource))

(defun amd-dep--text-plugin-from-file (file)
  (let ((resource (js-pkg-file-to-res file)))
    (when resource
      (amd-dep-create "text" resource))))

(defun amd-dep--text-plugin-to-files (resource)
  (js-pkg-res-to-files resource))

(defun amd-dep--text-plugin-to-var (resource)
  (amd--camelize
   (concat (file-name-sans-extension
            (file-name-nondirectory resource))
           "-" (file-name-extension resource))))

(amd-dep-register-plugin "text"
  (lambda (resource) (amd-dep--text-plugin-create resource))
  (lambda (resource) (amd-dep--text-plugin-to-var resource))
  (lambda (file) (amd-dep--text-plugin-from-file file))
  (lambda (resource) (amd-dep--text-plugin-to-files resource)))

(provide 'amd-text-plugin)
;;; amd-text-plugin.el ends here
