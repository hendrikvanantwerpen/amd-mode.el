;;; amd-util.el --- Emacs AMD/RequireJS utility functions

;; Copyright 2013 Hendrik van Antwerpen

;; Author: Hendrik van Antwerpen <hendrik@van-antwerpen.net>
;; Version: 0.2.0
;; Package-Requires: ((js2-mode "?.?.?"))

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

(require 'js2-mode)

;; regex

(setq amd--js-id-cls "[_$[:alnum:]]")
(setq amd--js-id (concat amd--js-id-cls "+"))
(setq amd--ws-cls "[\n[:space:]]")
(setq amd--ws+ (concat amd--ws-cls "+"))
(setq amd--ws* (concat amd--ws-cls "*"))

(defun amd--seplist (term &optional separator)
  (concat "\\(?:" term "\\(?:" amd--ws* (or separator ",")
          amd--ws* term "\\)*\\)"))

;; js2
                 
(defun amd--js2-var-at-point (&optional pnt)
  "Get variable name at point or nil."
  (js2-parse)
  (let ((node (js2-node-at-point (or pnt (point)))))
    (when (and node
               (equal (js2-node-short-name node) "js2-name-node"))
      (js2-node-string node))))

(defun amd--js2-string-at-point (&optional pnt)
  "Get string value (without quotes) at point or nil."
  (js2-parse)
  (let ((node (js2-node-at-point (or pnt (point)))))
    (when (and node
               (equal (js2-node-short-name node) "js2-string-node"))
      (substring (js2-node-string node) 1 -1))))

;; alist

(defun amd--assoc (key list)
  (cdr (assoc key list)))

;; string

(defun amd--camelize (string)
  "Camelize a string, preserving the first letter's case."
  (if (s-blank? string)
      ""
    (concat (substring string 0 1)
            (s-lower-camel-case (substring string 1)))))

(provide 'amd-util)
;;; amd-util.el ends here
