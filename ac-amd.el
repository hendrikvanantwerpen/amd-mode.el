;;; ac-amd.el --- Emacs AMD/RequireJS auto completion

;; Copyright 2013 Hendrik van Antwerpen

;; Author: Hendrik van Antwerpen <hendrik@van-antwerpen.net>
;; Version: 0.2.0
;; Package-Requires: ((s "1.3.1"))

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

;;; Commentary:

;; It seems that multiple sources with a prefix don't play very well together.
;; The current solution works (at least with only the ac-amd sources enabled).
;; Not sure if this is a bug in this code, or a feature in auto-complete.
;; A comment by the auto-complete author suggests this is intentional,
;; see https://github.com/auto-complete/auto-complete/pull/175#issuecomment-8859875

;;; Code:

(require 's)
(require 'auto-complete)
(require 'amd-header)
(require 'amd-util)

(defvar ac-amd-properties nil)
(defvar ac-amd-functions nil)
(defvar ac-amd-constructors nil)

(setq ac-source-amd-variables
  '((candidates . ac-source-amd-variable-candidates)
    (requires . 1)
    ))

(defun ac-source-amd-variable-candidates ()
  (let ((header (amd-header-read)))
    (when header
      (amd-header-vars header))))

(setq ac-source-amd-properties
  '((candidates . ac-source-amd-property-candidates)
    (prefix . ac-source-amd-property-start-pos)
    (requires . 0)
    ))

(setq ac-source-amd-property-re
      (concat "\\(" amd--js-id "\\)\\." amd--js-id-cls "*(?"))

(defun ac-source-amd-property-start-pos ()
  (when (looking-back ac-source-amd-property-re nil t)
    (let ((pos (1+ (match-end 1))))
      pos)))

(defun ac-source-amd-property-candidates ()
  (save-excursion
    (goto-char ac-point)
    (when (looking-back ac-source-amd-property-re nil t)
      (let* ((var (match-string-no-properties 1))
             (header (amd-header-read))
             (module-id (amd--get-module-id var header)))
        (when module-id
          (amd--assoc module-id ac-amd-properties))))))

(setq ac-source-amd-functions
  '((candidates . ac-source-amd-function-candidates)
    (requires . 1)
    ))

(defun ac-source-amd-function-candidates ()
  (let ((header (amd-header-read)))
    (when header
      (-filter 'identity
        (-map (lambda (var)
                (let ((mid (amd--get-module-id var header)))
                  (when mid
                    (let ((args (amd--assoc mid ac-amd-functions)))
                      (when args
                        (concat var args))))))
              (amd-header-vars header))))))

(defun amd--get-module-id (&optional var header)
  (when (and var header)
    (let ((dep (amd-header-dep-by-var var header)))
      (when (and dep
                 (not (amd-dep-plugin dep)))
            (amd-dep-resource dep)))))
    
(setq ac-source-amd-constructors
  '((candidates . ac-source-amd-constructor-candidates)
    (prefix . ac-source-amd-constructor-start-pos)
    (requires . 0)
    ))

(defun ac-source-amd-constructor-start-pos ()
  (when (looking-back (concat "\\(new \\)" amd--js-id-cls "*") nil t)
    (let ((pos (match-end 1)))
      (print pos)
      pos)))

(defun ac-source-amd-constructor-candidates ()
  (let ((header (amd-header-read)))
    (when header
      (-filter 'identity
        (-map (lambda (var)
                (let ((mid (amd--get-module-id var header)))
                  (when mid
                    (let ((args (amd--assoc mid ac-amd-constructors)))
                      (when args
                        (concat var args))))))
              (amd-header-vars header))))))

(defun ac-amd-setup ()
  (setq ac-sources (append '(ac-source-amd-variables
                             ac-source-amd-functions
                             ac-source-amd-properties
                             ac-source-amd-constructors)
                           ac-sources))
  (setq ac-auto-start 0))

(defun ac-amd-config-default ()
  (add-hook 'amd-mode-hook
            (lambda ()
              (ac-amd-setup))))

(provide 'ac-amd)
;;; ac-amd.el ends here
