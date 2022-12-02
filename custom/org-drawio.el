;;; org-drawio.el --- Drawio support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Schspa

;; Author: Schspa <schspa@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/schspa/org-drawio

;;; Commentary:

;; Quick insert Drawio files in org-mode Notes.
;; Auto refresh the preview image. Exportation supported.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
;;;; Requirements

(require 'org)
(require 'cl-lib)
(require 'filenotify)
(require 'f)

;;;; Customization

(defgroup org-drawio nil
  "org-drawio customization."
  :group 'org
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-note-dir org-directory
  "Default directory to save Drawio note files."
  :group 'org-drawio
  :type 'string
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-template-dir (expand-file-name "resources/" org-directory)
  "Directory to save Drawio template files."
  :group 'org-drawio
  :type 'string
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-link-prefix "drawio"
  "Drawio Link Prefix."
  :group 'org-drawio
  :type 'string
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-bin (if (eq system-type 'darwin)
                              "/Applications/draw.io.app/Contents/MacOS/draw.io"
                            "drawio")
  "Location of drawiopp executable."
  :group 'org-drawio
  :type 'string
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-process-picture-after-convert t
  "Whether to process the picture after conversion from *.xopp file."
  :group 'org-drawio
  :type 'bool
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-use-relative-filename t
  "Whether to use relative link path or absolute link path."
  :group 'org-drawio
  :type 'bool
  :package-version '(org-drawio . "0.1.0"))


(defcustom org-drawio-get-new-filepath #'org-drawio--new-drawio-file-in-default-dir
  "Function returning filepath of new created image."
  :group 'org-drawio
  :type 'function
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-get-new-desc (lambda () (read-string "Description: "))
  "Function returning description of new created image."
  :group 'org-drawio
  :type 'function
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-path-format-function #'org-drawio-path-format-function-default
  "Function that takes FILENAME and returns a drawio note file path."
  :group 'org-drawio
  :type 'function
  :package-version '(org-drawio . "0.1.0"))

(defcustom org-drawio-process-picture-functon #'org-drawio-process-picture-functon-default
  "Function that process the image which is converted from *.xopp file."
  :group 'org-drawio
  :type 'function
  :package-version '(org-drawio . "0.1.0"))


;;;; Variables

(defvar-local org-drawio-overlays nil
  "A-list mapping file names to overlay.")

(defvar-local org-drawio-watchers nil
  "A-list mapping file names to change watcher descriptors.")

(defvar org-drawio-always-use-default-template nil)

;;;; Commands

(defun org-drawio--new-drawio-file-in-default-dir ()
  (let (
        (file-name (read-minibuffer "New Drawio file: "
                                    (format "%s_%s"
                                            (format-time-string "%Y%m%d_%H%M%S")
                                            (org-drawio-org-heading-escape (org-entry-get nil "ITEM")))))
        )
    (funcall org-drawio-path-format-function file-name)
    )
  )


(defun org-drawio-org-heading-escape (heading)
  (setq heading (replace-regexp-in-string "\\[.*\\]" "" heading))
  ;; First filter out weird symbols
  (setq heading (replace-regexp-in-string "[/;:'\"\(\)]+" "" heading))
  (setq heading (string-trim heading))
  ;; filter out swedish characters åäö -> aao
  (setq heading(replace-regexp-in-string "[åÅäÄ]+" "a" heading))
  (setq heading(replace-regexp-in-string "[öÓ]+" "o" heading))
  ;; whitespace and . to underscores
  (setq heading (replace-regexp-in-string "[ .]+" "_" heading))
  heading
  )

(defun org-drawio-path-format-function-default (file-name)
  "The default function of `org-drawio-path-format-function'.
Get the drawio note file link path by file-name."
  (let ((absolute-path (expand-file-name (format "%s.xopp" file-name)
                                         org-drawio-note-dir)))
    (if org-drawio-use-relative-filename
        (org-link-escape (file-relative-name absolute-path))
      (org-link-escape absolute-path))
    )
  )

(defun org-drawio-process-picture-functon-default (png-path)
  "Process the image png-path after conversion."
  (call-process-shell-command (format "convert %s -trim -bordercolor '#FFFFFF' -border 25 +repage %s" png-path png-path))
  )

(defun org-drawio-get-links ()
  "Get all drawio link in buffer"
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) org-drawio-link-prefix)
        link))))

(defun org-drawio-get-page-index-from-name (drawio-path)
  "Get page index from page name"
  (let* ((path-args (split-string drawio-path "#"))
         (file-path (nth 0 path-args))
         (page-name (nth 1 path-args))
         (openwith-associations '())
         (page-index 0))
    (with-temp-buffer
      (insert-file-contents file-path)
      ;; (format-insert-file drawio-path 'plain)
      (setq dom (libxml-parse-html-region (point-min) (point-max)))
      ;;(princ "Current buffer content: %s" (buffer-string))
      ;; (pprint  dom)
      (dolist (ele (dom-by-tag dom 'diagram))
        (when (equal (alist-get 'name (nth 1 ele)) page-name)
          (return page-index))
        (setq page-index (+ page-index 1))
        ))))

(defun org-drawio-get-output-file-name-base (drawio-path)
  "Get Output file name for a drawio link, it handle #page-index algorithm"
  (let* ((path-args (split-string drawio-path "#"))
         (base-path (file-name-base (nth 0 path-args)))
         (page-name (nth 1 path-args))
         (page-index (if (string-match "^[0-9]+$" page-name)
                         (string-to-number page-name)
                       (org-drawio-get-page-index-from-name drawio-path)
                       )))
    (cons (nth 0 path-args)
          (if (<= (length path-args) 1)
              (cons (format "%s.%s.png" (or (file-name-directory drawio-path) "") base-path) nil)
            (cons (format "%s.%s-%s.png" (or (file-name-directory drawio-path) "") base-path page-index)
                  page-index)))))

(defun org-drawio-save-image (drawio-path png-path &optional page-index)
  "Convert DRAWIO-PATH to PNG and write it to PNG-PATH."
  (let* ((page-args (if page-index (format "--page-index %d" page-index) ""))
         (process_cmd (format "%s -x %s -f png -o %s %s" org-drawio-bin page-args png-path drawio-path))
         (need_process (not (file-newer-than-file-p png-path drawio-path))))
    (if need_process
        (progn
          (message "process cmd: %s" process_cmd)
          (call-process-shell-command process_cmd)))))

(defun pprint (form &optional output-stream)
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         output-stream))


(defun org-drawio-get-png (drawio-link-path)
  "Get png image data from given DRAWIO-PATH."
  (let* ((path-args (org-drawio-get-output-file-name-base drawio-link-path))
         (drawio-path (car path-args))
         (png-path (cadr path-args))
         (index (cddr path-args)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (org-drawio-save-image drawio-path png-path index)
      (if org-drawio-process-picture-after-convert (funcall org-drawio-process-picture-functon png-path))
      (insert-file-contents-literally png-path)
      (buffer-string))
    )
  )

(defun org-drawio-show-link (link)
  (org-drawio-hide-link link)
  (let* ((start (org-element-property :begin link))
         (end (org-element-property :end link))
         (overlay (make-overlay start end))
         (drawio-path (org-element-property :path link)))
    (overlay-put overlay 'display (create-image (org-drawio-get-png drawio-path) 'png t :scale 0.4))
    (push (cons drawio-path overlay) org-drawio-overlays)))

(defun org-drawio-hide-link (link)
  (let ((overlay (alist-get (org-element-property :path link) org-drawio-overlays nil nil #'string-equal)))
    (when overlay (delete-overlay overlay))))

(defun org-drawio-hide-all ()
  (dolist (link (org-drawio-get-links))
    (org-drawio-hide-link link)))


(defun org-drawio-disable ()
  "Disable watchers and hide drawio images."
  (dolist (watcher org-drawio-watchers)
    (file-notify-rm-watch (cdr watcher)))
  (setq org-drawio-watchers nil)
  (org-drawio-hide-all))

(defun org-drawio-event-file-path (event)
  (if (eq (nth 1 event) 'renamed)
      (nth 3 event)
    (nth 2 event)))

(defun org-drawio-watcher-callback (event)
  "Callback that runs after drawio files are modified."
  (let* ((drawio-path (org-drawio-event-file-path event))
         (links (org-drawio-get-links))  ;;TODO get all links?
         (paths (mapcar (lambda (it) (expand-file-name (org-element-property :path it))) links))
         (idx (cl-position drawio-path paths :test #'string-equal))) ;; then find current
    (when idx (org-drawio-show-link (nth idx links)))))

(defun org-drawio-add-watcher (drawio-path)
  "Setup auto-refreshing watcher for given drawio LINK."
  (let ((desc (file-notify-add-watch drawio-path '(change) #'org-drawio-watcher-callback)))
    (unless (alist-get drawio-path org-drawio-watchers nil nil #'string-equal)
      (push (cons drawio-path desc) org-drawio-watchers))))


(defun org-drawio-enable ()
  (unless (file-directory-p org-drawio-note-dir)
    (make-directory org-drawio-note-dir))
  (dolist (link (org-drawio-get-links))
    (org-drawio-show-link link)))


(defun org-drawio-edit (path)
  "Edit given PATH in drawio."
  (let* ((path-args (org-drawio-get-output-file-name-base path))
         (drawio-path (expand-file-name (car path-args))))
    (when (f-exists-p drawio-path)
      (cond
       ((eq system-type 'darwin)
        (call-process-shell-command (format "open -a %s %s" org-drawio-bin drawio-path))
        )
       (t  ;; TODO need feedback from other os
        (start-process-shell-command
         (format "drawio-edit:%s" drawio-path) (format "drawio-edit:%s" drawio-path)
         (format "%s %s" org-drawio-bin drawio-path))))
      (org-drawio-add-watcher drawio-path))))


(defun org-drawio-export (_path _desc _backend)
  "Export drawio canvas _PATH from Org files.
Argument _DESC refers to link description.
Argument _BACKEND refers to export backend."
  (let* ((path-args (org-drawio-get-output-file-name-base _path))
         (png-path (cadr path-args)))
    (cl-case _backend
      (html (format "<img src=\"%s\">"
                    (prog1 png-path
                      (org-drawio-save-image (car path-args) png-path (cddr path-args)))))
      (md (format "[%s](%s)"
                  (or _desc _path)
                  (prog1 png-path (org-drawio-save-image (car path-args) png-path (cddr path-args)))))
      (ascii (format "%s (%s)" (or _desc _path) _path))
      (latex (format "\\includegraphics[width=0.98\\linewidth,keepaspectratio]{%s}"
                     (prog1 png-path
                       (org-drawio-save-image (car path-args) png-path (cddr path-args)))))
      (rst (format ".. image:: %s"
                   (prog1 png-path
                     (org-drawio-save-image (car path-args) png-path (cddr path-args)))))
      )))

;;;; Functions

(org-link-set-parameters
 org-drawio-link-prefix
 :follow #'org-drawio-edit
 :export #'org-drawio-export
 )

;;;###autoload
(define-minor-mode org-drawio-mode
  "Mode for displaying editable drawio images within Org file."
  :init-value nil
  (if org-drawio-mode (org-drawio-enable) (org-drawio-disable)))

;;;; Footer
(provide 'org-drawio)

;;; org-drawio.el ends here
