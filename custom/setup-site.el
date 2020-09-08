;;; setup-site.el --- Set up my blog site

;; Author: schspa  schspa@gmail.com
;; URL:

;; Copyright (C) 2020, schspa, all rights reserved.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
;;; init-site.el --- Exports org to site.
;;; Commentary:
;;; Code:

(use-package htmlize
  :ensure t
  ;; :config
  ;; (setq htmlize-output-type 'font)
  )
(eval-after-load 'ox-html
  (setq
   ;; org-html-doctype "html5"
   ;; org-export-default-language "ch"
   user-full-name "Schspa"))

(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        ))

(defun org-blogs-get-date (filename)
  "Extract the `#+date:` from file-name as date-time."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+date:[ ]*<\\([^]>]+\\)>$" nil t)
	      (date-to-time (match-string 1))
	    (time-since 0)))))

(defun org-blogs-generate-post-link-path (post-filename)
  (format-time-string "%Y/%m/%d/" (org-blogs-get-date post-filename)))

(defun org-blogs-generate-post-path (post-filename)
  (concat "~/site/public/" (org-blogs-generate-post-link-path post-filename)))

(defun schspa/readlines (filePath)
  "Return a list for a file"
  (with-temp-buffer
	(insert-file-contents filePath)
	(split-string (buffer-string) "\n" t)))

(defcustom schspa/blog-source-dir "~/org/"
  "font-size in mm."
  :group 'schspa
  :type 'string)

(defun schspa/getposts (filePath)
  "Return posts file name"
  (seq-filter 'file-readable-p
			  (mapcar (lambda (arg)
						(expand-file-name arg schspa/blog-source-dir))
					  (schspa/readlines
					   (expand-file-name filePath schspa/blog-source-dir)))))

(defun org-blog-get-out-dir (filename default)
  (let* ((is-external-file
          (string-prefix-p "../" (file-relative-name filename "~/org/blogs/")))
         (out-dir (if is-external-file
                      (org-blogs-generate-post-path filename) default)))
    out-dir))

(defun schspa/org-html-link (orig-fun &rest args)
  (if (string-equal "file" (org-element-property :type (car args)))
	  (let* ((raw-path (org-element-property :path (car args)))
             (output-path
              (concat
               (org-blogs-generate-post-link-path (expand-file-name raw-path))
               (file-name-nondirectory raw-path))))
        (org-element-put-property (car args) :path output-path))
    )
  (apply orig-fun args))

(defun org-html-publish-to-blogs (plist filename pub-dir)
  "Publish an org file to HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let ((output))
    (advice-add 'org-html-link :around #'schspa/org-html-link)
    (setq output (org-html-publish-to-html
                  plist filename (org-blog-get-out-dir filename pub-dir)))
    (advice-remove 'org-html-link #'schspa/org-html-link)
    output))

(defun org-blogs-postamble (info)
  (with-temp-buffer
    (let ((files '()))
      (unless (string-prefix-p "index." (plist-get info :input-buffer))
        (add-to-list 'files "~/site/assets/postamble.html"))
      (add-to-list 'files "~/site/assets/footer.html")
      (dolist (postamblefile files) (insert-file-contents postamblefile)))
    (buffer-string)))

(eval-after-load 'ox-publish
  ;; org-publish-project-alist
  ;; ("project-name" :property value :property value ...)
  ;; ("project-name" :components ("project-name" "project-name" ...))
  (lambda ()
    (setq org-publish-project-alist
          `(("orgfiles"
             ;; ; Sources and destinations for files.
             :base-directory "~/org/blogs/"  ;; local dir
             :publishing-directory "~/site/public/" ;; :publishing-directory "/ssh:jack@192.112.245.112:~/site/public/"
             ;; :preparation-function
             ;; :complete-function

             ;; ; Selecting files
             :base-extension "org"
             ;; :exclude "PrivatePage.org"     ;; regexp
             :include ,(schspa/getposts "posts")
             :recursive t

             ;; ; Publishing action
             :publishing-function org-html-publish-to-blogs

             ;; :htmlized-source

             ;; ;;; Options for the exporters

             ;; ; Generic properties
             ;; :archived-trees	org-export-with-archived-trees
             ;; :exclude-tags	org-export-exclude-tags
             :headline-levels 4 ;; org-export-headline-levels
             ;; :language	org-export-default-language
             ;; :preserve-breaks	org-export-preserve-breaks
             :section-numbers nil	;; org-export-with-section-numbers
             ;; :select-tags	org-export-select-tags
             :with-author "Schspa" ;; org-export-with-author
             ;; :with-broken-links	org-export-with-broken-links
             ;; :with-clocks	t ;; org-export-with-clocks
             ;; :with-creator nil ;; org-export-with-creator
             ;; :with-date org-export-with-date
             ;; :with-drawers	org-export-with-drawers
             ;; :with-email	org-export-with-email
             ;; :with-emphasize	org-export-with-emphasize
             ;; :with-fixed-width org-export-with-fixed-width
             ;; :with-footnotes	org-export-with-footnotes
             ;; :with-latex	org-export-with-latex
             ;; :with-planning	org-export-with-planning
             :with-priority t ;; org-export-with-priority ;
             ;; :with-properties	org-export-with-properties
             ;; :with-special-strings	org-export-with-special-strings
             ;; :with-sub-superscript	org-export-with-sub-superscripts
             ;; :with-tables	org-export-with-tables
             ;; :with-tags	org-export-with-tags
             ;; :with-tasks	org-export-with-tasks
             ;; :with-timestamps	org-export-with-timestamps
             ;; :with-title	org-export-with-title
             :with-toc t ;; org-export-with-toc
             ;; :with-todo-keywords	org-export-with-todo-keywords

             ;; ; HTML specific properties
             ;; :html-allow-name-attribute-in-anchors	org-html-allow-name-attribute-in-anchors
             ;; :html-checkbox-type	org-html-checkbox-type
             ;; :html-container	org-html-container-element
             ;; :html-divs	org-html-divs
             :html-doctype "html5" ;; org-html-doctype
             ;; :html-extension	org-html-extension
             ;; :html-footnote-format nil ;; org-html-footnote-format
             ;; :html-footnote-separator	org-html-footnote-separator
             ;; :html-footnotes-section	org-html-footnotes-section
             ;; :html-format-drawer-function	org-html-format-drawer-function
             ;; :html-format-headline-function	org-html-format-headline-function
             ;; :html-format-inlinetask-function	org-html-format-inlinetask-function
             ;; :html-head-extra	org-html-head-extra
             ;; :html-head-include-default-style	org-html-head-include-default-style
             ;; :html-head-include-scripts	org-html-head-include-scripts
             ;; :html-head	org-html-head
             :html-head	,(concat "<link rel=\"shortcut icon\" href=\"/images/rose-red.png\"
type=\"image/x-icon\" />"
                                 "<link rel=\"stylesheet\" href=\"/css/animate.min.css\" />"
			                     "<link rel=\"stylesheet\" href=\"/css/all.min.css\" />"
			                     "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\" />"
			                     ""
			                     "<script src=\"/js/jquery.min.js\"></script>"
			                     "<script src=\"/js/darkreader.js\"></script>"
			                     "<script src=\"/user.config.js\"></script>"
			                     "<script src=\"/js/main.js\"></script>")
             ;; :html-home/up-format	org-html-home/up-format
             ;; :html-html5-fancy	org-html-html5-fancy
             ;; :html-indent	org-html-indent
             ;; :html-infojs-options	org-html-infojs-options
             ;; :html-infojs-template	org-html-infojs-template
             ;; :html-inline-image-rules	org-html-inline-image-rules
             ;; :html-inline-images	org-html-inline-images
             ;; :html-link-home	org-html-link-home
             ;; :html-link-org-files-as-html	org-html-link-org-files-as-html
             ;; :html-link-up	org-html-link-up
             ;; :html-link-use-abs-url	org-html-link-use-abs-url
             ;; :html-mathjax-options	org-html-mathjax-options
             ;; :html-mathjax-template	org-html-mathjax-template
             ;; :html-metadata-timestamp-format	org-html-metadata-timestamp-format
             ;; :html-postamble-format  org-html-postamble-format
             :html-postamble org-blogs-postamble ;; org-html-postamble
             ;; :html-preamble-format	org-html-preamble-format
             ;; :html-preamble nil ;; org-html-preamble
             ;; :html-self-link-headlines	org-html-self-link-headlines
             ;; :html-table-align-individual-field	de{org-html-table-align-individual-fields
             ;; :html-table-attributes	org-html-table-default-attributes
             ;; :html-table-caption-above	org-html-table-caption-above
             ;; :html-table-data-tags	org-html-table-data-tags
             ;; :html-table-header-tags	org-html-table-header-tags
             ;; :html-table-row-tags	org-html-table-row-tags
             ;; :html-table-use-header-tags-for-first-column	org-html-table-use-header-tags-for-first-column
             ;; :html-tag-class-prefix	org-html-tag-class-prefix
             ;; :html-text-markup-alist	org-html-text-markup-alist
             ;; :html-todo-kwd-class-prefix	org-html-todo-kwd-class-prefix
             ;; :html-toplevel-hlevel	org-html-toplevel-hlevel
             ;; :html-use-infojs	org-html-use-infojs
             ;; :html-validation-link	org-html-validation-link
             ;; :html-viewport	org-html-viewport
             ;; :html-wrap-src-lines	org-html-wrap-src-lines
             ;; :html-xml-declaration	org-html-xml-declaration

             ;; ; Markdown specific properties
             ;; :md-footnote-format	org-md-footnote-format
             ;; :md-footnotes-section	org-md-footnotes-section
             ;; :md-headline-style	org-md-headline-style

             ;; ; Other options
             :table-of-contents t
             ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
             )
            ;; static assets
            ("js"
             :base-directory "~/site/js/"
             :base-extension "js"
             :publishing-directory "~/site/public/js/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("conf"
             :base-directory "~/site/"
             :base-extension "js"
             :publishing-directory "~/site/public/"
             :recursive nil
             :publishing-function org-publish-attachment
             )
            ("css"
             :base-directory "~/site/css/"
             :base-extension "css"
             :publishing-directory "~/site/public/css/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("images"
             :base-directory "~/site/images/"
             :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
             :publishing-directory "~/site/public/images/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("assets"
             :base-directory "~/site/assets/"
             :base-extension "mp3"
             :publishing-directory "~/site/public/assets/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("webfonts"
             :base-directory "~/site/webfonts/"
             :base-extension "eot\\|svg\\|ttf\\|woff\\|woff2"
             :publishing-directory "~/site/public/webfonts/"
             :recursive t
             :publishing-function org-publish-attachment
             )

            ("website" :components ("conf" "orgfiles" "js" "css" "images" "assets" "webfonts"))
            ("statics" :components ("conf" "js" "css" "images" "assets" "webfonts"))
            ))))

(defun save-and-publish-website()
  "Save all buffers and publish."
  (interactive)
  (when (yes-or-no-p "Really save and publish current project?")
    (save-some-buffers t)
    (org-publish-project "website" t)
    (message "Site published done.")))

(defun save-and-publish-statics ()
  "Just copy statics like js, css, and image file .etc."
  (interactive)
  (org-publish-project "statics" t)
  (message "Copy statics done."))

(defun save-and-publish-file ()
  "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  (org-publish-current-file t))

(defun delete-org-and-html ()
  "Delete current org and the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete current org and the relative html?")

    (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (delete-file fileurl))
      (delete-file (buffer-file-name))
      (kill-this-buffer)
      (message "Delete org and the relative html done."))))

(defun just-delete-relative-html ()
  "Just delete the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete the relative html?")

    (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (progn
            (delete-file fileurl)
            (message "Delete the relative html done.")
            )
        (message "None relative html.")))))

(define-minor-mode auto-save-and-publish-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if auto-save-and-publish-file-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'save-and-publish-file :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'save-and-publish-file :local)))

(eval-after-load 'auto-save-and-publish-file-mode
  (add-hook 'org-mode-hook 'auto-save-and-publish-file-mode))

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root "~/site/public"))

(defun preview-current-buffer-in-browser ()
  "Open current buffer as html."
  (interactive)
  (let ((fileurl (concat "http://127.0.0.1:8080/" (file-name-base (buffer-name)) ".html")))
    (save-and-publish-file)
    (unless (httpd-running-p) (httpd-start))
    (browse-url fileurl)))

(provide 'setup-site)
;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-site.el ends here.
