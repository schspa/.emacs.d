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
  (custom-set-variables
   '(org-export-in-background t)
   '(org-html-htmlize-output-type 'inline-css)
   '(org-html-head-include-default-style nil)
   '(org-html-head-include-scripts nil)
   '(org-html-wrap-src-lines t)
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

(defcustom org-blogs-site-path
  (expand-file-name "site" user-emacs-directory)
  "Path to drawio.cli executable."
  :group 'org-babel
  :type 'string)

(defcustom org-blogs-output-path
  (expand-file-name "sites" (getenv "HOME"))
  "Path to drawio.cli executable."
  :group 'org-babel
  :type 'string)

(defun org-blogs-generate-post-path (post-filename)
  (concat org-blogs-output-path "/" (org-blogs-generate-post-link-path post-filename)))

(defun schspa/readlines (filePath)
  "Return a list for a file"
  (with-temp-buffer
	(insert-file-contents filePath)
	(split-string (buffer-string) "\n" t)))

(defcustom schspa/blog-source-dir "~/org/"
  "font-size in mm."
  :group 'schspa
  :type 'string)


(defun org-blog-get-out-dir (filename default)
  (let* ((is-external-file
          (string-prefix-p "../" (file-relative-name filename "~/org/blogs/")))
         (out-dir (if is-external-file
                      (org-blogs-generate-post-path filename) default)))
    out-dir))

(defun org-blog-get-out-path (filename default)
  (let (path)
    (setq path (org-blog-get-out-dir filename nil))
    (if path
        (concat (file-relative-name path org-blogs-output-path)
                (file-name-nondirectory filename)) default)))

(defun schspa/expandprops (props_alist &optional exclude-key)
  "Expand props in tags, category.."
  (seq-map (lambda (elt)
             (let* ((key (car elt))
                    (value (if (member key exclude-key) (cadr elt)
                             (if (stringp (cadr elt)) (split-string (cadr elt) ":" t)
                               (cadr elt)))))
               (cons key value))) props_alist))

(defun schspa/get-org-filetags (file)
  (with-temp-buffer
    (let ((delay-mode-hooks (org-mode)))
      (insert-file-contents file)
      (schspa/expandprops
       (org-collect-keywords
        '("category" "filetags" "title" "date")) '("TITLE" "DATE")))))

(defcustom org-posts-file-regexp "\\`[^.].*\\.org\\'"
  "Regular expression to match files for `org-agenda-files'.
If any element in the list in that variable contains a directory instead
of a normal file, all files in that directory that are matched by this
regular expression will be included."
  :group 'schspa
  :type 'regexp)

(defun schspa/getposts-info (filePath)
  "Return posts file information"
  (let ((my-posts ())
        (json-encoding-pretty-print t))
    (seq-filter
     (lambda (elt)
       (let* ((fileprops (schspa/get-org-filetags elt))
              (filetags (alist-get "FILETAGS" fileprops '() nil 'equal))
              (add-to-blogs (member "blogs" filetags)))
         (if add-to-blogs
             (progn
               (message "Add %S to blogs" elt)
               (setf (alist-get "URL" fileprops)
                     (concat "/" (file-name-sans-extension
                                  (org-blog-get-out-path elt elt))
                             ".html")
                     (alist-get :file-path fileprops) elt)
               (add-to-list
                'my-posts
                fileprops)))
         'add-to-blogs
         ))
     (directory-files-recursively filePath org-posts-file-regexp))
    (write-region
     (json-encode my-posts) nil
     (expand-file-name "blogs.json" org-blogs-output-path))
    my-posts))

(defun schspa/getposts (filePath)
  "Return file path sequence"
  (seq-map
   (lambda (elt)
     (alist-get :file-path elt))
   (schspa/getposts-info filePath)))

(defun schspa/parent-directory (dir)
  (if dir (unless (equal "/" dir)
			(file-name-directory (directory-file-name dir)))
	"."))

(defun schspa/copy-file (from to &optional is-image)
  "copy file and make directory automaticly"
  (make-directory (schspa/parent-directory to) t)
  (message "copy %s to %s is-image: %S" from to is-image)
  (if is-image (progn
                 (message "Copy image and add watermark")
                 (call-process-shell-command (format "convert -size 280x160 xc:none -fill grey -gravity NorthWest -draw \"text 10,10 'schspa.github.io'\" -gravity SouthEast -draw \"text 5,15 'schspa.github.io'\" miff:- | composite -tile - %s  %s" from to)))

    (copy-file from to t t)))

(defun schspa/org-html-link (orig-fun &rest args)
  "Copy an image file to website directory.

orig-func is the origin `org-html-link' function to export org links. args is
the arguments passed to `org-html-link'

Return output file name."
  (if (string-equal "file" (org-element-property :type (car args)))
      (let* ((raw-path (org-element-property :path (car args)))
             (is-org (string-suffix-p ".org" raw-path))
             (base-path)
             (output-path
              (if is-org (org-blog-get-out-path raw-path raw-path)
                (setq base-path (org-blogs-generate-post-link-path
                                 buffer-file-name))
                (concat "assets/" (file-name-nondirectory raw-path)))
              ))
        (if is-org
            (org-element-put-property (car args) :path output-path)
          (schspa/copy-file raw-path
                            (concat org-blogs-output-path
                                    "/" base-path "/" output-path)
                            (org-export-inline-image-p (car args)))
          (org-element-put-property (car args) :path  output-path))))
  (let ((exported (apply orig-fun args)))
    ;; Copy drawio file path to
    (if (string-equal "drawio" (org-element-property :type (car args)))
        (let* ((raw-path (org-element-property :path (car args)))
               (path-args (org-drawio-get-output-file-name-base raw-path))
               (png-path (cadr path-args))
               (site-path (concat "/assets/" (file-name-nondirectory png-path))))
          (message "linktype: %s site-path: %s" (org-element-property :type (car args)) site-path)
          (schspa/copy-file png-path (concat org-blogs-output-path site-path) t)
          (format "<img src=\"%s\">" site-path))
      exported)))

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
        (add-to-list 'files (concat org-blogs-site-path "/assets/postamble.html")))
      (add-to-list 'files (concat org-blogs-site-path "/assets/footer.html"))
      (dolist (postamblefile files) (insert-file-contents postamblefile)))
    (buffer-string)))

(defun org-blogs-preamble (info)
  (with-temp-buffer
    (insert-file-contents (concat org-blogs-site-path "/assets/preamble.html"))
    (buffer-string)))

(defun org-blogs-sitemap-entry (entry style project)
  "Default format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
	     (format "[[file:%s][%s]]"
		         entry
		         (org-publish-find-title entry project)))
	    ((eq style 'tree)
	     ;; Return only last subdir.
	     (file-name-nondirectory (directory-file-name entry)))
	    (t nil)))

(defun org-blogs-sitemap (title list)
  "Default site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n"
	      (org-list-to-org (remove (list nil) list))))

(defun org-publish-refresh-settings ()
  "Update site publish settings, This function will walk through
`schspa/blog-source-dir' and search all org files with filetags \"blogs\""
  (interactive)
  (setq org-publish-project-alist
        `(("orgfiles"
           ;; ; Sources and destinations for files.
           :base-directory "~/org/blogs/"  ;; local dir
           :publishing-directory ,org-blogs-output-path
           ;; :publishing-directory "/ssh:jack@192.112.245.112:~/site/public/"

           :auto-sitemap t
           :sitemap-function org-blogs-sitemap
           :sitemap-sort-folders ,'first
           :sitemap-style ,'list
           :sitemap-format-entry org-blogs-sitemap-entry

           ;; :preparation-function
           ;; :complete-function

           ;; ; Selecting files
           :base-extension "org"
           ;; :exclude "PrivatePage.org"     ;; regexp
           :include ,(schspa/getposts schspa/blog-source-dir)
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
           :html-head	,(org-file-contents (concat org-blogs-site-path "/assets/header.html"))
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
           :html-preamble org-blogs-preamble ;; org-html-preamble
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
           :base-directory ,(concat org-blogs-site-path "/js/")
           :base-extension "js"
           :publishing-directory ,(concat org-blogs-output-path "/js/")
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("conf"
           :base-directory ,org-blogs-site-path
           :base-extension "js"
           :publishing-directory ,org-blogs-output-path
           :recursive nil
           :publishing-function org-publish-attachment
           )
          ("css"
           :base-directory ,(concat org-blogs-site-path "/css/")
           :base-extension "css"
           :publishing-directory ,(concat org-blogs-output-path "/css/")
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("images"
           :base-directory ,(concat org-blogs-site-path "/images/")
           :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
           :publishing-directory ,(concat org-blogs-output-path "/images/")
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("assets"
           :base-directory ,(concat org-blogs-site-path "/assets/")
           :base-extension "mp3"
           :publishing-directory ,(concat org-blogs-output-path "/assets/")
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("webfonts"
           :base-directory ,(concat org-blogs-site-path "/webfonts/")
           :base-extension "eot\\|svg\\|ttf\\|woff\\|woff2"
           :publishing-directory ,(concat org-blogs-output-path "/webfonts/")
           :recursive t
           :publishing-function org-publish-attachment
           )

          ("website" :components ("conf" "orgfiles" "js" "css" "images" "assets" "webfonts"))
          ("statics" :components ("conf" "js" "css" "images" "assets" "webfonts")))))

(custom-set-variables
 '(org-publish-timestamp-directory
   (convert-standard-filename
    (expand-file-name ".org-timestamps/" org-blogs-output-path))))

(eval-after-load 'ox-publish
  ;; org-publish-project-alist
  ;; ("project-name" :property value :property value ...)
  ;; ("project-name" :components ("project-name" "project-name" ...))
  '(call-interactively #'org-publish-refresh-settings))

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

    (let ((fileurl (concat org-blogs-site-path "/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (delete-file fileurl))
      (delete-file (buffer-file-name))
      (kill-this-buffer)
      (message "Delete org and the relative html done."))))

(defun just-delete-relative-html ()
  "Just delete the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete the relative html?")

    (let ((fileurl (concat org-blogs-output-path "/" (file-name-base (buffer-name)) ".html")))
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
  (setq httpd-root org-blogs-output-path))

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
