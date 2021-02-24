;;; setup-org.el --- Setup org mode

;; Author: SH147NDT2F  schspa@SH147NDT2F
;; Keywords: elisp
;; URL:

;; Copyright (C) 2019, SH147NDT2F, all rights reserved.
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

(require 'org)
(defun schspa/org-confirm-babel-evaluate (lang body)
  ;; org babel evaluate whitelist
  (let* ((whitelist-languages '("latex" "dot" "mermaid" "ditaa" "drawio" "plantuml")))
	(not (member lang whitelist-languages))))

(setq org-confirm-babel-evaluate 'schspa/org-confirm-babel-evaluate)

;; from https://emacs.stackexchange.com/questions/12841/quickly-insert-source-blocks-in-org-mode
(defvar org-sai-src-default "C++"
  "This is the list used to store the default label for source code section.")
(setq org-image-actual-width '(600))
(defun org-insert-src-block ()
  "Insert the source code section in `org-mode'."
  (interactive)
  (let* ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite"))
         (src-prompt-str
          (concat "Source code type (default "
                  org-sai-src-default
                  "): "))
         (temp-src-code-types
          (cons org-sai-src-default src-code-types))
         (src-type-str
          (completing-read src-prompt-str temp-src-code-types
                           nil nil nil nil org-sai-src-default)))
    (setq org-sai-src-default src-type-str))
  (insert (format "#+BEGIN_SRC %s\n" src-type-str))
  (newline)
  (org-indent-line)
  (insert "#+END_SRC\n")
  (forward-line -2))

(add-hook 'org-mode-hook
          (lambda ()
            ;; keybinding for inserting code blocks
            (local-set-key (kbd "C-c s") 'org-insert-src-block)))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path
		(expand-file-name "bin/plantuml.jar" user-emacs-directory)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (latex . t)
   (shell . t)
   ;;(mermaid . t)
   (plantuml . t)
   (ditaa . t)
   (drawio . t)
   ))

(require 'ob-drawio)

(use-package htmlize
  :ensure t
  :demand)

(setq org-html-htmlize-output-type 'css)

(use-package mermaid-mode
  :ensure t
  :demand)

(use-package ob-mermaid
  :ensure t
  :demand)

;; FIXME: workaround
;; https://github.com/syl20bnr/spacemacs/issues/11798
(when (version<= "9.2" (org-version))
  (require 'org-tempo))

(use-package org-download
  :ensure t
  :config
  (setq-default org-download-image-dir "~/Sync/org/pic")
  (setq-default org-download-display-inline-images t)
  (setq-default org-download-heading-lvl nil)
  (if (equal system-type 'darwin)
      (setq org-download-screenshot-method "/usr/sbin/screencapture -i %s"))
  :hook
  ((dired-mode . org-download-enable)
   (org-mode . org-download-enable)))

(let ((font "Sarasa Mono SC"))
  (when (member font (font-family-list))
    (set-face-attribute 'org-table nil :family "Sarasa Mono SC")))

;; GTD
(use-package org-gtd
  :ensure t
  :after org
  :demand t ;; without this, the package won't be loaded, so org-agenda won't be configured
  :custom
  ;; where org-gtd will put its files. This value is also the default one.
  (org-gtd-directory "~/org/gtd/")
  ;; package: https://github.com/Malabarba/org-agenda-property
  ;; this is so you can see who an item was delegated to in the agenda
  (org-agenda-property-list '("DELEGATED_TO"))
  ;; I think this makes the agenda easier to read
  (org-agenda-property-position 'next-line)
  ;; package: https://www.nongnu.org/org-edna-el/
  ;; org-edna is used to make sure that when a project task gets DONE,
  ;; the next TODO is automatically changed to NEXT.
  (org-edna-use-inheritance t)
  (org-edna-load)
  :bind
  (("C-c a" . org-agenda)
   ("C-c d c" . org-gtd-capture) ;; add item to inbox
   ("C-c d a" . org-agenda-list) ;; see what's on your plate today
   ("C-c d p" . org-gtd-process-inbox) ;; process entire inbox
   ("C-c d n" . org-gtd-show-all-next) ;; see all NEXT items
   ("C-c d s" . org-gtd-show-stuck-projects)) ;; see projects that don't have a NEXT item
  :init
  (bind-key "C-c c" 'org-gtd-clarify-finalize))

(use-package org-agenda
  :ensure nil ;; this is how you tell use-package to manage a sub-package
  :after org-gtd ;; because we need to add the org-gtd directory to the agenda files
  :custom
  ;; use as-is if you don't have an existing org-agenda setup
  ;; otherwise push the directory to the existing list
  (org-agenda-files `(,org-gtd-directory))
  ;; a useful view to see what can be accomplished today
  (org-agenda-custom-commands '(("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT"))))))

(use-package org-capture
  :ensure nil
  ;; note that org-gtd has to be loaded before this
  :after org-gtd
  :config
  ;; use as-is if you don't have an existing set of org-capture templates
  ;; otherwise add to existing setup
  ;; you can of course change the letters, too
  (setq org-capture-templates `(("i" "Inbox"
                                 entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t)
                                ("l" "Todo with link"
                                 entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
                                 "* %?\n%U\n\n  %i\n  %a"
                                 :kill-buffer t))))

;; latex settings
(setq-default org-preview-latex-image-directory (concat my-cache-dir "/org-latex"))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

;; setup icalendar
;; advice to org-icalendar--valarm
;; we need entry argument, so this function is a goog choice.
(defun org-gdt-icalendar--valarm (orig-fun &rest args)
  "Add needed URL entry for icalendar"
  (let ((valarms (apply orig-fun args)))
    (concat
     "URL:org-protocol://gdt?id=" (org-element-property :ID (car args)) "\n"
     (if valarms valarms ""))))

(eval-after-load 'ox-icalendar
  (progn
    (setq org-icalendar-include-todo t
          org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due event-if-todo-not-done)
          org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start event-if-todo-not-done)
          org-icalendar-with-timestamps t)
    (advice-add 'org-icalendar--valarm :around #'org-gdt-icalendar--valarm)))

(setenv "GPG_AGENT_INFO" nil)
(require 'password-cache)
(require 'epa-file)
(epa-file-enable)
(setq password-cache-expiry (* 15 60))
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq epa-pinentry-mode 'loopback)
(setq auth-source-debug nil)

;; org-caldav
(use-package org-caldav
  :ensure t
  :config
  (unless (file-exists-p (concat my-cache-dir "/org-caldav/"))
    (make-directory (concat my-cache-dir "/org-caldav/")))
  (setq
   org-caldav-save-directory (concat my-cache-dir "/org-caldav/")
   org-caldav-backup-file (concat my-cache-dir "/org-caldav-backup.org")
   org-caldav-show-sync-results 'nil
   org-caldav-files (mapcar (lambda (x)
                              (concat org-directory x))
                            '("gtd/tasks.org")))
  (setq org-caldav-inbox (concat org-directory "gtd/inbox.org")))

(require 'org-protocol)

(defun org-gdt-protocol-open (info)
  "This handler simply opens the file with emacsclient.
INFO is an alist containing additional information passed by the protocol URL.
It should contain the id key, pointing to the ID property for a org file to open.
  Example protocol string:
org-protocol://gdt?id=CBEC8DD1-7814-44A7-AA3D-97AEC35B6DB7"
  (when-let ((id (plist-get info :id)))
    (raise-frame)
    (org-open-link-from-string (format "id:%s" id)))
  nil)

(push '("org-gdt-ref"  :protocol "gdt"   :function org-gdt-protocol-open)
      org-protocol-protocol-alist)

(setq org-protocol-project-alist
      '(("http://schspa.tk/"
         :base-url "http://schspa.tk/"
         :working-directory "/Users/schspa/org/blogs/"
         :online-suffix ".html"
         :working-suffix ".org"
         :rewrites (("org/?$" . "index.php")))
        ("org"
         :base-url "org/"
         :working-directory "~/org/")))

(use-package ox-gfm
  :ensure t
  :config
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

(use-package ox-json
  :ensure t
  :config
  (eval-after-load "org"
    '(require 'ox-json nil t)))

;; Presentations
(use-package org-re-reveal
  :ensure t
  :custom
  (org-re-reveal-revealjs-version "3.7.0")
  (org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.7.0"))
;;set agenda files
(provide 'setup-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-org.el ends here.
