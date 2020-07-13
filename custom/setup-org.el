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

(provide 'setup-org)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-org.el ends here.
