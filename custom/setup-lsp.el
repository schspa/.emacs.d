;;; setup-lsp.el --- lsp

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

(use-package ccls
  :ensure t)
(use-package lsp-java
  :ensure t)

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)    ; 我習慣自動選project root
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-file-watchers nil)
  ;; (setq lsp-prefer-flymake t)  ; 預設t。flymake替代flycheck
  :config
  (require 'ccls)
  (require 'lsp-clients)          ; ocaml,css,python,bash,...
  (require 'lsp-java)
  :hook
  (hack-local-variables . (lambda ()
                            (when (derived-mode-p
                                   'c-mode
                                   'c++-mode
                                   'objc-mode
                                   'python-mode
                                   'java-mode)
                              (lsp)))))


(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :config
  :hook (lsp-mode . lsp-ui-mode))

;;pip install python-language-server
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

(provide 'setup-lsp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-lsp.el ends here.
