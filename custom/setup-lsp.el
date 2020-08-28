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
  :ensure t
  :config
  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq
   ccls-initialization-options
   `(:clang
     (:excludeArgs
      ;; Linux's gcc options. See ccls/wiki
      ["--param=allow-store-data-races=0" "-W*" "-f*" "-m*"]
      :extraArgs ["--gcc-toolchain=/usr"])
     :completion
     (:include
      (:blacklist
       ["^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
        "^/usr/(local/)?include/c\\+\\+/v1/"
        ]))))
  )
(use-package lsp-java
  :ensure t)

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred
(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)    ; 我習慣自動選project root
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-file-watchers nil)
  ;; (setq lsp-prefer-flymake t)  ; 預設t。flymake替代flycheck
  :hook
  (hack-local-variables . (lambda ()
							(cond ((derived-mode-p 'c-mode 'c++-mode 'objc-mode)
								   (progn
									 (require 'ccls)
									 (lsp)))
								  ((derived-mode-p 'python-mode)
								   (progn
									 (require 'lsp-python-ms)
									 (lsp)))
								  ((derived-mode-p 'java-mode)
								   (progn
									 (require 'lsp-java)
									 (lsp)))
								  ))))
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

;; dap mode
(use-package dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(provide 'setup-lsp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-lsp.el ends here.
