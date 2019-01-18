(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		                   ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		              ;; --- Auto-completion ---
		              company
		              ;; --- Better Editor ---
		              hungry-delete
		              swiper
		              counsel
		              smartparens
		              ;; --- Major Mode ---
		              js2-mode
		              ;; --- Minor Mode ---
		              nodejs-repl
		              exec-path-from-shell
		              ;; --- Themes ---
		              ;; solarized-theme
                      ;; smart-mode-line
                      validate
                      dracula-theme
					  android-mode
		              anaconda-mode
					  company-c-headers
					  ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
	    when (not (package-installed-p pkg)) do (return nil)
	    finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
	  (package-install pkg))))

;; Find Executable Path on OS X
(when (featurep 'cocoa)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
                                        ; (require 'use-package)
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq-default use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'use-package)

(use-package ccls
  :ensure t)
(use-package lsp-java
  :ensure t)

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)    ; 我習慣自動選project root
  (setq lsp-enable-indentation nil)
  ;; (setq lsp-prefer-flymake t)  ; 預設t。flymake替代flycheck
  :config
  (require 'ccls)
  (require 'lsp-clients)          ; ocaml,css,python,bash,...
  (require 'lsp-java)
  :hook ((c-mode c++-mode objc-mode python-mode java-mode) . (lambda () (lsp)))
  )

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

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-c)
(require 'setup-python)
(require 'setup-bugreport)
(require 'setup-elisp)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)


