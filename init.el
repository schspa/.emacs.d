(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
						   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                           ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
  (package-initialize))

;; cl - Common Lisp Extension
(require 'cl)

(setq package-selected-packages
      '(
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
        restart-emacs
        ))

(defun my/packages-installed-p ()
  (loop for pkg in package-selected-packages
	    when (not (package-installed-p pkg)) do (return nil)
	    finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (call-interactively #'package-install-selected-packages))

;; Find Executable Path on OS X
(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (setq mac-option-modifier 'hyper)
  (setq mac-command-modifier 'meta))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq-default use-package-always-ensure t)

(defvar my-cache-dir (expand-file-name ".cache" user-emacs-directory)
  "My cache directory")

(use-package quelpa
  :ensure t
  :init
  (ignore-errors (make-directory (expand-file-name "quelpa" my-cache-dir)))
  :custom
  (quelpa-dir (expand-file-name "quelpa" my-cache-dir))
  (quelpa-update-melpa-p nil)
  (quelpa-self-upgrade-p nil))

(use-package quelpa-use-package
  :ensure t
  :after quelpa
  :config
  (quelpa-use-package-activate-advice))

(use-package el-get
  :ensure t
  :custom
  (el-get-dir (expand-file-name "el-get" my-cache-dir)))

(setq custom-file "~/.emacs-custom.el")
(if (file-exists-p custom-file) (load custom-file))

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'use-package)

(require 'server)
(let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir))
	   (server-file (expand-file-name server-name server-dir)))
  (message "server server-file: %s" server-file)
  (unless (file-exists-p server-file)
    (server-start)))

(require 'init-const)
(require 'setup-ui)
(require 'setup-modeline)
(require 'setup-general)
(require 'setup-ivy-counsel)
(require 'setup-projectile)
(require 'setup-prog)
(require 'setup-md)
(require 'setup-windows)
(require 'setup-abbrev)
(require 'setup-editing)
(require 'setup-lsp)
(require 'setup-c)
(require 'setup-python)
(require 'setup-bugreport)
(require 'setup-elisp)
(require 'setup-eaf)
(require 'setup-org)
(require 'setup-pdf)
(require 'setup-wsl)
(require 'setup-site)
(require 'setup-telegram)
(require 'setup-mail)
(require 'setup-pdms)
(require 'setup-vterm)
(put 'magit-clean 'disabled nil)
