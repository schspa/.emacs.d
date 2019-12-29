(when (>= emacs-major-version 24)
  (require 'package)
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
					  (not (gnutls-available-p))))
		 (proto (if no-ssl "http" "https")))
	(when no-ssl
	  (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
	;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
	(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
	;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
	(when (< emacs-major-version 24)
	  ;; For important compatibility libraries like cl-lib
	  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
  (package-initialize))

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

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)

;; Find Executable Path on OS X
(when (featurep 'cocoa)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq-default use-package-always-ensure t)

(setq custom-file "~/.emacs-custom.el")
(if (file-exists-p custom-file) (load custom-file))

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'use-package)

(require 'init-const)
(require 'setup-ui)
(require 'setup-modeline)
(require 'setup-helm)
(require 'setup-general)

(require 'setup-prog)
(require 'setup-md)
(require 'setup-windows)
(require 'setup-abbrev)
(require 'setup-helm-gtags)
(require 'setup-cedet)
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
(require 'setup-blog)

