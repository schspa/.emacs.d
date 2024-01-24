(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
						   ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

;; cl - Common Lisp Extension
(require 'cl-lib)

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
        dracula-theme
        android-mode
        anaconda-mode
        company-c-headers
        restart-emacs
        ))

(defun my/packages-installed-p ()
  (cl-loop for pkg in package-selected-packages
	       when (not (package-installed-p pkg)) do (cl-return nil)
	       finally (cl-return t)))

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

;; Link: https://github.com/SystemCrafters/crafted-emacs/blob/master/early-init.el#L58
;;
;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name "var/eln-cache/" my-cache-dir)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/eln-cache/" my-cache-dir)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" my-cache-dir)))


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
(require 'setup-security)
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
(require 'setup-ctags)
(require 'setup-python)
(require 'setup-bugreport)
(require 'setup-elisp)
(require 'setup-eaf)
(require 'setup-site)
(require 'setup-org)
(require 'setup-pdf)
(require 'setup-wsl)
(require 'setup-telegram)
(require 'setup-mail)
(require 'setup-pdms)
(require 'setup-vterm)
(require 'setup-net)
(require 'org-drawio)
(require 'org-srclink)
(require 'org-video)

(org-srclink-set-parameters
 "atf"
 :localpath (expand-file-name "~/work/src/arm-trusted-firmware")
 :html-link-fmt "<a target=\"_blank\" href=\"https://github.com/ARM-software/arm-trusted-firmware/blob/%s/%s#L%s\">%s</a>")

(org-srclink-set-parameters
 "kernel"
 :localpath (expand-file-name "~/work/src/linux")
 :html-link-fmt "<a target=\"_blank\" href=\"https://github.com/torvalds/linux/blob/%s/%s#L%s\">%s</a>")

(org-srclink-set-parameters
 "optee-os"
 :localpath (expand-file-name "~/work/src/optee/optee_os")
 :html-link-fmt "<a target=\"_blank\" href=\"https://github.com/OP-TEE/optee_os//blob/%s/%s#L%s\">%s</a>")


(put 'magit-clean 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'magit-diff-edit-hunk-commit 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
