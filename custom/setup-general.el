;;; setup-general.el --- Initialization file for Emacs

;;; Commentary: Emacs Startup File --- initialization for Emacs
;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 29 Nov 2017
;; Version: 2.4
;; Package-Requires: ((emacs "24.3") (bind-key "2.4"))
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/jwiegley/use-package

;; general setup, theme, global operations etc.

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-directory-alist (quote (("." . "~/.backups"))))
(global-auto-revert-mode t)

(setq gc-cons-threshold 100000000)
(setq jit-lock-defer-time 0.05)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

;; dired 自动刷新
(setq dired-auto-revert-buffer t)

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 5 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (local-set-key (kbd "C-s") 'counsel-grep-or-swiper)
    (local-set-key (kbd "C-r") 'counsel-grep-or-swiper)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; Package zygospore
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

(require 'recentf)
(recentf-mode t)
(setq-default recentf-max-menu-item 200)

(if (file-directory-p "~/org") (setq org-agenda-files '("~/org")))

(global-set-key (kbd "C-c a") 'org-agenda)

(require 'tramp)
(setq tramp-terminal-type "tramp")
(setq tramp-default-method "ssh")
(setq tramp-chunksize 100)

(define-key global-map (kbd "C-c t") 'helm-tramp)

(use-package sr-speedbar
  :ensure t
  :init
  (global-set-key (kbd "s-s") 'sr-speedbar-toggle))

;; hlt-hlight
(use-package highlight
  :ensure t
  :init
  (global-set-key [f8] 'hlt-highlight-symbol)
  (global-set-key [f9] 'hlt-unhighlight-symbol))

;; aggressive-indent
(use-package aggressive-indent
  :ensure t
  :init)

;; function-args
(use-package function-args
  :ensure t
  :init
  ())

;; (setq sml/theme 'smart-mode-line-powerline)
;; (add-hook 'after-init-hook 'sml/setup)

(defvar scratch-run-alist
  '(("java"   . java-mode)
    ("c++"    . c++-mode)
    ("perl"   . perl-mode)
    ("python" . python-mode)
    ("js"     . javascript-mode)
    ("j"      . j-mode)
    ("tcl"    . tcl-mode))
  "生成草稿buffer的简短mode名称列表")
(defun scratch-run ()
  "Run a scratch"
  (interactive)
  (let ((mode (ido-completing-read
               "What kind of scratch mode ?:"
               (append (all-completions ""
                                        obarray
                                        (lambda (s)
                                          (and (fboundp s)
                                               (string-match "-mode$" (symbol-name s)))))
                       (mapcar 'car scratch-run-alist)))))
    (pop-to-buffer (get-buffer-create (format "* scratch * %s *" mode)))
    (funcall (if (assoc mode scratch-run-alist)
                 (cdr (assoc mode scratch-run-alist))
               (intern mode)))
    ))

(global-set-key [remap other-window] 'ace-window)

(use-package which-key
  :ensure t
  :init
  (which-key-mode t))

(require 's)
(defun retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  " set theUrl to get URL of active tab of first window\n"
                  " set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))


(provide 'setup-general)
