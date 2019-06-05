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

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-directory-alist (quote (("." . "~/.backups"))))

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)


(defalias 'yes-or-no-p 'y-or-n-p)

(global-visual-line-mode t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

;; add english helper
(require 'company-english-helper)
(use-package youdao-dictionary
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

(autoload 'yas-expand-snippet "yasnippet")
(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :ensure t
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)

  (setq auto-insert-directory (locate-user-emacs-file "auto-insert"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)

  :config
  (define-auto-insert "\\.el$" ["el-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.erl$" ["erl-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.hrl$" ["hrl-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.c$"  ["c-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.cpp$" ["c++-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.cc$" ["c++-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.cs$" ["csharp-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.org$" ["org-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.tex$" ["tex-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.py$" ["py-auto-insert" autoinsert-yas-expand])
  (define-auto-insert "\\.go$" ["go-auto-insert" autoinsert-yas-expand]))

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; auto adjust font according to screen dpi
;; frome https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(defun my-dpi ()
  (let* ((attrs (car (display-monitor-attributes-list)))
         (size (assoc 'mm-size attrs))
         (sizex (cadr size))
         (res (cdr (assoc 'geometry attrs)))
         (resx (- (caddr res) (car res)))
         dpi)
    (catch 'exit
      ;; in terminal
      (unless sizex
        (throw 'exit 10))
      ;; on big screen
      (when (> sizex 1000)
        (throw 'exit 10))
      ;; DPI
      (* (/ (float resx) sizex) 25.4))))

(defun my-preferred-font-size ()
  (let ( (dpi (my-dpi)) )
    (cond
     ((< dpi 110) 10)
     ((< dpi 130) 11)
     ((< dpi 160) 12)
     (t 12))))

(defvar my-preferred-font-size (my-preferred-font-size))

(defvar my-regular-font
  (cond
   ((eq window-system 'x) (format "DejaVu Sans Mono-%d:weight=normal" my-preferred-font-size))
   ((eq window-system 'w32) (format "Courier New-%d:antialias=none" my-preferred-font-size))))
(defvar my-symbol-font
  (cond
   ((eq window-system 'x) (format "DejaVu Sans Mono-%d:weight=normal" my-preferred-font-size))
   ((eq window-system 'w32) (format "DejaVu Sans Mono-%d:antialias=none" my-preferred-font-size))))

(cond
 ((eq window-system 'x)
  (if (and (fboundp 'find-font) (find-font (font-spec :name my-regular-font)))
      (set-frame-font my-regular-font)
    (set-frame-font "DejaVu Sans Mono")))
 ((eq window-system 'w32)
  (set-frame-font my-regular-font)
  (set-fontset-font nil 'cyrillic my-regular-font)
  (set-fontset-font nil 'greek my-regular-font)
  (set-fontset-font nil 'phonetic my-regular-font)
  (set-fontset-font nil 'symbol my-symbol-font)))


;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)
(setq jit-lock-defer-time 0.05)
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

;;export http_proxy="http://127.0.0.1:12333"
;;export https_proxy="http://127.0.0.1:12333"

(defun toggle-env-http-proxy ()
  "set/unset the environment variable http_proxy which w3m uses"
  (interactive)
  (let ((proxy "http://127.0.0.1:12333"))
    (if (string= (getenv "http_proxy") proxy)
        ;; clear the proxy
        (progn
          (setenv "http_proxy" "")
          (message "env http_proxy is empty now"))
      ;; set the proxy
      (setenv "http_proxy" proxy)
      (message "env http_proxy is %s now" proxy))))

(use-package w3m
  :ensure t)
;; ;; Compilation
;; (global-set-key (kbd "<f5>") (lambda ()
;;                                (interactive)
;;                                (setq-local compilation-read-command nil)
;;                                (call-interactively 'compile)))

;; setup GDB
(setq-default
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

(setq select-enable-primary t)
(setq select-enable-clipboard t)

;; company
(use-package company
  :init
  (global-company-mode 1)
  (delete 'company-semantic company-backends)
  :config
  (setq-default company-dabbrev-ignore-case nil))

;; Package zygospore
(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(windmove-default-keybindings)

(require 'recentf)
(recentf-mode t)
(setq-default recentf-max-menu-item 20)

(use-package dracula-theme
  :ensure t
  :config
  :init
  (add-hook 'after-init-hook
			'(lambda ()
			   (load-theme 'dracula t))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(set-face-attribute 'default nil :height 120)
(when (memq window-system '(mac ns))
  (set-face-attribute 'default nil :height 140))

(if (file-directory-p "~/Dropbox/org") (setq org-agenda-files '("~/Dropbox/org")))

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

(global-hl-line-mode t)
(global-auto-revert-mode t)

;; aggressive-indent
(use-package aggressive-indent
  :ensure t
  :init)

(when (>= emacs-major-version 26)
  (global-display-line-numbers-mode t))

(add-hook 'prog-mode-hook
          '(lambda ()
             (smartparens-mode t)
             (define-key global-map (kbd "M-r") 'helm-gtags-find-rtag)
             (define-key global-map (kbd "M-t") 'helm-dwim-target)
             (define-key global-map (kbd "M-i") 'helm-imenu)
             (when (< emacs-major-version 26)
               (line-number-mode))
             (show-paren-mode t)))

;; function-args
(use-package function-args
  :ensure t
  :init
  ())

;; (setq sml/theme 'smart-mode-line-powerline)
;; (add-hook 'after-init-hook 'sml/setup)

(use-package flycheck
  :ensure t
  :init
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode t))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; for occur
(defun occur-dwim ()
  "Call `occur' with a sane default." (interactive)
  (push (if (region-active-p)
			(buffer-substring-no-properties (region-beginning)
											(region-end))
		  (let ((sym (thing-at-point 'symbol)))
			(when (stringp sym)
			  (regexp-quote sym))))
		regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

(defun hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

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

(use-package popwin
  :ensure t
  :init
  :config
  (popwin-mode 1))

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
