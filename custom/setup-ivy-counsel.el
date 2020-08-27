(use-package ivy
  :diminish ivy-mode
  :init
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  :hook (after-init . ivy-mode)
  :bind (("C-c h o" . counsel-grep-or-swiper)
         ("C-c s" . swiper-all-thing-at-point)))

(use-package counsel
  :after ivy
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-c r" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h l" . counsel-load-library)
   ("M-i" . counsel-imenu))
  (:map counsel-find-file-map ("C-l" . counsel-up-directory))
  :config
  ;; reference to https://github.com/seagle0128/.emacs.d/blob/1398ae9339dd03f67dca836a80e25b91eae2f8d5/lisp/init-ivy.el
  ;; Pre-fill search keywords
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  (defvar my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines keep-lines ivy-read
      swiper swiper-backward swiper-all
      swiper-isearch swiper-isearch-backward
      lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
      counsel-grep-or-swiper counsel-grep-or-swiper-backward
      counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))
  (defvar-local my-ivy-fly--travel nil)

  (defun my-ivy-fly-back-to-present ()
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command '(self-insert-command
                                    ivy-forward-char
                                    ivy-delete-char delete-forward-char
                                    end-of-line mwim-end-of-line
                                    mwim-end-of-code-or-line mwim-end-of-line-or-code
                                    yank ivy-yank-word counsel-yank-pop))
               (equal (this-command-keys-vector) (kbd "M-n")))
           (unless my-ivy-fly--travel
             (delete-region (point) (point-max))
             (when (memq this-command '(ivy-forward-char
                                        ivy-delete-char delete-forward-char
                                        end-of-line mwim-end-of-line
                                        mwim-end-of-code-or-line
                                        mwim-end-of-line-or-code))
               (insert (ivy-cleanup-string ivy-text))
               (when (memq this-command '(ivy-delete-char delete-forward-char))
                 (beginning-of-line)))
             (setq my-ivy-fly--travel t)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion
            (insert (propertize (replace-regexp-in-string
                                 "\\\\_<" ""
                                 (replace-regexp-in-string
                                  "\\\\_>" ""
                                  future))
                                'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t))))

(use-package ivy-xref
  :ensure t
  :after ivy
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package counsel-projectile
  :after counsel
  :init
  (counsel-projectile-mode))

(use-package ivy-posframe
  :after counsel ivy
  :init
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (require 'ivy-posframe)
  (ivy-posframe-mode 1))

(use-package ivy-rich
  :ensure t
  :config
  (setq ivy-rich-path-style 'abbrev
        ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 20))
            (ivy-rich-switch-buffer-size (:width 7 :align right))
            (ivy-rich-switch-buffer-indicators
             (:width 2 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 8 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))))

  (ivy-rich-mode 1))

(el-get-bundle counsel-woman
  :url "https://raw.githubusercontent.com/yangyingchao/tubo-env/master/.emacs.d/site-lisp/counsel%2B/counsel-woman.el"
  (require 'counsel-woman)
  (define-key global-map (kbd "C-c h m") 'counsel-woman))

(provide 'setup-ivy-counsel)
