;; Take the coding system setup before pyim setup.
;; set-language-environment utf-8 will set default input method to rfc1345
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

(eval-after-load 'rst' (define-key rst-mode-map (kbd "C-=") nil))

;; add english helper
(require 'company-english-helper)
(use-package youdao-dictionary
  :ensure t
  :init
  (define-prefix-command 'youdao-prefix-map)
  (global-set-key (kbd "C-c y") 'youdao-prefix-map)
  :config
  :bind
  (("C-c y s" . youdao-dictionary-search-at-point-tooltip)
   ("C-c y S" . youdao-dictionary-search-at-point)
   ("C-c y p" . youdao-dictionary-play-voice-at-point)))

(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command (expand-file-name "bin/LanguageTool-5.6/languagetool-commandline.jar" user-emacs-directory)
        languagetool-server-command (expand-file-name "bin/LanguageTool-5.6/languagetool-server.jar"
                                                      user-emacs-directory)))
(use-package flycheck-languagetool
  :ensure t
  :hook ((text-mode . flycheck-languagetool-setup)
         (latex-mode . flycheck-languagetool-setup)
         (org-mode . flycheck-languagetool-setup)
         (markdown-mode . flycheck-languagetool-setup))
  :init
  (setq flycheck-languagetool-server-jar
        (expand-file-name "bin/LanguageTool-5.6/languagetool-server.jar"
                          user-emacs-directory)))

(use-package rime
  :quelpa (rime :fetcher github
                :repo "DogLooksGood/emacs-rime"
                :files ("*.el" "Makefile" "lib.c"))
  :if (getenv "SSH_CONNECTION")
  :custom
  (rime-show-candidate 'posframe)
  (default-input-method "rime")
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding)
        ("C-\\" . 'rime-inline-ascii)))

;;(setq use-package-ensure-function 'quelpa)
(quelpa-use-package-activate-advice)

(use-package sis
  :quelpa
  (sis :fetcher github
       :repo "laishulu/emacs-smart-input-source")
  :if (not (getenv "SSH_CONNECTION"))
  :config
  (cond (sys/macp (sis-ism-lazyman-config
                   "com.apple.keylayout.ABC"
                   "im.rime.inputmethod.Squirrel.Rime"))
        ((string-equal (getenv "GTK_IM_MODULE") "fcitx")
         (sis-ism-lazyman-config "1" "2" 'fcitx5))
        ((t (sis-ism-lazyman-config "xkb:us::eng" "rime" 'ibus))))

  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  (sis-global-respect-mode t)
  (sis-global-inline-mode t)
  :bind (("C-\\" . sis-switch)))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (yas-global-mode 1))

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

;; GROUP: Editing -> Editing Basics
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))


(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package browse-kill-ring
  :ensure t
  :config
  (global-set-key (kbd "C-c k") 'browse-kill-ring))

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

;; Package: volatile-highlights
;; GROUP: Editing -> Volatile Highlights
(use-package volatile-highlights
  :init
  (volatile-highlights-mode t))

;; Package: undo-tree
;; GROUP: Editing -> Undo -> Undo Tree
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))


;; Package: yasnippet
;; GROUP: Editing -> Yasnippet
;; Package: yasnippet
(use-package yasnippet
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;; Package: clean-aindent-mode
(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; Package: dtrt-indent
(use-package dtrt-indent
  :init
  (dtrt-indent-mode t)
  (dtrt-indent-global-mode t)
  (setq dtrt-indent-verbosity 0))

;; Package: ws-butler
(use-package ws-butler
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode))

;; PACKAGE: comment-dwim-2
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2))
  )

;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
(use-package anzu
  :init
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

;; PACKAGE: iedit
(use-package iedit
  :bind (("C-;" . iedit-mode))
  :init
  (setq iedit-toggle-key-default nil))

;; Customized functions
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode yank-indent-blacklisted-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; add duplicate line function from Prelude
;; taken from prelude-core.el
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(setq select-enable-primary nil)
(setq select-enable-clipboard t)

;; company
(use-package company
  :init
  (global-company-mode 1)
  :config
  (setq-default company-dabbrev-ignore-case nil))

(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (progn
          (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start)))))
  :config (setq edit-server-url-major-mode-alist
                (list '("stackexchange" . markdown-mode)
                      '("github.com" . markdown-mode)
                      '("emacs-china.org" . markdown-mode))))

(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        (cond
         ((string-equal system-type "darwin")
          '(("\\.\\(dmg\\|doc\\|docs\\|xls\\|xlsx\\)$"
             "open" (file))
            ("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "open" ("-a" "VLC" file))))
         ((string-equal system-type "gnu/linux")
          '(("\\.\\(mp4\\|mp3\\|webm\\|avi\\|flv\\|mov\\)$"
             "xdg-open" (file))))))
  (add-to-list 'openwith-associations
               '("\\.\\(drawio\\)$"
                 "draw.io" (file)))
  (openwith-mode +1))

(provide 'setup-editing)
