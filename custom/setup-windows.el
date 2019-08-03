;;; setup-windows.el --- set window related function

;; Author:   schspa@Arch-Schspa-laptop
;; Keywords: elisp
;; URL:

;; Copyright (C) 2019, , all rights reserved.
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

;; switch windows with shift + direction key
(windmove-default-keybindings)

(use-package hydra
  :ensure t)

;; Quickly switch windows
(use-package ace-window
  :functions hydra-frame-window/body
  :bind ([remap other-window] . ace-window)
  :custom-face
  (aw-leading-char-face ((t (:inherit 'error :bold t :height 1.2))))
  (aw-mode-line-face ((t (:inherit 'mode-line-emphasis :bold t))))
  :preface
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  :hook (after-init . ace-window-display-mode)
  :config
  ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (with-eval-after-load 'hydra
    ;; https://github.com/abo-abo/ace-window/wiki/Hydra
    ;; hydra-frame-window is designed from `ace-window' and
    ;; matches aw-dispatch-alist with a few extra
    (defhydra hydra-frame-window (:color red :hint nil)
      "
^Frame^                 ^Window^      Window Size^^^^^^    ^Text Zoom^               (__)
_0_: delete             _t_oggle        ^ ^ _k_ ^ ^            _=_                   (oo)
_1_: delete others      _s_wap          _h_ ^+^ _l_            ^+^             /------\\/
_2_: new                _d_elete        ^ ^ _j_ ^ ^            _-_            / |    ||
_F_ullscreen            ^ ^             _b_alance^^^^          ^ ^        *  /\\---/\\  ~~  C-c w/C-x o w
"
      ("0" delete-frame :exit t)
      ("1" delete-other-frames :exit t)
      ("2" make-frame  :exit t)
      ("b" balance-windows)
      ("s" ace-swap-window)
      ("F" toggle-frame-fullscreen)
      ("t" toggle-window-split)
      ("d" ace-delete-window :exit t)
      ("-" text-scale-decrease)
      ("=" text-scale-increase)
      ("h" shrink-window-horizontally)
      ("k" shrink-window)
      ("j" enlarge-window)
      ("l" enlarge-window-horizontally)
      ("q" nil "quit"))
    (add-to-list 'aw-dispatch-alist '(?w hydra-frame-window/body) t)
    (bind-key "C-c w" #'hydra-frame-window/body)))


(provide 'setup-windows)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-windows.el ends here.
