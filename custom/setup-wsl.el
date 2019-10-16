;;; setup-wsl.el --- WSL Setup

;; Author: Schspa  schspa@gmail.com
;; Keywords: elisp
;; URL:

;; Copyright (C) 2019, Schspa, all rights reserved.
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
;; we could wrap it up nicely:
(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun get_upper_dir_and_name (path)
  "Get Upper level directory"
  (let* ((file_paths (split-string path "/")))
	(list (mapconcat 'identity (butlast file_paths) "/") (car (last file_paths)))))

;; TODO optomize function
(defun wsl-path-r (path)
  "convert wsl path to windows path"
  (let* ((file_paths (split-string path))
		 (wsl-outuput)
		 (wsl-ret)
		 (wsl-path))
	(setq wsl-output (process-exit-code-and-output "/bin/wslpath" "-w" path))
	(setq wsl-ret (car wsl-output))
	(if (= wsl-ret 0) (string-trim (car (last wsl-output)))
	  (let* ((parents (get_upper_dir_and_name path))
			 (tmp-path))
		(setq tmp-path (wsl-path (car parents)))
		(concat tmp-path "\\" (car (last parents)))))))

(defun wsl-path-escape (path)
  "Escaping wsl-path to use it in powershell command"
  (replace-regexp-in-string "\\\\" "\\\\\\\\" (wsl-path-r path)))

(defun wsl/yank-image-from-win-clipboard-through-powershell()
  "to simplify the logic, use c:/Users/Public as temporary directoy, and move it into current directoy"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (file-name (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))
		 (file-directory (expand-file-name "./pic/"))
         ;; (file-path-powershell (concat "c:/Users/\$env:USERNAME/" file-name))
		 (file-path-wsl (concat file-directory file-name))
		 )
	;; (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/\\$env:USERNAME/" file-name "\\\")\""))
	(shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/Public/" file-name "\\\")\""))
	(if (not (file-directory-p file-directory)) (make-directory file-directory))
	(rename-file (concat "/mnt/c/Users/Public/" file-name) file-path-wsl)))

(defun wsl/open-in-external-file-explorer (path)
  "Open file in another file explorer"
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
		 (directory (wsl-path-escape path)))
	(shell-command (concat powershell  " \"start " directory "\""))))

(defun wsl/open-in-desktop-from-wsl()
  "open desktop by send command from wsl into powershell"
  (interactive)
  (wsl/open-in-external-file-explorer default-directory))

(defun wsl/open-in-xternal-app-from-wsl()
  "open desktop by send command from wsl into powershell"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
		 (-file-list
		  (if (string-equal major-mode "dired-mode")
			  (dired-get-marked-files)
			(list (buffer-file-name)))))
    (shell-command (concat powershell " -command \"start " (wsl-path-escape (nth 0 -file-list)) "\""))))

(provide 'setup-wsl)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-wsl.el ends here.
