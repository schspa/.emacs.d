;;; setup-mail.el --- setup email -*- lexical-binding: t -*-

;; Author: schspa  schspa@gmail.com
;; URL:

;; Copyright (C) 2020, schspa, all rights reserved.
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
(require 'json)

;; https://www.sastibe.de/2021/01/setting-up-emacs-as-mail-client/
;; https://gist.github.com/letoh/5497116
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-03.org
;; Set work email with fellowing elisp code.
;; (secrets-create-item
;;  (secrets-get-alias "default")
;;  "email configuration for work"
;;  (json-serialize '((:user-full-name . "zhaohui.shi")
;;                    (:password . "xxx")
;;                    (:username . "zhaohui.shi")
;;                    (:host . "mail.domain.com")
;;                    (:user-mail-address . "zhaohui.shi@domain.com")
;;                    (:maildir . "^/work")
;;                    (:get-mail-command . "offlineimap -a work")))
;;  :service "email-conf"
;;  :username "work")

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str))) str)


(defun get-keychain-password (service account-name)
  "Gets `account` keychain password from OS X Keychain"
  (if (eq system-type 'darwin)
      (chomp
       (shell-command-to-string
        (concatenate
         'string
         "security find-generic-password -s " service " -wga "
         account-name)))
    (let ((collect (secrets-get-alias "default")))
      (secrets-get-secret
       collect
       (car (secrets-search-items
             collect :service service :username account-name))))))

(defun get-mail-conf (user attribute)
  "Get email configuration for system keyring"
  (let* ((json-string (get-keychain-password "email-conf" "gmail"))
         (mail-conf (json-parse-string json-string :object-type 'alist)))
    (cdr (assoc attribute mail-conf))))

(defgroup mail-work nil
  "Facilities for work environment mail to work."
  :group 'development)

(defcustom work-mail-dir nil
  "mail address for mail"
  :group 'mail-work
  :type 'string)

(defcustom work-mail-username ""
  "mail address for mail"
  :group 'mail-work
  :type 'string)

(defcustom work-mail-address ""
  "mail address for work"
  :group 'mail-work
  :type 'string)

(defcustom work-mail-user-full-name ""
  "mail address for mail"
  :group 'mail-work
  :type 'string)

(defcustom work-mail-getmail-command ""
  "mail address for mail"
  :group 'mail-work
  :type 'string)

(defcustom work-mail-smtp-server ""
  "mail address for mail"
  :group 'mail-work
  :type 'string)

;; use auth-source to open ~/.authinfo.gpg, thus to avoid to input password
(defun schspa/mu4e-update-mail-and-index (orig-fun prefix &rest args)
  (interactive "P")
  (auth-source-search :host "smtp.gmail.com")
  (funcall orig-fun args))

(use-package mu4e
  :ensure-system-package mu
  :load-path (lambda ()
               (let* ((mu-path (string-trim (shell-command-to-string "realpath $(which mu)")))
                      (mu-dir (file-name-directory mu-path))
                      (parent-dir (file-name-directory (directory-file-name mu-dir))))
                 (expand-file-name "share/emacs/site-lisp/mu/mu4e" parent-dir)))
  :config
  (setq
   mu4e-get-mail-command "offlineimap"
   mu4e-update-interval 300
   mu4e-headers-auto-update t
   mu4e-compose-format-flowed nil)
  (advice-add 'mu4e-update-mail-and-index
              :around #'schspa/mu4e-update-mail-and-index)
  (setq mu4e-contexts
        `( ,(make-mu4e-context
	         :name "Private"
	         :enter-func (lambda () (mu4e-message "Entering Private context"))
             :leave-func (lambda () (mu4e-message "Leaving Private context"))
	         ;; we match based on the maildir of the message
             :match-func (lambda (msg) (when msg
                                         (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
	         :vars '((mu4e-sent-folder   . "/gmail/sent")
	                 (mu4e-trash-folder  . "/gmail/trash")
	                 (mu4e-refile-folder . "/gmail/archive")
	                 (mu4e-drafts-folder . "/gmail/drafts")
                     ( user-mail-address     . "schspa@gmail.com"  )
		             ( user-full-name	     . "Schspa Shi" )
                     ( mu4e-get-mail-command . "offlineimap -a gmail")
                     ( mu4e-compose-signature  .
                       ;; https://en.wikipedia.org/wiki/Signature_block#Standard_delimiter
                       "BRs\nSchspa Shi\n")
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
	                 (smtpmail-smtp-server . "smtp.gmail.com")
	                 (smtpmail-smtp-user . "schspa@gmail.com")
                     (smtpmail-debug-info . t)
                     (smtpmail-debug-verb . t)
	                 (smtpmail-stream-type . starttls)
                     (starttls-use-gnutls . t)
	                 (smtpmail-smtp-service . 587)
                     ))))
  (when work-mail-dir
    (add-to-list 'mu4e-contexts
                 (make-mu4e-context
                  :name "work"
                  :enter-func (lambda () (mu4e-message "Switch to the Work context"))
                  ;; no leave-func
                  ;; we match based on the maildir of the message
                  :match-func (lambda (msg) (when msg
                                              (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
                  :vars
                  `((mu4e-sent-folder   . "/work/sent")
	                (mu4e-trash-folder  . "/work/trash")
	                (mu4e-refile-folder . "/work/archive")
	                (mu4e-drafts-folder . "/work/drafts")
                    ( user-mail-address       . ,(symbol-value 'work-mail-address))
                    ( user-full-name          . ,(symbol-value 'work-mail-user-full-name))
                    ( mu4e-compose-signature  .
                      ,(concat
                        "BRs\n"
                        work-mail-user-full-name
                        "\n"))
                    ( mu4e-get-mail-command   . ,(symbol-value 'work-mail-getmail-command))
                    (smtpmail-default-smtp-server . ,(symbol-value 'work-mail-smtp-server))
	                (smtpmail-smtp-server . ,(symbol-value 'work-mail-smtp-server))
	                (smtpmail-smtp-user . ,(symbol-value 'work-mail-username))
                    ;; (smtpmail-debug-info . t)
                    ;; (smtpmail-debug-verb . t)
	                (smtpmail-stream-type . starttls)
                    (starttls-use-gnutls . t)
	                (smtpmail-smtp-service . 587)
                    ))))
  )

(setq send-mail-function 'smtpmail-send-it)

(use-package org-mime
  :ensure t
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                                   :with-author nil
                                                   :with-toc nil))
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                             "#E6E1DC" "#232323"))))
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))

(provide 'setup-mail)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-mail.el ends here.
