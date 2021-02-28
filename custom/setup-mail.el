;;; setup-mail.el --- setup email

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

;; Set work email with fellowing elisp code.
;; (secrets-create-item
;;  (secrets-get-alias "default")
;;  "work-mail" "password"
;;  :service "email"
;;  :user-full-name "Zhaohui Shi"
;;  :username "zhaohui.shi"
;;  :host "mail.domain.com"
;;  :user-mail-address "zhaohui.shi@domain.com"
;;  :maildir "^/work"
;;  :get-mail-command "offlineimap -a work")

(defun get-mail-conf (service attribute)
  "Get email configuration for system keyring"
  (let* (
         (collect (secrets-get-alias "default"))
         (item (car (secrets-search-items collect :service service))))
    (if (equal attribute :password) (secrets-get-secret collect item)
      (secrets-get-attribute collect item attribute))))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :config
  (setq mu4e-contexts
        `( ,(make-mu4e-context
	         :name "Private"
	         :enter-func (lambda () (mu4e-message "Entering Private context"))
             :leave-func (lambda () (mu4e-message "Leaving Private context"))
	         ;; we match based on the maildir of the message
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :maildir "^/gmail")))
	         :vars '(( user-mail-address     . "schspa@gmail.com"  )
		             ( user-full-name	     . "Schspa Shi" )
                     ( mu4e-get-mail-command . "proxychains offlineimap -a gmail")))
           ))
  (when (get-mail-conf "email" :maildir)
    (add-to-list 'mu4e-contexts
                 (make-mu4e-context
                  :name "work"
                  :enter-func (lambda () (mu4e-message "Switch to the Work context"))
                  ;; no leave-func
                  ;; we match based on the maildir of the message
                  :match-func (lambda (msg)
                                (when msg
                                  (mu4e-message-contact-field-matches
                                   msg
                                   :maildir (get-mail-conf "email" :maildir))))
                  :vars
                  `(( user-mail-address       . ,(get-mail-conf "email" :user-mail-address))
                    ( user-full-name          . ,(get-mail-conf "email" :user-full-name))
                    ( mu4e-compose-signature  . ,(concat
                                                  (get-mail-conf "email" :user-full-name)
                                                  "\n"
                                                  "BRs\n"))
                    ( mu4e-get-mail-command   . ,(get-mail-conf "email" :get-mail-command))))))
  )

(provide 'setup-mail)

;; Local Variables:
;; coding: utf-8
;; End:

;;; setup-mail.el ends here.
