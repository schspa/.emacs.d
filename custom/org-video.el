;;; org-video.el --- Description

;; Author: Schspa  schspa@gmail.com
;; URL:

;; Copyright (C) 2023, Schspa, all rights reserved.
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
;; https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E5%9C%A8org-mode%E4%B8%AD%E7%94%A8%E9%93%BE%E6%8E%A5%E7%9A%84%E5%BD%A2%E5%BC%8F%E5%B5%8C%E5%85%A5Youtube%E8%A7%86%E9%A2%91.org
;;

;;; Code:
(require 'org)

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-link-set-parameters
 "yt"
 :follow (lambda (handle)
           (browse-url
            (concat "https://www.youtube.com/embed/"
                    handle)))
 :export (lambda (path desc backend)
           (cl-case backend
             (html (format yt-iframe-format
                           path (or desc "")))
             (latex (format "\href{%s}{%s}"
                            path (or desc "video"))))))

(defvar video-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"600\""
          " height=\"400\""
          " src=\"%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))


(org-link-set-parameters
 "video"
 :follow (lambda (handle)
           (browse-url handle))
 :export (lambda (path desc backend)
           (cl-case backend
             (html (format video-iframe-format
                           path (or desc ""))))))


(provide 'org-video)
;; Local Variables:
;; coding: utf-8
;; End:

;;; org-video.el ends here.
