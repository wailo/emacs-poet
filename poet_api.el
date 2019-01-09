;;; package-name.el --- Package description (don't include the word "Emacs")  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 W.Yahia

;; Author: W.Yahia
;; URL: https://github.com/wailo/emacs-poet
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.2"))
;; Keywords: Po.et

;; This file is not part of GNU Emacs.

;;; License:

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

;;; code:


;;;; Requirements

(require 'widget)
(require 'wid-edit)
(require 'request)


;;;; Customization

(defcustom poet-api-url "https://api.poetnetwork.net/works" "POET API URL"
  :type '(string)
  :group 'Po.et
  )

(defcustom poet-api-token "" "Po.et api Authentication token"
  :type '(string)
  :group 'Po.et
  )


;;;; Variables

(defvar poet-works nil "Data structure for Po.et works")


(defun get-content (buf)
  "get content in a selectd region or the whole buffer."
  (with-current-buffer buf
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (buffer-substring-no-properties (point-min) (point-max) )))
  )

(defun poet-create-claim-form (buf)
  "Create PO.ET claim form."
  (setq content (get-content buf))
  (let ((inhibit-read-only t) )
    (erase-buffer))
  (remove-overlays)
  (widget-insert (propertize "PO.ET\n\n" 'face 'info-title-1))

  (if (not poet-api-token)
      (progn
        (widget-create 'editable-field
                       :size 98
                       :format "API Token:\t%v" ; Text after the field!
                       :notify (lambda (wid &rest ignore) (if (string-prefix-p "TEST" (widget-value wid))
                                                              (message "yes") ;; change API address and inform the user
                                                            (message "no")))
                       "")


        (widget-insert " See instructions at https://docs.poetnetwork.net/use-poet/create-your-first-claim.html\n")
        ))

  (setq w_name (widget-create 'editable-field
                              :size 13
                              :format "Name:\t\t%v\n" ; Text after the field!
                              ""))
  (setq w_date_c (widget-create 'editable-field
                                :size 13
                                :format "Date Created:\t%v\n" ; Text after the field!
                                (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil "UTC0")))

  (setq w_date_p (widget-create 'editable-field
                                :size 13
                                :format "Date Published:\t%v\n" ; Text after the field!
                                (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil "UTC0")))
  (setq w_author (widget-create 'editable-field
                                :size 13
                                :format "Author:\t\t%v\n" ; Text after the field!
                                ""))
  (setq w_tags (widget-create 'editable-field
                              :size 13
                              :format "Tags:\t\t%v\n" ; Text after the field!
                              ""))
  (widget-insert "\n")

  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (poet-create-claim-request (widget-value w_name)
                                                      (widget-value w_date_c)
                                                      (widget-value w_date_p)
                                                      (widget-value w_author)
                                                      (widget-value w_tags)
                                                      content))
                 "Create claim")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (kill-buffer (current-buffer)))
                 "Exit")

  (widget-insert "\n\n")
  (widget-insert (propertize "Content" 'face 'info-title-2))
  (widget-insert "\n-------------------------------------------\n")
  (widget-insert (format "%s" content))
  (use-local-map widget-keymap)
  (widget-setup)
  (beginning-of-buffer))



(defun poet-popup-form ()
  (interactive)
  (setq content-buf (current-buffer))
  (with-temp-buffer "*Po.et Claim*"
                    (switch-to-buffer-other-window "*Po.et Claim*")
                    (poet-create-claim-form content-buf)
                    )
  )

(defun poet-create-claim-request (name date-c date-p author tags content)
  "Create cleam on poet network."
  (custom-set-variables '(request-log-level 'debug )
                        '(request-message-level 'debug))

  (print (request
          poet-api-url
          :type "POST"
          :data (json-encode `(("name" . ,name) ("dateCreated" . ,date-c)
                               ("datePublished" . ,date-p) ("author" . ,author) ("tags" . ,tags) ("content" . ,content)))
          :headers `(("Content-Type" . "application/json") ("token" . ,poet-api-token))
          :parser 'json-read
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (message "Work Id: %S" (assoc-default 'workId data))))))
  )

(defun poet-retrieve-works ()
  "Create cleam on poet network."
  (interactive)

  ;; (custom-set-variables '(request-log-level 'debug )
  ;;                       '(request-message-level 'debug))
  
  (setq response nil)

  (request
   poet-api-url
   :type "GET"
   :headers `(("Content-Type" . "application/json") ("token" . ,poet-api-token))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq poet-works data)
               (poet-works-popup (poet-parse-works-response data))
               )))
  )

(defun poet-parse-works-response (json-response)
  (setq index 0)
  (mapcar (lambda (work) (append (list (cl-incf index) (poet-parse-works-extract-values work)))) json-response)
  )

(defun poet-parse-works-extract-values (work)
  (vector (assoc-default 'name work)
          (assoc-default 'author work)
          (assoc-default 'tags work)
          (assoc-default 'dateCreated work)
          (assoc-default 'datePublished work)
          ;; (assoc-default 'hash work)
          ;; (assoc-default 'archiveUrl work)
          )
  )


(define-derived-mode poet-mode tabulated-list-mode "po.et-mode" "Major mode PO.ET mode"
  (define-key tabulated-list-mode-map (kbd "RET") 'poet-open-work)
  (use-local-map tabulated-list-mode-map)
  (setq tabulated-list-format [("name" 50 t)
                               ("Author" 30 nil)
                               ("tags"  50 t)
                               ("dateCreated"  30 t)
                               ("datePublished"  30 t)
                               ;; ("hash"  10 t)
                               ;; ("archiveUrl" 0 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "name" nil))
  (tabulated-list-init-header))

(defun poet-open-work ()
  (interactive)

  (setq index (tabulated-list-get-id))
  (setq content-header (aref poet-works index))
  (setq url (assoc-default 'archiveUrl content-header))

  (request
   url
   :parser 'buffer-string
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (poet-works-buffer content-header data)
               )))
  )

(defun poet-works-buffer (content-header content)
  (setq name (assoc-default 'name content-header))
  (setq author (assoc-default 'author content-header))
  (setq tags (assoc-default 'tags content-header))
  (setq dateCreated (assoc-default 'dateCreated content-header))
  (setq datePublished (assoc-default 'datePublished content-header))
  (setq hash (assoc-default 'hash content-header))
  (setq archiveUrl (assoc-default 'archiveUrl content-header))

  (with-output-to-temp-buffer "foo"
    (print (format "Name: %s\nAuthor: %s\nCreationDate: %s\nPublicationDate: %s\nTags: %s\nHash: %s \nURL: %s\n\nContent:\n%s" name author dateCreated datePublished tags hash archiveUrl content))
    )
  )

(defun poet-works-popup (data)
  (pop-to-buffer "*PO.ET Works*" nil)
  (poet-mode)
  (setq tabulated-list-entries data)
  (tabulated-list-print t))

(provide 'poet)
;;; poet_api.el ends here
