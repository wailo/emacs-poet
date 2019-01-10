;;; poet-client.el --- Client for Po.et network api  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 W.Yahia

;; Author: W.Yahia
;; Version: 0.1-pre
;; Package-Requires: ((widget) (wid-edit) (request))
;; Keywords: Po.et blockchain publishing
;; URL: https://github.com/wailo/emacs-poet

;;; Commentary:

;; This package provide a client to Po.et network api.
;; The can publish work to the network and list published works

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

(defcustom poet-api-url "https://api.poetnetwork.net/works" "Po.et api url."
  :type '(string)
  :group 'Po.et)

(defcustom poet-api-token "" "Po.et api Authentication token."
  :type '(string)
  :group 'Po.et)


;;;; Variables

(defvar poet-works nil "Data structure for Po.et works.")
(defvar poet-last-windows nil "Last window configuration.")


;;;; Functions

(defun get-content (buf)
  "Get content in a selectd region or the whole buffer.
BUF Target buffer where content will be extracted"

  (with-current-buffer buf
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (buffer-substring-no-properties (point-min) (point-max)))))


(defun poet-remove-quotes (str)
"Remove surrounding quotes from a string
STR string"
(string-remove-suffix "\"" (string-remove-prefix "\"" str)))

(defun poet-create-claim-form (buf)
  "Create ui for Po.et claim form.
BUF Target buffer where content will be extracted"

  (setq content (get-content buf))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert (propertize "PO.ET\n\n" 'face 'info-title-1))

  (if (not poet-api-token)
      (progn
        (setq w_api_token (widget-create 'editable-field
                       :size 98
                       :format "API Token:\t%v" ; Text after the field!
                       :notify (lambda (wid &rest ignore) (if (string-prefix-p "TEST" (widget-value wid))
                                                              (message "yes") ;; change API address and inform the user
                                                            (message "no")))
                       ""))
        (widget-insert " See instructions at https://docs.poetnetwork.net/use-poet/create-your-first-claim.html\n")))

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
                           (if (not poet-api-token)
                               (setq poet-api-token (widget-value w_api_token))
                             )

                           (poet-create-claim-request (poet-remove-quotes (widget-value w_name))
                                                      (poet-remove-quotes (widget-value w_date_c))
                                                      (poet-remove-quotes (widget-value w_date_p))
                                                      (poet-remove-quotes (widget-value w_author))
                                                      (poet-remove-quotes (widget-value w_tags))
                                                      content))
                 "Create claim")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (kill-buffer (current-buffer))
                           ; Restore window configuration
                           (set-window-configuration poet-last-windows))
                 "Exit")

  (widget-insert "\n\n")
  (widget-insert (propertize "Content" 'face 'info-title-2))
  (widget-insert "\n-------------------------------------------\n")
  (widget-insert (format "%s" content))
  (use-local-map widget-keymap)
  (widget-setup)
  (beginning-of-buffer))


;;;###autoload
(defun poet-create-claim ()
  "Create cleam on Po.et network."

  (interactive)
  ; Save the current window configuration
  (setq poet-last-windows (current-window-configuration))
  (setq content-buf (current-buffer))
  (with-temp-buffer "*Po.et Claim*"
                    (switch-to-buffer-other-window "*Po.et Claim*")
                    (poet-create-claim-form content-buf)))

(defun poet-create-claim-request (name date-c date-p author tags content)
  "Create cleam on poet network.
NAME published work name
DATE-C published work creation date
DATE-P published work publication date
AUTHOR published work author
TAGS published work tags
CONTENT published work content"

  (message (request
            poet-api-url
            :type "POST"
            :data (json-encode `(("name" . ,name) ("dateCreated" . ,date-c)
                                 ("datePublished" . ,date-p) ("author" . ,author) ("tags" . ,tags) ("content" . ,content)))
            :headers `(("Content-Type" . "application/json") ("token" . ,poet-api-token))
            :parser 'json-read
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        (message "Work Id: %S" (assoc-default 'workId data)))))))

;;;###autoload
(defun poet-retrieve-works ()
  "Retrieve works from Po.et network"
  (interactive)
  (request
   poet-api-url
   :type "GET"
   :headers `(("Content-Type" . "application/json") ("token" . ,poet-api-token))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq poet-works data)
               (poet-works-list-popup (poet-parse-works-response data))))))

(defun poet-parse-works-response (works-json-response)
  "Parse works json response and convert it to a list of vector.
WORKS-JSON-RESPONSE api response of $API_URL/works"

  (setq index 0)
  (mapcar (lambda (work) (append (list (cl-incf index) (poet-parse-works-extract-values work)))) works-json-response))

(defun poet-parse-works-extract-values (work)
  "Extract values from a single work entry.
WORK work entry"

  (vector (assoc-default 'name work)
          (assoc-default 'author work)
          (assoc-default 'tags work)
          (assoc-default 'dateCreated work)
          (assoc-default 'datePublished work)
          ))


(define-derived-mode poet-mode tabulated-list-mode "Po.et-mode" "Major mode for Po.et UI menu of publised works"
  (define-key tabulated-list-mode-map (kbd "RET") (lambda () (interactive) (poet-get-selected-work-from-url)))
  (use-local-map tabulated-list-mode-map)
  (setq tabulated-list-format [("name" 50 t)
                               ("Author" 30 nil)
                               ("tags" 50 t)
                               ("dateCreated" 30 t)
                               ("datePublished" 30 t)
                               ;; ("hash"  10 t)
                               ;; ("archiveUrl" 0 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "name" nil))
  (tabulated-list-init-header))

(defun poet-get-selected-work-from-url ()
  "Get/Download published user selected work.
The index of the selected work is retrieved using 'tabulated-list-get-id'"

  (let* ((index (tabulated-list-get-id))
    (content-header (aref poet-works index))
    (url (assoc-default 'archiveUrl content-header)))

  (request
   url
   :parser 'buffer-string
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (poet-works-buffer content-header data))))))

(defun poet-works-buffer (content-header content)
  "Display work content in a buffer.
CONTENT-HEADER header of the publishd work e.g. name, author.
CONTENT the body/content of the published work"

  (let ((name (assoc-default 'name content-header))
        (author (assoc-default 'author content-header))
        (tags (assoc-default 'tags content-header))
        (dateCreated (assoc-default 'dateCreated content-header))
        (datePublished (assoc-default 'datePublished content-header))
        (hash (assoc-default 'hash content-header))
        (archiveUrl (assoc-default 'archiveUrl content-header)))

  (with-output-to-temp-buffer (format "*Po.et %s" name)
    (print (format "Name: %s\nAuthor: %s\nCreationDate: %s\nPublicationDate: %s\nTags: %s\nHash: %s \nURL: %s\n\nContent:\n%s" name author dateCreated datePublished tags hash archiveUrl content)))))

(defun poet-works-list-popup (poet-works-list)
  "List published works in a popup.
POET-WORKS-LIST list of published works"

  (pop-to-buffer "*Po.et Works*" nil)
  (poet-mode)
  (setq tabulated-list-entries poet-works-list)
  (tabulated-list-print t))

(provide 'poet-client)
;;; poet-client.el ends here
