;;; poet-client.el --- Client for po.et network api  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 W.Yahia

;; Author: W.Yahia
;; Version: 0.1-pre
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/wailo/emacs-poet

;;; Commentary:

;; This package provide a client to po.et network api.
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
(defgroup po.et nil
  "Client for po.et network api."
  :group 'Applications)

(defcustom poet-client-api-url "https://api.poetnetwork.net/works" "Po.et api url."
  :type '(string)
  :group 'po.et)

(defcustom poet-client-api-token "" "Po.et api Authentication token."
  :type '(string)
  :group 'po.et)

(defcustom poet-client-default-author ""  "Default author for claim form."
  :type '(string)
  :group 'po.et)

(defcustom poet-client-enable-logs nil  "Enable verbose ouput for debugging and development."
  :type '(integer)
  :group 'po.et)


;;;; Variables

(defvar poet-client-works nil "Data structure for po.et works.")
(defvar poet-client-last-windows nil "Last window configuration.")


;;;; Functions

(defun poet-client-set-prompt-api-token ()
  "Set API token, prompt user if empty."
  (while (string-blank-p poet-client-api-token)
    (setq poet-client-api-token (poet-client-remove-quotes-spaces (read-string "Enter po.et API token: ")))))

(defun poet-client-get-content (buf)
  "Get content in a selectd region or the whole buffer.
BUF Target buffer where content will be extracted"

  (with-current-buffer buf
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun poet-client-remove-quotes-spaces (str)
"Remove surrounding quotes and spaces from a string.
STR string"
(string-remove-suffix "\"" (string-remove-prefix "\"" (string-trim str))))

(defun poet-client-create-claim-form (buf)
  "Create ui for po.et claim form.
BUF Target buffer where content will be extracted"

  (setq content (poet-client-get-content buf))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert (propertize "po.et\n\n" 'face 'info-title-1))

  (if (string-blank-p poet-client-api-token)
      (progn
        (widget-insert "See instructions at https://docs.poetnetwork.net/use-poet/create-your-first-claim.html\n")
        (setq w_api_token (widget-create 'editable-field
                       :size 98
                       :format "API Token:\t%v" ; Text after the field!
                       :notify (lambda (wid &rest _ignore) (if (string-prefix-p "TEST" (widget-value wid))
                                                              (message "yes") ;; change API address and inform the user
                                                            (message "no")))
                       ""))
        (widget-insert "    ")
        (widget-create 'push-button
                       :notify (lambda (&rest _ignore) (if (yes-or-no-p "Do you want to remember the token for later sessions? ")
                                                         (custom-set-variables '(poet-client-api-token (widget-value w_api_token))))) "Remember for later sessions")
                (widget-insert "\n")))

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
                                :format "Author:\t\t%v" ; Text after the field!
                                poet-client-default-author))
  (widget-insert "    ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore) (if (yes-or-no-p "Do you want to remember the author for later sessions? ")
                                                    (custom-set-variables '(poet-client-default-author (widget-value w_author))))) "Remember for later sessions")
  (widget-insert "\n")
  (setq w_tags (widget-create 'editable-field
                              :size 13
                              :format "Tags:\t\t%v\n" ; Text after the field!
                              ""))
  (widget-insert "\n")


  (defun poet-client-send-form (&rest _ignore)
    (if (not poet-client-api-token)
        (setq poet-client-api-token (widget-value w_api_token)))

    (poet-client-create-claim-request (poet-client-remove-quotes-spaces (widget-value w_name))
                                      (poet-client-remove-quotes-spaces (widget-value w_date_c))
                                      (poet-client-remove-quotes-spaces (widget-value w_date_p))
                                      (poet-client-remove-quotes-spaces (widget-value w_author))
                                      (poet-client-remove-quotes-spaces (widget-value w_tags))
                                      content))

  (widget-create 'push-button
                 :notify 'poet-client-send-form
                 "Create claim [C-c C-c]")
  (widget-insert "    ")

  (defun  poet-client-kill-form (&rest _ignore)
    (kill-buffer (current-buffer))
    ;; Restore window configuration
    (set-window-configuration poet-client-last-windows))

  (widget-create 'push-button
                 :notify  'poet-client-kill-form
                 "Exit [q]")
  (widget-insert "\n")
  (widget-insert (make-string 80 ?\u2501))
  (widget-insert "\n\n")
  (widget-insert (propertize "Content" 'face 'info-title-2))
  (widget-insert "\n-------------------------------------------\n")
  (widget-insert (format "%s" content))
  (define-key widget-keymap (kbd "C-c C-c") (lambda () (interactive) (poet-client-send-form)))
  (define-key widget-keymap (kbd "q") (lambda () (interactive) (poet-client-kill-form)))
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))


;;;###autoload
(defun poet-client-register-claim ()
  "Register cleam on po.et network."

  (interactive)
  ; Save the current window configuration
  (setq poet-client-last-windows (current-window-configuration))
  (setq content-buf (current-buffer))
  (with-temp-buffer "*po.et Claim*"
                    (switch-to-buffer-other-window "*po.et Claim*")
                    (poet-client-create-claim-form content-buf)))

(defun poet-client-create-claim-request (name date-c date-p author tags content)
  "Register a claim on po.et network.
NAME published work name
DATE-C published work creation date
DATE-P published work publication date
AUTHOR published work author
TAGS published work tags
CONTENT published work content"


  (if poet-client-enable-logs (progn (custom-set-variables '(request-log-level 'blather)
                                                    '(request-message-level 'blather)))
    (progn (custom-set-variables '(request-log-level -1)
                                 '(request-message-level -1))))

  (request
   poet-client-api-url
   :type "POST"
   :data (json-encode `(("name" . ,name) ("dateCreated" . ,date-c)
                        ("datePublished" . ,date-p) ("author" . ,author) ("tags" . ,tags) ("content" . ,content)))
   :headers `(("Content-Type" . "application/json") ("token" . ,poet-client-api-token))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Work Id: %S" (assoc-default 'workId data))))))

;;;###autoload
(defun poet-client-retrieve-works ()
  "Retrieve works from po.et network."
  (interactive)
  (poet-client-set-prompt-api-token)

  (if poet-client-enable-logs (progn (custom-set-variables '(request-log-level 'blather)
                                                    '(request-message-level 'blather)))
    (progn (custom-set-variables '(request-log-level -1)
                                 '(request-message-level -1))))

  (request
   poet-client-api-url
   :type "GET"
   :headers `(("Content-Type" . "application/json") ("token" . ,poet-client-api-token))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq poet-client-works data)
               (poet-client-works-list-popup (poet-client-parse-works-response data))))))

(defun poet-client-parse-works-response (works-json-response)
  "Parse works json response and convert it to a list of vector.
WORKS-JSON-RESPONSE api response of $API_URL/works"

  (let ((index 0))
  (mapcar (lambda (work) (append (list (cl-incf index) (poet-client-parse-works-extract-values work)))) works-json-response)))

(defun poet-client-parse-works-extract-values (work)
  "Extract values from a single work entry.
WORK work entry"

  (vector (assoc-default 'name work)
          (assoc-default 'author work)
          (assoc-default 'tags work)
          (assoc-default 'dateCreated work)
          (assoc-default 'datePublished work)
          ))


(define-derived-mode poet-client-mode tabulated-list-mode "po.et-mode" "Major mode for po.et UI menu of publised works"
  (define-key tabulated-list-mode-map (kbd "RET") (lambda () (interactive) (poet-client-get-selected-work-from-url)))
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

(defun poet-client-get-selected-work-from-url ()
  "Get/Download published user selected work.
The index of the selected work is retrieved using 'tabulated-list-get-id'"

 ;; Table index starts from 1
  (let* ((index (- (tabulated-list-get-id) 1))
    (content-header (aref poet-client-works index))
    (url (assoc-default 'archiveUrl content-header)))

  (request
   url
   :parser 'buffer-string
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (poet-client-works-buffer content-header data))))))

(defun poet-client-works-buffer (content-header content)
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

  (with-output-to-temp-buffer (format "*po.et %s" name)
    (print (format "Name: %s\nAuthor: %s\nCreationDate: %s\nPublicationDate: %s\nTags: %s\nHash: %s \nURL: %s\n\nContent:\n%s" name author dateCreated datePublished tags hash archiveUrl content)))))

(defun poet-client-works-list-popup (works-list)
  "List published works in a popup.
WORKS-LIST list of published works"

  (pop-to-buffer "*po.et Works*" nil)
  (poet-client-mode)
  (setq tabulated-list-entries works-list)
  (tabulated-list-print t))

(provide 'poet-client)
;;; poet-client.el ends here
