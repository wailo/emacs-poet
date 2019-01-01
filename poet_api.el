;;; code:


(defcustom POET-API-URL "https://api.poetnetwork.net/works" "POET API URL"
  :type '(string)
  :group 'POET
  )

(defcustom POET-API-TOKEN "" "POET API Authentication token"
  :type '(string)
  :group 'POET
  )


(defun get-content (buf)
  "get content in a selectd region or the whole buffer."
  (with-current-buffer buf
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
  (buffer-substring-no-properties (point-min) (point-max) )))
  )

(require 'widget)

(eval-when-compile
  (require 'wid-edit))


(defun POET-create-claim-form (buf)
  "Create PO.ET claim form."
  (setq content (get-content buf))
  (let ((inhibit-read-only t) )
    (erase-buffer))
  (remove-overlays)
  (widget-insert (propertize "PO.ET\n\n" 'face 'info-title-1))

  (if (not POET-API-TOKEN)
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



(defun POET-popup-form ()
  (interactive)
  (setq content-buf (current-buffer))
  (with-temp-buffer "*POET Claim*"
  (switch-to-buffer-other-window "*POET Claim*")
    (POET-create-claim-form content-buf)
    )
  )

(POET-popup-form)

(defun poet-create-claim-request (name date-c date-p author tags content)
  "Create cleam on poet network."

  (require 'request)
 (custom-set-variables '(request-log-level 'debug )
                        '(request-message-level 'debug))

  (print (request
   POET-API-URL
   :type "POST"
   :data (json-encode `(("name" . ,name) ("dateCreated" . ,date-c)
                        ("datePublished" . ,date-p) ("author" . ,author) ("tags" . ,tags) ("content" . ,content)))
   :headers `(("Content-Type" . "application/json") ("token" . ,POET-API-TOKEN))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Work Id: %S" (assoc-default 'workId data))))))
  )


(provide poet_api)
;;; poet_api.el ends here



;;; function to create a org buffer
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;; (progn
;;   (setq claim-buffer (get-buffer-create "poet-claim"))
;;   (with-current-buffer claim-buffer
;;     (org-mode)
;;     (insert "* Claim name\n:Name: \n:Author:\n:CreationDate: \n:PublicationDate: \n:Tags: \n:Content:")
;;     ))
