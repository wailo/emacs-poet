;;; code:


(defcustom POET-API-URL "https://api.poetnetwork.net/works" "POET API URL"
  :type '(string)
  :group 'POET
  )

(defcustom POET-API-TOKEN "" "POET API Authentication token"
  :type '(string)
  :group 'POET
  )

(defvar-local poet-works "Data structure for poet works")


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

(defun poet-retrieve-works ()
  "Create cleam on poet network."
  (interactive)
  (require 'request)
  (custom-set-variables '(request-log-level 'debug )
                        '(request-message-level 'debug))
  (setq response nil)

  (request
   POET-API-URL
   :type "GET"
   :headers `(("Content-Type" . "application/json") ("token" . ,POET-API-TOKEN))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq poet-works (poet-parse-works-response data))
               (poet-works-popup poet-works)
               ;; (poet-works-table response)
               )))
  )


(defun poet-parse-works-response (json-response)
  (setq poet-works json-response)
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
  (tabulated-list-get-id)
  (tabulated-list-get-entry)
  )

(defun poet-works-popup (data)
  (pop-to-buffer "*PO.ET Works*" nil)
  (poet-mode)
  (setq tabulated-list-entries data)
  (tabulated-list-print t))


(require 'ctable)
(defun poet-works-table (input-data)
(let* ((column-model ; column model
        (list (make-ctbl:cmodel
              :title "Name" :sorter 'ctbl:sort-string-lessp
              :min-width 5 :align 'left)
              (make-ctbl:cmodel
               :title "Author" :align 'left
               :sorter (lambda (a b) 'ctbl:sort-string-lessp))
              (make-ctbl:cmodel
               :title "Date Created" :align 'left)
              (make-ctbl:cmodel
               :title "Date Published" :align 'left)
              (make-ctbl:cmodel
               :title "Tags" :align 'left)
              (make-ctbl:cmodel
               :title "hash" :align 'left)
       (make-ctbl:cmodel
               :title "URL" :align 'left)))
       (data (mapcar (lambda (element) (append element nil))  (mapcar (lambda (pair) (nth 1 pair)) input-data) ))
       (model ; data model
          (make-ctbl:model
           :column-model column-model :data data))
       (component ; ctable component
        (ctbl:create-table-component-buffer
         :model model)))
  (pop-to-buffer (ctbl:cp-get-buffer component)))
)


(provide poet_api)
;;; poet_api.el ends here
