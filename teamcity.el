;;;; Emacs TeamCity Client

(defgroup teamcity nil
  "Emacs interface for TeamCity."
  :prefix "teamcity-"
  :group 'tools)

(defcustom teamcity-server "buildserver"
  "TeamCity server."
  :group 'teamcity
  :type 'string)

(defcustom teamcity-username "guest"
  "TeamCity username."
  :group 'teamcity
  :type 'string)


(defun teamcity-get-url (url-end)
  (url-generic-parse-url (concat (teamcity-get-base-url-string url-end))))


(defun teamcity-get-url-string (url-end)
  (concat "http://" teamcity-username "@" teamcity-server 
          "/httpAuth/app/rest/" url-end))


(defun teamcity-rest (request)
  (let ((response-buffer (generate-new-buffer "teamcity-rest-response")))
    (save-current-buffer
      (set-buffer response-buffer)
      (url-insert-file-contents (teamcity-get-url-string request)))
    response-buffer))

  
(defun teamcity-get-version ()
  (let ((response-buffer (teamcity-rest "version")))
    (save-current-buffer
      (set-buffer response-buffer)
      (buffer-string))))


(defun teamcity-version ()
  "Display TeamCity version."
  (interactive)
  (message (concat "TeamCity version is " (teamcity-get-version))))


(provide 'teamcity)