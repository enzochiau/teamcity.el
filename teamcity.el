;;;; Emacs TeamCity Client

(require 'cl)


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
  (url-generic-parse-url (concat (teamcity-get-url-string url-end))))


(defun teamcity-get-url-string (url-end)
  (concat "http://" teamcity-username "@" teamcity-server 
          "/httpAuth/app/rest/" url-end))


(defun teamcity-rest-buffer (request)
  "Sends TeamCity REST request and returns a buffer with response"
  (let ((response-buffer (generate-new-buffer "teamcity-rest-response")))
    (save-current-buffer
      (set-buffer response-buffer)
      (url-insert-file-contents (teamcity-get-url-string request)))
    response-buffer))


(defun teamcity-rest-xml (request)
  "Sends TeamCity REST request and returns a parsed xml"
  (let ((buf (teamcity-rest-buffer request)))
    (with-current-buffer buf
      (save-excursion
        (xml-parse-region (point-min)
                          (point-max)
                          (current-buffer))))))

  
(defun teamcity-get-version ()
  (let ((response-buffer (teamcity-rest-buffer "version")))
    (save-current-buffer
      (set-buffer response-buffer)
      (buffer-string))))


(defun teamcity-version ()
  "Display TeamCity version."
  (interactive)
  (message (concat "TeamCity version is " (teamcity-get-version))))


(defun teamcity-projects()
  "Display TeamCity projects"
  (interactive)
  (let* ((projects-buffer (get-buffer-create "*TeamCity: Projects*"))
         (projects (teamcity-get-projects)))
    (set-buffer projects-buffer)
    (dolist (p projects nil)
      (insert (concat "* " (teamcity-project-get-name p)))
      (let* ((start (point-at-bol))
             (end (point-at-eol))
             (project-id (teamcity-project-get-id p))
             (project-details-str (teamcity-get-url-string (concat "projects/id:" project-id))))
        (put-text-property start end 'details project-details-str))
      (insert "\n"))
    (teamcity-mode)
    (switch-to-buffer projects-buffer)))


(defun teamcity-get-projects ()
  (teamcity-parse-projects (teamcity-rest-xml "projects")))


(defun teamcity-parse-projects (xml)
  (let* ((root-node (car xml))
         (projects (xml-node-children root-node)))
    (mapcar* 'teamcity-parse-project projects)))

(defun teamcity-parse-project (xml)
  (let ((id   (xml-get-attribute xml 'id))
        (name (xml-get-attribute xml 'name)))
    (list (cons 'id id) (cons 'name name))))


(defun teamcity-project-get-name (project)
  (cdr (assoc 'name project)))


(defun teamcity-project-get-id (project)
  (cdr (assoc 'id project)))


(defvar teamcity-mode-hook nil)


(defvar teamcity-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "RET") 'teamcity-show-details)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "u") 'cua-scroll-up)
    (define-key map (kbd "d") 'cua-scroll-down)
    map))


(defun teamcity-mode ()
  ""
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq major-mode 'teamcity-mode
        mode-name "TeamCity")
  (use-local-map teamcity-mode-map)
  (run-hooks 'teamcity-mode-hook))


(defun teamcity-show-details ()
  (interactive)
  (let* ((details-str (get-text-property (point) 'details))
         (details-url (teamcity-get-url details-str)))
    ))


(provide 'teamcity)