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

(defgroup teamcity-faces nil
  "Customize TeamCity UI"
  :prefix "teamcity-"
  :group 'faces
  :group 'teamcity)

(defface teamcity-project
  '((t ;:underline t
       :foreground "blue"))
  "Face for project."
  :group 'teamcity-faces)

(defface teamcity-buildtype
  '((t :foreground "sienna"))
  "Face for project."
  :group 'teamcity-faces)


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
  (let* ((buf (teamcity-rest-buffer request))
         (xml (with-current-buffer buf
                (save-excursion
                  (xml-parse-region (point-min)
                                    (point-max)
                                    (current-buffer))))))
    (kill-buffer buf)
    xml))

  
(defun teamcity-get-version ()
  (let ((response-buffer (teamcity-rest-buffer "version")))
    (save-current-buffer
      (set-buffer response-buffer)
      (let ((version (buffer-string)))
        (kill-buffer response-buffer)
        version))))


(defun teamcity-version ()
  "Display TeamCity version."
  (interactive)
  (message (concat "TeamCity version is " (teamcity-get-version))))


(defun teamcity-projects ()
  "Display TeamCity projects"
  (interactive)
  (let* ((projects-buffer (get-buffer-create "*TeamCity: Projects*"))
         (projects (teamcity-get-projects)))
    (set-buffer projects-buffer)
    (dolist (p projects nil)
      (insert (concat "+ " (teamcity-project-get-name p)))
      (let* ((start (point-at-bol))
             (end (point-at-eol))
             (project-id (teamcity-project-get-id p))
             (project-details-str (teamcity-get-url-string (concat "projects/id:" project-id))))
        (put-text-property start end 'teamcity-object-type 'project)
        (put-text-property start end 'id project-id)
        (put-text-property start end 'details project-details-str)
        (put-text-property start end 'expand 'teamcity-project-expand)
        (put-text-property start end 'collapse 'teamcity-project-collapse)
        (put-text-property start end 'face 'teamcity-project)
        (insert "\n")))
    (beginning-of-buffer)
    (teamcity-mode)
    (switch-to-buffer projects-buffer)))


(defun teamcity-project-expand ()
  (interactive)
  (let ((start (point-at-bol))
        (end (point-at-eol))
        (details-loaded (get-text-property (point) 'project-details-loaded)))
    (cond ((eq details-loaded 'yes)
           (let ((contents (get-text-property (point) 'contents)))
             (save-excursion
               (move-beginning-of-line 2)
               (insert contents))))
          (t
           (let* ((project-id (get-text-property (point) 'id))
                  (project-details-request (concat "projects/id:" project-id))
                  (project-details-xml (teamcity-rest-xml project-details-request))
                  (project-details (teamcity-parse-project-details project-details-xml)))
             (save-excursion
               (move-beginning-of-line 2)
               (dolist (bt (teamcity-project-get-buildTypes project-details) nil)
                 (insert (concat "  " (teamcity-object-get bt 'name)))
                 (insert "\n")
                 (save-excursion
                   (previous-line)
                   (let ((bt-start (point-at-bol))
                       (bt-end (point-at-eol)))
                     (put-text-property bt-start bt-end 'face 'teamcity-buildtype)))))
             (put-text-property start end 'project-details-loaded 'yes))))
    (save-excursion
      (goto-char (+ start 1))
      (insert-and-inherit "-")
      (delete-region start (+ start 1)))))


(defun teamcity-project-collapse ()
  (interactive)
  (let* ((start (point-at-bol))
         (end (point-at-eol))
         (next-project-line (teamcity-get-next-object-point 'project 1)))
    (save-excursion
      (move-beginning-of-line 2)
      (let ((contents (delete-and-extract-region (point) next-project-line)))
        (put-text-property start end 'contents contents))
      (goto-char (+ start 1))
      (insert-and-inherit "+")
      (delete-region start (+ start 1)))))


(defun teamcity-get-next-object-point (object-type lines-forward)
  (save-excursion
    (move-beginning-of-line (+ lines-forward 1))
    (while (and (not (eobp))
                (not (eq (teamcity-get-current-line-object-type) object-type)))
      (next-line))
    (point)))


(defun teamcity-get-current-line-object-type()
  (get-text-property (point) 'teamcity-object-type))


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


(defun teamcity-parse-project-details (xml)
  (let* ((project-node (car xml))
         (id (xml-get-attribute project-node 'id))
         (name (xml-get-attribute project-node 'projectName))
         (webUrl (xml-get-attribute project-node 'webUrl))
         (buildTypes (xml-node-children project-node)))
    (list (cons 'id id)
          (cons 'name name)
          (cons 'webUrl webUrl)
          (cons 'buildTypes (teamcity-parse-project-buildTypes buildTypes)))))


(defun teamcity-parse-project-buildTypes (xml)
  (let* ((root-node (car xml))
         (buildTypes (xml-node-children root-node)))
    (mapcar* 'teamcity-parse-project-buildType buildTypes)))


(defun teamcity-parse-project-buildType (xml)
  (let ((id   (xml-get-attribute xml 'id))
        (name (xml-get-attribute xml 'name))
        (webUrl (xml-get-attribute xml 'webUrl)))
    (list (cons 'id id)
          (cons 'name name)
          (cons 'webUrl webUrl))))


(defun teamcity-project-get-name (project)
  (cdr (assoc 'name project)))


(defun teamcity-project-get-id (project)
  (cdr (assoc 'id project)))


(defun teamcity-project-get-buildTypes (project)
  (cdr (assoc 'buildTypes project)))


(defun teamcity-object-get (object field-symbol)
  (cdr (assoc field-symbol object)))


(defvar teamcity-mode-hook nil)


(defvar teamcity-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "RET") 'teamcity-show-details)
    (define-key map (kbd "TAB") 'teamcity-expand-collapse)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "u") 'cua-scroll-down)
    (define-key map (kbd "d") 'cua-scroll-up)
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


(defun teamcity-expand-collapse ()
  (interactive)
  (let ((expand (get-text-property (point) 'expand))
        (collapse (get-text-property (point) 'collapse))
        (expanded (get-text-property (point) 'expanded))
        (start (point-at-bol))
        (end (point-at-eol))
        (inhibit-read-only t))
    (cond ((eq expanded 'yes)
           (funcall collapse)
           (put-text-property start end 'expanded 'no))
          (t (funcall expand)
             (put-text-property start end 'expanded 'yes)))))



(provide 'teamcity)