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

(defcustom teamcity-builds-count 10
  "Initial number of builds in the build configuration view."
  :group 'teamcity
  :type 'int)

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

(defface teamcity-buildtype-name-header
  '((t :foreground "blue"))
  "Face for the name of build type on build type screen."
  :group 'teamcity-faces)

(defface teamcity-build-success
  '((t :foreground "forest green"))
  "Successful build."
  :group 'teamcity-faces)

(defface teamcity-build-fail
  '((t :foreground "red2"))
  "Failed build."
  :group 'teamcity-faces)

(defface teamcity-build-unknown
  '((t :foreground "gray"))
  "Failed to start/canceled build."
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


(defun teamcity-send-put-request (request request-data)
  (let* ((buf (generate-new-buffer "teamcity-rest-response"))
         (url-request-method "PUT")
         (url-request-data request-data)
         (url (teamcity-get-url request)))
	  (with-current-buffer buf
      (url-retrieve-synchronously url)
      'ok)))


(defun teamcity-send-delete-request (request request-data)
  (let* ((buf (generate-new-buffer "teamcity-rest-response"))
         (url-request-method "DELETE")
         (url-request-data request-data)
         (url (teamcity-get-url request)))
	  (with-current-buffer buf
      (url-retrieve-synchronously url)
      'ok)))


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


(defun teamcity-rest-text (request)
  "Sends TeamCity REST request and returns a response as a text"
  (let* ((buf (teamcity-rest-buffer request))
         (response (with-current-buffer buf
                     (buffer-string))))
    (kill-buffer buf)
    response))


(defun teamcity-get-version ()
  (teamcity-rest-text "version"))


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
      (insert (concat "+ " (teamcity-get-field p 'name)))
      (let* ((start (point-at-bol))
             (end (point-at-eol))
             (project-id (teamcity-get-field p 'id)))
        (put-text-property start end 'teamcity-object-type 'project)
        (put-text-property start end 'id project-id)
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
               (dolist (bt (teamcity-get-field project-details 'buildTypes) nil)
                 (insert (concat "  " (teamcity-get-field bt 'name)))
                 (insert "\n")
                 (save-excursion
                   (previous-line)
                   (let ((bt-start (point-at-bol))
                       (bt-end (point-at-eol)))
                     (put-text-property bt-start bt-end 'id (teamcity-get-field bt 'id))
                     (put-text-property bt-start bt-end 'open 'teamcity-open-buildtype)
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


(defun teamcity-open-buildtype ()
  (interactive)
  (let* ((id (get-text-property (point) 'id))
         (details (teamcity-get-buildtype-details id))
         (builds (teamcity-get-builds :buildType id :count teamcity-builds-count))
         (buffer (get-buffer-create "*TeamCity: Build Configuration*")))
    (set-buffer buffer)
    (teamcity-show-bt-header details)
    (teamcity-show-bt-builds builds)
    (beginning-of-buffer)
    (teamcity-mode)
    (teamcity-buildtype-mode)
    (switch-to-buffer buffer)))


(defun teamcity-show-bt-header (bt-details)
  (message (teamcity-get-field details 'name))
  (insert (teamcity-get-field details 'name))
  (put-text-property (point-at-bol) (point-at-eol) 'face 'teamcity-buildtype-name-header)
  (insert "\n\n"))


(defun teamcity-show-bt-builds (builds)
  (let* ((start (point-at-bol))
         (max-widths (teamcity-get-max-column-width builds))
         (number-width (teamcity-get-field max-widths 'number))
         (status-width (teamcity-get-field max-widths 'status)))
    (dolist (build builds nil)
      (teamcity-show-bt-build build number-width status-width))))


(defun teamcity-get-max-column-width (builds)
  (reduce (lambda (x y)
            (let ((number-len (length (teamcity-get-field y 'number)))
                  (status-len (length (teamcity-get-field y 'status)))
                  (number-max (teamcity-get-field x 'number))
                  (status-max (teamcity-get-field x 'status)))
              (list (cons 'number (max number-max number-len))
                    (cons 'status (max status-max status-len)))))
          builds
          :initial-value (list (cons 'number 0) (cons 'status 0))))


(defun teamcity-show-bt-build (build number-width status-width)
  (let ((pinned (teamcity-is-build-pinned (teamcity-get-field build 'id))))
    (insert (concat "+ " (teamcity-ljust (teamcity-get-field build 'number) number-width)
                    "   " (teamcity-format-date (teamcity-get-field build 'start))
                    "   " (teamcity-rjust (teamcity-get-field build 'status) status-width)
                    (if pinned "    pinned")))
    (put-text-property (point-at-bol) (point-at-eol) 'face (teamcity-build-get-face build))
    (put-text-property (point-at-bol) (point-at-eol) 'id (teamcity-get-field build 'id))
    (put-text-property (point-at-bol) (point-at-eol) 'teamcity-object-type 'build)
    (put-text-property (point-at-bol) (point-at-eol) 'pinned pinned)
    (insert "\n")))


(defun teamcity-build-get-face (build)
  (let ((status (teamcity-get-field build 'status)))
    (cond ((equal status "SUCCESS") 'teamcity-build-success)
          ((or (equal status "FAILURE") (equal status "ERROR")) 'teamcity-build-fail)
          (t 'teamcity-build-unknown))))


(defun teamcity-get-buildtype-details (id)
  (let* ((request (concat "buildTypes/" id))
         (response (teamcity-rest-xml request)))
    (teamcity-parse-buildtype-details response)))


(defun teamcity-parse-buildtype-details (xml)
  (let* ((root (car xml))
         (id (xml-get-attribute root 'id))
         (name (xml-get-attribute root 'name))
         (webUrl (xml-get-attribute root 'webUrl)))
    (list (cons 'id id)
          (cons 'name name)
          (cons 'weburl webUrl))))


(defun teamcity-builds-request (&rest build-locator)
  (concat "builds?locator="
          (teamcity-serialize-locator build-locator)))


(defun teamcity-serialize-locator (locator)
  (teamcity-serialize-locator* locator ""))

(defun teamcity-serialize-locator* (locator result)
  (if (not locator)
      result
    (let* ((head (car locator))
           (tail (cdr locator))
           (dimension (and (symbolp head)
                           (equal (substring (symbol-name head) 0 1) ":")
                           (substring (symbol-name head) 1)))
           (coma (if (> (length result) 0) "," "")))
      (if (and dimension tail)
          (let* ((val (car tail))
                 (strval (if (stringp val) val (number-to-string val))))
            (teamcity-serialize-locator*
             (cdr tail)
             (concat result (and (> (length result) 0) ",") dimension ":" strval)))
        (teamcity-serialize-locator* tail result)))))


(defun teamcity-get-builds (&rest build-locator)
  (let* ((request (apply 'teamcity-builds-request build-locator))
         (response (teamcity-rest-xml request)))
    (teamcity-parse-builds response)))


(defun teamcity-parse-builds (xml)
  (let* ((root (car xml))
         (builds (xml-node-children root)))
    (mapcar* 'teamcity-parse-build builds)))


(defun teamcity-parse-build (xml)
  (let ((id (xml-get-attribute xml 'id))
        (number (xml-get-attribute xml 'number))
        (status (xml-get-attribute xml 'status))
        (start (xml-get-attribute xml 'startDate)))
    (list (cons 'id id)
          (cons 'number number)
          (cons 'status status)
          (cons 'start (teamcity-parse-date start)))))


(defun teamcity-parse-date (date)
  (let ((year (substring date 0 4))
        (month (substring date 4 6))
        (day (substring date 6 8))
        (hour (substring date 9 11))
        (min (substring date 11 13))
        (sec (substring date 13 15)))
    (list (cons 'year year)
          (cons 'month month)
          (cons 'day day)
          (cons 'hour hour)
          (cons 'min min)
          (cons 'sec sec))))


(defun teamcity-format-date (date)
  (concat (teamcity-get-field date 'day) "."
          (teamcity-get-field date 'month) "."
          (teamcity-get-field date 'year) " "
          (teamcity-get-field date 'hour) ":"
          (teamcity-get-field date 'min) ":"
          (teamcity-get-field date 'sec)))


(defun teamcity-get-next-object-point (object-type lines-forward)
  (save-excursion
    (move-beginning-of-line (+ lines-forward 1))
    (while (and (not (eobp))
                (not (eq (teamcity-object-type-at-point) object-type)))
      (next-line))
    (point)))


(defun teamcity-object-type-at-point()
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


(defun teamcity-get-field (object field-symbol)
  (cdr (assoc field-symbol object)))


(defvar teamcity-mode-hook nil)


(defvar teamcity-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "RET") 'teamcity-open-new-window)
    (define-key map (kbd "TAB") 'teamcity-expand-collapse)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "u") 'cua-scroll-down)
    (define-key map (kbd "d") 'cua-scroll-up)
    map))


(defvar teamcity-buildtype-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'teamcity-toggle-build-pin)
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


(define-minor-mode teamcity-buildtype-mode
    "Minor mode to view a build type details."
  :group teamcity
  :init-value ()
  :lighter ()
  :keymap teamcity-buildtype-mode-map)

(defun teamcity-open-new-window ()
  (interactive)
  (let* ((open (get-text-property (point) 'open)))
    (if open
        (funcall open)
      (message "There is nothing to open here"))))


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
          (expand
           (funcall expand)
           (put-text-property start end 'expanded 'yes))
          (t (message "There is nothing to expand here")))))


(defun teamcity-ljust (str width)
  (let ((len (length str)))
    (cond ((<= len width)
           (concat str (make-string (- width len) ? )))
          (t
           (substring str 0 width)))))


(defun teamcity-rjust (str width)
  (let ((len (length str)))
    (cond ((<= len width)
           (concat (make-string (- width len) ? ) str))
          (t
           (substring str (- len width) len)))))


(defun teamcity-get-build-pin-request (build-id)
  (concat "builds/id:" build-id "/pin/"))


(defun teamcity-pin-build (build-id comment)
  (let ((request (teamcity-get-build-pin-request build-id)))
    (teamcity-send-put-request request comment)))


(defun teamcity-unpin-build (build-id comment)
  (let ((request (teamcity-get-build-pin-request build-id)))
    (teamcity-send-delete-request request comment)))


(defun teamcity-is-build-pinned (build-id)
  (let* ((request (teamcity-get-build-pin-request build-id))
         (response (teamcity-rest-text request)))
    (string= response "true")))


(defun teamcity-toggle-build-pin ()
  (interactive)
  (let ((type (teamcity-object-type-at-point)))
    (if (not (eq type 'build))
        (message "Nothing to pin")
      (let ((build-id (get-text-property (point) 'id))
            (pinned (get-text-property (point) 'pinned))
            (comment (read-string (if pinned "Unpin comment: " "Pin comment: "))))
        (if pinned
            (teamcity-unpin-build build-id comment)
          (teamcity-pin-build build-id comment))))))


(provide 'teamcity)