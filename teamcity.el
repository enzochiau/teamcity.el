;;;; Emacs TeamCity Client

(require 'cl)
(require 'thread-dump)

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

(defcustom teamcity-autorefresh-interval-sec 0
  "Autorefresh interval in seconds."
  :group 'teamcity
  :type 'int)

(defcustom teamcity-thread-dump-dir "~/tmp"
  "Directory to store thread dumps."
  :group 'teamcity
  :type 'directory)

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
         (projects (teamcity-get-projects))
         (pos (and (eq (current-buffer) projects-buffer) (point)))
         (inhibit-read-only t))
    (set-buffer projects-buffer)
    (erase-buffer)
    (dolist (p projects nil)
      (teamcity-show-project p))
    (goto-char (or pos (point-min)))
    (teamcity-mode)
    (switch-to-buffer projects-buffer)
    (teamcity-set-refresh-fn 'teamcity-projects)))


(defun teamcity-show-project (p)
  (insert
   (propertize (concat "+ " (teamcity-get-field p 'name) "\n")
               'teamcity-object-type 'project
               'id (teamcity-get-field p 'id)
               'expand 'teamcity-project-expand
               'collapse 'teamcity-project-collapse
               'weburl-fn 'teamcity-project-get-weburl
               'face 'teamcity-project)))


(defun teamcity-project-get-weburl ()
  (let* ((project-id (get-text-property (point) 'id))
         (project-details (teamcity-project-get-details project-id)))
    (teamcity-get-field project-details 'webUrl)))


(defun teamcity-project-get-details (id)
  (let* ((project-details-request (concat "projects/id:" project-id))
         (project-details-xml (teamcity-rest-xml project-details-request)))
    (teamcity-parse-project-details project-details-xml)))


(defun teamcity-project-expand ()
  (interactive)
  (save-excursion
    (let ((start (point-at-bol))
          (end (point-at-eol))
          (details-loaded (get-text-property (point) 'project-details-loaded)))
      (cond ((eq details-loaded 'yes)
             (let ((contents (get-text-property (point) 'contents)))
               (move-beginning-of-line 2)
               (insert contents)))
            (t
             (let* ((project-id (get-text-property (point) 'id))
                    (project-details (teamcity-project-get-details project-id)))
               (move-beginning-of-line 2)
               (dolist (bt (teamcity-get-field project-details 'buildTypes) nil)
                 (insert (propertize (concat "  " (teamcity-get-field bt 'name) "\n")
                                     'id (teamcity-get-field bt 'id)
                                     'open 'teamcity-open-buildtype
                                     'face 'teamcity-buildtype
                                     'weburl (teamcity-get-field bt 'webUrl))))
               (put-text-property start end 'project-details-loaded 'yes)
               (put-text-property start end 'weburl (teamcity-get-field project-details 'webUrl)))))
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
  (let ((id (get-text-property (point) 'id)))
    (teamcity-show-buildtype id)))


(defun teamcity-show-buildtype (btid)
  (let* ((buffer (get-buffer-create (concat "*TeamCity: Build Configuration " btid "*"))))
    (teamcity-show-buildtype* btid buffer)
    (switch-to-buffer buffer)))


(defun teamcity-show-buildtype* (btid buffer)
  (let* ((details (teamcity-get-buildtype-details btid))
         (builds (teamcity-get-builds :buildType btid :count teamcity-builds-count))
         (running-builds (teamcity-get-builds :buildType btid :running "true")))
    (teamcity-show-buildtype** btid buffer details builds running-builds)))


(defun teamcity-show-buildtype** (btid buffer details builds running-builds)
  (let* ((pos (and (eq (current-buffer) buffer) (point)))
         (inhibit-read-only t))
    (set-buffer buffer)
    (teamcity-turn-off-auto-refresh)
    (erase-buffer)
    (teamcity-show-bt-header details)
    (teamcity-show-bt-builds running-builds "Running builds:")
    (insert "\n\n")
    (teamcity-show-bt-builds builds "History:")
    (goto-char (or pos (point-min)))
    (teamcity-mode)
    (teamcity-buildtype-mode)
    (make-local-variable 'teamcity-buildtype-id)
    (setq teamcity-buildtype-id btid)
    (make-local-variable 'teamcity-weburl)
    (setq teamcity-weburl (teamcity-get-field details 'webUrl))
    (teamcity-set-refresh-fn 'teamcity-refresh-buildtype)
    (when (> teamcity-autorefresh-interval-sec 0)
      (make-local-variable 'teamcity-autorefresh-timer)
      (setq teamcity-autorefresh-timer
            (run-at-time (concat (number-to-string teamcity-autorefresh-interval-sec) " sec")
                         nil
                         (teamcity-mk-buildtype-refresh-fn btid buffer details)))
      (add-hook 'kill-buffer-hook 'teamcity-turn-off-auto-refresh))))


(defun teamcity-refresh-buildtype ()
  (let ((id (buffer-local-value 'teamcity-buildtype-id (current-buffer))))
    (teamcity-show-buildtype id)))


(defun teamcity-mk-buildtype-refresh-fn (btid buffer details)
  (lexical-let ((id btid)
                (buf buffer)
                (det details))
    (lambda ()
      (teamcity-get-builds-async (teamcity-build-locator :buildType id :count teamcity-builds-count)
                               (lambda (builds)
                                 (lexical-let ((builds builds))
                                   (teamcity-get-builds-async (teamcity-build-locator :buildType id :running "true")
                                                              (lambda (running-builds)
                                                                (lexical-let ((running-builds running-builds))
                                                                  (teamcity-show-buildtype** id buf det builds running-builds))))))))))


(defun teamcity-show-bt-header (bt-details)
  (insert (teamcity-buildtype-get-fullname bt-details))
  (put-text-property (point-at-bol) (point-at-eol) 'face 'teamcity-buildtype-name-header)
  (insert "\n\n\n"))


(defun teamcity-show-bt-builds (builds &optional header)
  (let* ((start (point-at-bol))
         (max-widths (teamcity-get-max-column-width builds))
         (number-width (teamcity-get-field max-widths 'number))
         (status-width (teamcity-get-field max-widths 'status)))
    (if header (insert (concat header "\n")))
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
  (let ((percent-str (teamcity-get-build-percentage-str build)))
    (insert
     (propertize (teamcity-get-bt-build-line build number-width status-width percent-str)
                 'face (teamcity-build-get-face build)
                 'id (teamcity-get-field build 'id)
                 'teamcity-object-type 'build
                 'build build
                 'number-width number-width
                 'status-width status-width
                 'weburl (teamcity-get-field build 'webUrl)))))


(defun teamcity-get-bt-build-line (build number-width status-width percent-str)
  (concat "+ "  (teamcity-ljust (teamcity-get-field build 'number) number-width)
          "   " (teamcity-format-date (teamcity-get-field build 'start))
          "   " (teamcity-rjust (teamcity-get-field build 'status) status-width)
          (if percent-str (concat "   " percent-str))
          "\n"))


(defun teamcity-get-build-percentage-str (build)
  (let ((percentage (teamcity-get-field build 'percentage)))
    (if percentage
        (let ((p (/ (string-to-number percentage) 10)))
          (concat "["
                  (make-string p ?#)
                  (make-string (- 10 p) ? )
                  "] "
                  percentage
                  "%")))))


(defun teamcity-refresh-build-at (point)
  (let ((inhibit-read-only t)
        (number-width (get-text-property (point) 'number-width))
        (status-width (get-text-property (point) 'status-width))
        (build (get-text-property (point) 'build))
        (start (point-at-bol)))
    (teamcity-remove-build-at point)
    (goto-char start)
    (teamcity-show-bt-build build number-width status-width)
    (goto-char start)))


(defun teamcity-remove-build-at (point)
  (let ((start (point-at-bol))
        (end (+ (point-at-eol) 1)))
    (delete-region start end)))


(defun teamcity-build-get-face (build)
  (let ((status (teamcity-get-field build 'status)))
    (cond ((equal status "SUCCESS") 'teamcity-build-success)
          ((or (equal status "FAILURE") (equal status "ERROR")) 'teamcity-build-fail)
          (t 'teamcity-build-unknown))))


(defun teamcity-get-buildtype-details (id)
  (let* ((request (concat "buildTypes/" id))
         (response (teamcity-rest-xml request)))
    (teamcity-parse-buildtype-details response)))


(defun teamcity-get-buildtype-details-async (id callback)
    (let ((request (concat "buildTypes/" id)))
      (teamcity-rest-xml-async
       request
       (lexical-let ((cbk callback))
         (lambda (xml)
          (let ((details (teamcity-parse-buildtype-details xml)))
            (funcall cbk details)))))))


(defun teamcity-parse-buildtype-details (xml)
  (let* ((root (car xml))
         (id (xml-get-attribute root 'id))
         (name (xml-get-attribute root 'name))
         (project-name (xml-get-attribute (car (xml-get-children root 'project)) 'name))
         (webUrl (xml-get-attribute root 'webUrl)))
    (list (cons 'id id)
          (cons 'name name)
          (cons 'project-name project-name)
          (cons 'webUrl webUrl))))


(defun teamcity-buildtype-get-fullname (bt)
  (let ((name (teamcity-get-field bt 'name))
        (project-name (teamcity-get-field bt 'project-name)))
    (concat project-name " :: " name)))


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


(defun teamcity-get-builds-async (build-locator callback)
  (let ((request (apply 'teamcity-builds-request build-locator)))
    (teamcity-rest-xml-async
     request
     (lexical-let ((cbk callback))
       (lambda (xml)
         (let* ((builds (teamcity-parse-builds xml))
                (builds-with-pin nil))
           (funcall cbk builds)))))))


(defun teamcity-build-locator (&rest build-locator)
  build-locator)


(defun teamcity-parse-builds (xml)
  (let* ((root (car xml))
         (builds (xml-node-children root)))
    (mapcar* 'teamcity-parse-build builds)))


(defun teamcity-parse-build (xml)
  (let ((id (xml-get-attribute xml 'id))
        (number (xml-get-attribute xml 'number))
        (status (xml-get-attribute xml 'status))
        (start (xml-get-attribute xml 'startDate))
        (percentage (xml-get-attribute-or-nil xml 'percentageComplete))
        (webUrl (xml-get-attribute-or-nil xml 'webUrl)))
    (list (cons 'id id)
          (cons 'number number)
          (cons 'status status)
          (cons 'start (teamcity-parse-date start))
          (cons 'percentage percentage)
          (cons 'webUrl webUrl))))


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
    (define-key map (kbd "g") 'teamcity-refresh)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "u") 'scroll-down)
    (define-key map (kbd "d") 'scroll-up)
    (define-key map (kbd "v") 'teamcity-visit-in-browser)
    map))


(defvar teamcity-buildtype-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'teamcity-run-build)
    map))


(defun teamcity-mode ()
  "TeamCity major mode"
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
  (let ((point (point))
        (type (teamcity-object-type-at-point)))
    (if (not (eq type 'build))
        (message "Nothing to pin")
      (let* ((build-id (get-text-property point 'id))
             (pinned (get-text-property point 'pinned))
             (comment (read-string (if pinned "Unpin comment: " "Pin comment: "))))
        (if pinned
            (teamcity-unpin-build build-id comment)
          (teamcity-pin-build build-id comment))
        (teamcity-refresh-build-at point)))))


(defun teamcity-refresh ()
  (interactive)
  (if (local-variable-p 'teamcity-refresh-fn)
      (funcall (buffer-local-value 'teamcity-refresh-fn (current-buffer)))
    (message "Nothing to refresh")))


(defun teamcity-set-refresh-fn (refresh-fn)
  (make-local-variable 'teamcity-refresh-fn)
  (setq teamcity-refresh-fn refresh-fn))


(defun teamcity-run-build ()
  (interactive)
  (let ((id (buffer-local-value 'teamcity-buildtype-id (current-buffer))))
    (teamcity-run-build-for-buildtype id)))


(defun teamcity-run-build-for-buildtype (id)
  (let ((response-buffer (generate-new-buffer "teamcity-rest-response")))
    (save-current-buffer
      (set-buffer response-buffer)
      (url-insert-file-contents
       (concat "http://" teamcity-username "@" teamcity-server "/httpAuth/action.html?add2Queue=" id)))
    'ok))


;; Transport:

(defun teamcity-mk-rest-url-string (rest-request)
  (concat "http://" teamcity-username "@" teamcity-server
          "/httpAuth/app/rest/" rest-request))


(defun teamcity-get-rest-url (rest-request)
  (url-generic-parse-url (concat (teamcity-mk-rest-url-string rest-request))))


(defun teamcity-rest-buffer (request)
  "Sends TeamCity REST request and returns a buffer with response"
  (let ((response-buffer (generate-new-buffer "teamcity-rest-response")))
    (save-current-buffer
      (set-buffer response-buffer)
      (url-insert-file-contents (teamcity-mk-rest-url-string request)))
    response-buffer))


(defun teamcity-send-put-request (request request-data)
  (let* ((buf (generate-new-buffer "teamcity-rest-response"))
         (url-request-method "PUT")
         (url-request-data request-data)
         (url (teamcity-get-rest-url request)))
	  (with-current-buffer buf
      (url-retrieve-synchronously url)
      'ok)))


(defun teamcity-send-delete-request (request request-data)
  (let* ((buf (generate-new-buffer "teamcity-rest-response"))
         (url-request-method "DELETE")
         (url-request-data request-data)
         (url (teamcity-get-rest-url request)))
	  (with-current-buffer buf
      (url-retrieve-synchronously url)
      'ok)))


(defun teamcity-rest-xml (request)
  "Sends TeamCity REST request and returns a parsed xml"
  (let* ((buf (teamcity-rest-buffer request))
         (xml (with-current-buffer buf
                (xml-parse-region (point-min)
                                  (point-max)
                                  (current-buffer)))))
    (kill-buffer buf)
    xml))


(defun teamcity-rest-xml-async (request callback)
  (let ((url (teamcity-get-rest-url request))
        (url-show-status nil))
    (url-retrieve
     url
     (teamcity-mk-xml-callback callback))))


(defun teamcity-mk-xml-callback (callback)
  (lexical-let ((cbk callback))
    (lambda (x) (teamcity-invoke-callback-with-xml cbk))))


(defun teamcity-invoke-callback-with-xml (callback)
  (teamcity-delete-http-headers)
  (let ((xml (xml-parse-region (point-min) (point-max) (current-buffer))))
    (funcall callback xml)))


(defun teamcity-delete-http-headers ()
  "Delete http headers from current buffer"
  (goto-char (point-min))
  (when (looking-at "^HTTP/1.*$")
        (re-search-forward "^$" nil t 1)
        (setq headers (buffer-substring-no-properties (point-min) (point))))
  (delete-region (+ (point) 1) (point-min))
  headers)


(defun teamcity-rest-text (request)
  "Sends TeamCity REST request and returns a response as a text"
  (let* ((buf (teamcity-rest-buffer request))
         (response (with-current-buffer buf
                     (buffer-string))))
    (kill-buffer buf)
    response))


(defun teamcity-visit-in-browser ()
  "Open object at point in the browser"
  (interactive)
  (let ((url (teamcity-get-weburl)))
    (if url
        (browse-url url)
      (message "Nothing to visit"))))


(defun teamcity-get-weburl ()
  (or (get-text-property (point) 'weburl)
      (and (get-text-property (point) 'weburl-fn) (funcall (get-text-property (point) 'weburl-fn)))
      (and (local-variable-p 'teamcity-weburl (current-buffer))
           (buffer-local-value 'teamcity-weburl (current-buffer)))))


(defun teamcity-turn-off-auto-refresh ()
  (let ((timer (and (local-variable-p 'teamcity-autorefresh-timer (current-buffer))
                    (buffer-local-value 'teamcity-autorefresh-timer (current-buffer)))))
    (if timer
        (cancel-timer timer))))


(defun teamcity-show-thread-dump ()
  "Show TeamCity server thread dump"
  (interactive)
  (let* ((thread-dump-buffer-name (teamcity-thread-dump-name))
         (thread-dump-buffer (get-buffer-create thread-dump-buffer-name)))
    (set-buffer thread-dump-buffer)
    (url-insert-file-contents (teamcity-thread-dump-url))
    (switch-to-buffer thread-dump-buffer)
    (thread-dump-mode)))

(defun teamcity-save-thread-dump (start-time)
  (with-temp-buffer
    (let ((current-date (format-time-string "%y.%m.%d"))
          (thread-dump-name (teamcity-thread-dump-name)))
      (url-insert-file-contents (teamcity-thread-dump-url))
      (set-visited-file-name (expand-file-name (concat teamcity-thread-dump-dir "/" current-date "/" start-time "/" thread-dump-name)))
      (save-buffer))))

(defun teamcity-show-thread-dump-and-schedule (start-time counter)
  (message start-time)
  (teamcity-save-thread-dump start-time)
  (if (< counter 10)
      (run-at-time "1 sec" nil 'teamcity-show-thread-dump-and-schedule start-time (+ counter 1))))

(defun teamcity-thread-dump-name ()
  (concat "TeamCity-thread-dump " (format-time-string "%y.%m.%d %H:%M:%S")))

(defun teamcity-thread-dump-url () 
  (concat "http://" teamcity-username "@" teamcity-server "/httpAuth/admin/diagnostic.html?actionName=threadDump&save=false"))

(defun teamcity-profile ()
  (interactive)
  (run-at-time "2 sec" nil 'teamcity-show-thread-dump-and-schedule (format-time-string "%H_%M") 0))

(provide 'teamcity)