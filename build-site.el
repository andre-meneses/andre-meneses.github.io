;; Publishing system
(require 'package)

(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(package-install 'htmlize)

;; Dependencies
(require 'ox)
(require 'ox-html)
(require 'ox-publish)

;; Add local lisp directory to load path
(add-to-list 'load-path (expand-file-name "./lisp/"))

;; Load ox-tufte
(condition-case err
    (progn
      (load-file "./lisp/ox-tufte.el")
      (message "ox-tufte loaded successfully"))
  (error (message "Failed to load ox-tufte: %S" err)))


;; Add ID resolution support for org-roam links
(require 'org-id)

;; Set up ID locations file
(setq org-id-locations-file (expand-file-name "./.org-id-locations"))
(setq org-id-track-globally t)
(setq org-id-extra-files (directory-files-recursively "./content/" "\\.org$"))

;; Generate ID locations database
(org-id-update-id-locations org-id-extra-files)

;; Configure org-id link export for HTML
(org-link-set-parameters "id"
                        :export (lambda (id desc format _)
                          (cond
                           ((eq format 'html)
                            (let* ((file (org-id-find-id-file id))
                                  (rel-file (when file
                                              (file-relative-name file (expand-file-name "./content/"))))
                                  (html-file (when rel-file
                                              (concat (file-name-sans-extension rel-file) ".html"))))
                              (if html-file
                                 (format "<a href=\"%s\">%s</a>" html-file (or desc id))
                                (format "<a href=\"#%s\">%s</a>" id (or desc id)))))
                           (t nil))))

;; Ensure org-tufte correctly handles ID links
(defun my/org-tufte-handle-id-link (link desc info)
  "Handle ID links specifically for Tufte HTML export."
  (let* ((id (org-element-property :path link))
         (file (org-id-find-id-file id))
         (current-file (plist-get info :input-file))
         ;; Get target file basename (without directory or extension)
         (target-basename (when file
                            (file-name-base file)))
         ;; Check if the current file is in posts directory
         (in-posts (and current-file (string-match "/posts/" current-file)))
         ;; Check if target file is in posts directory
         (target-in-posts (and file (string-match "/posts/" file))))
    (cond
     ;; Both in posts - just use basename
     ((and in-posts target-in-posts)
      (format "<a href=\"%s.html\">%s</a>" target-basename (or desc id)))
     ;; In posts linking to main page
     ((and in-posts (not target-in-posts))
      (format "<a href=\"../%s.html\">%s</a>" target-basename (or desc id)))
     ;; In main page linking to posts
     ((and (not in-posts) target-in-posts)
      (format "<a href=\"posts/%s.html\">%s</a>" target-basename (or desc id)))
     ;; Both in main pages
     (t
      (format "<a href=\"%s.html\">%s</a>" target-basename (or desc id))))))

;; Install the handler by advising the tufte link handler
(defun my/enhance-tufte-link-handling (orig-fun link desc info)
  "Advice to enhance ox-tufte link handling with better ID support."
  (if (string= (org-element-property :type link) "id")
      (my/org-tufte-handle-id-link link desc info)
    (funcall orig-fun link desc info)))

(advice-add 'org-tufte-maybe-margin-note-link :around #'my/enhance-tufte-link-handling)

;; Post management functions
(defun my/get-post-date (file)
  "Extract date from post FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+DATE: <\\([^>]+\\)>" nil t)
      (date-to-time (match-string 1)))))

(defun my/get-post-title (file)
  "Extract title from post FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE: \\(.+\\)" nil t)
      (match-string 1))))

(defun my/get-post-description (file)
  "Extract description from post FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+DESCRIPTION: \\(.+\\)" nil t)
        (match-string 1)
      ;; If no description, try to get first paragraph
      (goto-char (point-min))
      (when (re-search-forward "^\\* \\|^#\\+" nil t)
        (when (re-search-forward "^\\([^#*\n][^\n]+\\)" nil t)
          (let ((first-para (match-string 1)))
            (if (> (length first-para) 160)
                (concat (substring first-para 0 157) "...")
              first-para)))))))

(defun my/get-post-link (file)
  "Generate HTML link for post FILE."
  (let* ((title (or (my/get-post-title file) "Untitled"))
         (date (my/get-post-date file))
         (date-str (format-time-string "%Y-%m-%d" date))
         (href (concat "posts/" (file-name-sans-extension (file-name-nondirectory file)) ".html")))
    (format "<li><span class=\"post-date\">%s</span> - <a href=\"%s\">%s</a></li>\n"
            date-str href title)))

(defun my/format-post-archive-entry (file)
  "Generate HTML for a post archive entry."
  (let* ((title (or (my/get-post-title file) "Untitled"))
         (date (my/get-post-date file))
         (date-str (format-time-string "%B %d, %Y" date))
         (year (format-time-string "%Y" date))
         (description (or (my/get-post-description file) ""))
         (href (concat (file-name-sans-extension (file-name-nondirectory file)) ".html")))
    (format "<article class=\"post-entry\">
              <header>
                <h3><a href=\"%s\">%s</a></h3>
                <time datetime=\"%s\">%s</time>
              </header>
              <div class=\"post-description\">
                %s
              </div>
            </article>"
            href title
            (format-time-string "%Y-%m-%d" date) date-str
            description)))

(defun my/generate-posts-archive ()
  "Generate HTML for the complete posts archive."
  (let* ((posts-dir (expand-file-name "./content/posts/"))
         (posts (directory-files posts-dir t "\\.org$"))
         (valid-posts
          (seq-filter
           (lambda (file)
             (and (my/get-post-date file)
                  (my/get-post-title file)
                  (not (string= (file-name-nondirectory file) "index.org"))))
           posts))
         (sorted-posts
          (sort valid-posts
                (lambda (a b)
                  (time-less-p (my/get-post-date b)
                              (my/get-post-date a)))))
         (posts-by-year (seq-group-by 
                        (lambda (file)
                          (format-time-string "%Y" (my/get-post-date file)))
                        sorted-posts)))
    (if sorted-posts
        (mapconcat
         (lambda (year-group)
           (let ((year (car year-group))
                 (year-posts (cdr year-group)))
             (concat
              (format "<section class=\"archive-year\">
                        <h2>%s</h2>" year)
              (mapconcat #'my/format-post-archive-entry year-posts "\n")
              "</section>")))
         posts-by-year
         "\n")
      "<p>No posts yet.</p>")))



;; Generate navbar with correct paths
(defun my/generate-navbar (info)
  "Generate the navigation bar HTML."
  (let* ((input-file (plist-get info :input-file))
         (in-posts (and input-file (string-match "/posts/" input-file)))
         (prefix (if in-posts "../" "")))
    (concat
     "<nav class=\"navbar\">"
     "<ul>"
     (format "<li><a href=\"%sindex.html\">Home</a></li>" prefix)
     (format "<li><a href=\"%sabout.html\">About</a></li>" prefix)
     (format "<li><a href=\"%sposts/index.html\">Posts</a></li>" prefix)
     "</ul>"
     "</nav>")))

;; Basic HTML settings
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil)

;; Project-specific settings for different page types
(defun my/get-project-plist (project-name)
  "Get project-specific settings"
  (let ((base-plist
         `(:publishing-function org-tufte-publish-to-html
           :with-author t
           :with-creator t
           :with-toc nil
           :section-numbers nil
           :time-stamp-file nil
           :html-head ,(cond
                       ((string= project-name "posts")
                        (concat
                         "<link rel=\"stylesheet\" href=\"../tufte.css\" type=\"text/css\" />\n"
                         "<link rel=\"stylesheet\" href=\"../ox-tufte.css\" type=\"text/css\" />\n"
                         "<style>
                          /* Navbar styles */
                          .navbar { 
                            width: 100%; 
                            padding: 0.25rem 0;
                            border-bottom: 1px solid #ccc;
                            margin-bottom: 0.5rem;
                            margin-top: 2.5rem;
                            font-family: et-book, Palatino, \"Palatino Linotype\", \"Palatino LT STD\", \"Book Antiqua\", Georgia, serif;
                          }
                          .navbar ul { 
                            list-style: none; 
                            padding: 0; 
                            margin: 0; 
                            display: flex; 
                            gap: 2rem; 
                          }
                          .navbar li { 
                            margin: 0;
                            font-size: 1.4rem;
                          }
                          .navbar a { 
                            text-decoration: none;
                            font-style: italic;
                          }
                          .navbar a:hover { 
                            text-decoration: underline; 
                          }

			  h1.title {
			       margin-top: 1rem;
			  }

			  h1 + p, 
			  .subtitle + p,
			  h1 + h2,
			  .title + p {
			    margin-top: 0.5rem;
			  }

                          #content {
                             padding-top:2.5rem
                          } 
                          
                          /* Posts list on homepage */
                          .posts-list { width: 65%; }
                          .posts-list li { margin-bottom: 1rem; }
                          .more-posts { margin-top: 2rem; }
                          
                          /* Posts archive page */
                          .posts-archive { width: 65%; }
                          .archive-year { margin-bottom: 0.1rem; }
                          .archive-year h2 {
                            font-family: et-book, Palatino, \"Palatino Linotype\", \"Palatino LT STD\", \"Book Antiqua\", Georgia, serif;
                            font-style: italic;
                            font-weight: 400;
                            font-size: 2.2rem;
                            margin-top: 1.5rem;
                            margin-bottom: 0.5rem;
                            line-height: 1;
                            border-bottom: 1px solid #ccc;
                            padding-bottom: 0.1rem;
                          }
                          .post-entry {
                            margin-bottom: 2rem;
                            padding-bottom: 1.5rem;
                            padding-top: 1.5rem;
                          }
                          .post-entry:not(:last-child) {
                            border-bottom: 1px solid #eee;
                          }
                          .post-entry header {
                            display: flex;
                            justify-content: space-between;
                            align-items: baseline;
                            margin-bottom: 0.5rem;
                          }
                          .post-entry h3 {
                            margin: 0;
                            font-style: italic;
                            font-size: 1.7rem;
                            font-weight: 400;
                          }
                          .post-entry h3 a {
                            text-decoration: none;
                          }
                          .post-entry h3 a:hover {
                            text-decoration: underline;
                          }
                          .post-entry time {
                            font-family: et-book-roman-old-style;
                            color: #666;
                            font-size: 1.1rem;
                            margin-left: 1rem;
                          }
                          .post-description {
                            line-height: 1.6;
                            font-size: 1.4rem;
                          }
                          .post-date {
                            font-family: et-book-roman-old-style;
                            color: #666;
                          }
                          @media (max-width: 760px) {
                            .posts-archive { width: 100%; }
                            .post-entry header {
                              flex-direction: column;
                            }
                            .post-entry time {
                              margin-left: 0;
                              margin-top: 0.5rem;
                            }
                          }
                          @media (prefers-color-scheme: dark) {
                            .post-date { color: #999; }
                            .navbar { border-bottom-color: #333; }
                            .archive-year h2 { border-bottom-color: #333; }
                            .post-entry:not(:last-child) { border-bottom-color: #333; }
                            .post-entry time { color: #999; }
                          }
                         </style>"))
                       (t
                        (concat
                         "<link rel=\"stylesheet\" href=\"tufte.css\" type=\"text/css\" />\n"
                         "<link rel=\"stylesheet\" href=\"ox-tufte.css\" type=\"text/css\" />\n"
                         "<style>
                          /* Navbar styles */
                          .navbar { 
                            width: 100%; 
                            padding: 0.25rem 0;
                            border-bottom: 1px solid #ccc;
                            margin-bottom: 0.5rem;
                            margin-top: 2.5rem;
                            font-family: et-book, Palatino, \"Palatino Linotype\", \"Palatino LT STD\", \"Book Antiqua\", Georgia, serif;
                          }
                          .navbar ul { 
                            list-style: none; 
                            padding: 0; 
                            margin: 0; 
                            display: flex; 
                            gap: 2rem; 
                          }
                          .navbar li { 
                            margin: 0;
                            font-size: 1.4rem;
                          }
                          .navbar a { 
                            text-decoration: none;
                            font-style: italic;
                          }
                          .navbar a:hover { 
                            text-decoration: underline; 
                          }

			  h1.title {
			       margin-top: 1rem;
			  }

			  h1 + p, 
			  .subtitle + p,
			  h1 + h2,
			  .title + p {
			    margin-top: 0.5rem;
			  }

			 #content {
                             padding-top:2.5rem
                          }
                          
                          /* Posts list on homepage */
                          .posts-list { width: 75%; }
                          .posts-list li { margin-bottom: 1rem; }
                          .more-posts { margin-top: 2rem; }
                          
                          /* Posts archive page */
                          .posts-archive { width: 65%; }
                          .archive-year { margin-bottom: 0.1rem; }
                          .archive-year h2 {
                            font-family: et-book, Palatino, \"Palatino Linotype\", \"Palatino LT STD\", \"Book Antiqua\", Georgia, serif;
                            font-style: italic;
                            font-weight: 400;
                            font-size: 2.2rem;
                            margin-top: 2.1rem;
                            margin-bottom: 0.7rem;
                            line-height: 1;
                            border-bottom: 1px solid #ccc;
                            padding-bottom: 0.5rem;
                          }
                          .post-entry {
                            margin-bottom: 2rem;
                            padding-bottom: 1.5rem;
                          }
                          .post-entry:not(:last-child) {
                            border-bottom: 1px solid #eee;
                          }
                          .post-entry header {
                            display: flex;
                            justify-content: space-between;
                            align-items: baseline;
                            margin-bottom: 0.5rem;
                          }
                          .post-entry h3 {
                            margin: 0;
                            font-style: italic;
                            font-size: 1.7rem;
                            font-weight: 400;
                          }
                          .post-entry h3 a {
                            text-decoration: none;
                          }
                          .post-entry h3 a:hover {
                            text-decoration: underline;
                          }
                          .post-entry time {
                            font-family: et-book-roman-old-style;
                            color: #666;
                            font-size: 1.1rem;
                            margin-left: 1rem;
                          }
                          .post-description {
                            line-height: 1.6;
                            font-size: 1.4rem;
                          }
                          .post-date {
                            font-family: et-book-roman-old-style;
                            color: #666;
                          }
                          @media (max-width: 760px) {
                            .posts-archive { width: 100%; }
                            .post-entry header {
                              flex-direction: column;
                            }
                            .post-entry time {
                              margin-left: 0;
                              margin-top: 0.5rem;
                            }
                          }
                          @media (prefers-color-scheme: dark) {
                            .post-date { color: #999; }
                            .navbar { border-bottom-color: #333; }
                            .archive-year h2 { border-bottom-color: #333; }
                            .post-entry:not(:last-child) { border-bottom-color: #333; }
                            .post-entry time { color: #999; }
                          }
                         </style>")))
           :html-preamble my/generate-navbar)))
    (cond
     (t base-plist))))

;; Publishing configuration
(setq org-publish-project-alist
      `(("pages"
         :recursive nil
         :base-directory "./content"
         :publishing-directory "./public"
         ,@(my/get-project-plist "pages"))
        
        ("posts"
         :recursive t
         :base-directory "./content/posts"
         :publishing-directory "./public/posts"
         ,@(my/get-project-plist "posts"))

        ("static"
         :base-directory "./content"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "./public"
         :recursive t
         :publishing-function org-publish-attachment)

        ("website" :components ("pages" "posts" "static"))))

(defun my/process-index-file ()
  "Process the main index file to include recent posts."
  (let ((index-file (expand-file-name "./content/index.org")))
    (with-temp-buffer
      (insert-file-contents index-file)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Latest Posts" nil t)
        (delete-region (point) (or (re-search-forward "^\\*" nil t)
                                 (point-max)))
        (insert "\n\n")
        (insert (my/generate-posts-list))
        (insert "\n"))
      (write-region (point-min) (point-max) index-file))))

(defun my/process-posts-index ()
  "Process the posts index file to include the archive."
  (let ((posts-index (expand-file-name "./content/posts/index.org")))
    (with-temp-buffer
      (insert-file-contents posts-index)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+begin_export html" nil t)
        (let ((start (point)))
          (when (re-search-forward "^#\\+end_export" nil t)
            (beginning-of-line)
            (delete-region start (point))
            (insert "\n<div class=\"posts-archive\">\n"
                   (my/generate-posts-archive)
                   "</div>\n"))))
      (write-region (point-min) (point-max) posts-index))))


(defun my/generate-posts-list ()
  "Generate HTML list of recent posts for homepage."
  (let* ((posts-dir (expand-file-name "./content/posts/"))
         (posts (directory-files posts-dir t "\\.org$"))
         (valid-posts
          (seq-filter
           (lambda (file)
             (and (my/get-post-date file)
                  (my/get-post-title file)
                  (not (string= (file-name-nondirectory file) "index.org"))))
           posts))
         (sorted-posts
          (sort valid-posts
                (lambda (a b)
                  (time-less-p (my/get-post-date b)
                              (my/get-post-date a)))))
         (recent-posts (seq-take sorted-posts 5)))
    (concat "#+begin_export html\n"
            "<div class=\"posts-list\">\n<ul>\n"
            (mapconcat #'my/get-post-link recent-posts "")
            "</ul>\n"
            (when (> (length sorted-posts) 5)
              "<p class=\"more-posts\"><a href=\"posts/\">View all posts →</a></p>")
            "</div>\n"
            "#+end_export")))


;; Process files before publishing
(my/process-index-file)
(my/process-posts-index)

;; Generate site output
(org-publish-all t)

(message "Build complete")
