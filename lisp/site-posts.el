;;; site-posts.el --- Post metadata and generated indexes -*- lexical-binding: t; -*-

(require 'seq)
(require 'site-config)

(defun site-read-post (file)
  "Read FILE once and return its metadata when title and date are present."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((contents (buffer-string))
           (title (progn
                    (goto-char (point-min))
                    (when (re-search-forward "^#\\+TITLE: \\(.+\\)" nil t)
                      (match-string 1))))
           (date (progn
                   (goto-char (point-min))
                   (when (re-search-forward "^#\\+DATE: <\\([^>]+\\)>" nil t)
                     (date-to-time (match-string 1))))))
      (when (and title date)
        (list :title title :date date
              :description (site-post-description contents)
              :slug (file-name-base file))))))

(defun site-collect-posts ()
  "Collect dated, titled posts from the top-level posts directory, newest first."
  (let ((posts (delq nil
                     (mapcar (lambda (file)
                               (unless (string= (file-name-nondirectory file) "index.org")
                                 (site-read-post file)))
                             (directory-files (site-path "content/posts") t "\\.org$")))))
    (sort posts (lambda (a b)
                  (time-less-p (plist-get b :date) (plist-get a :date))))))

(defun site-post-description (contents)
  "Extract the description from post CONTENTS."
  (with-temp-buffer
    (insert contents)
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

(defun site-post-link (post)
  "Generate an HTML link for POST."
  (let* ((title (or (plist-get post :title) "Untitled"))
         (date (plist-get post :date))
         (date-str (format-time-string "%Y-%m-%d" date))
         (href (concat "posts/" (plist-get post :slug) ".html")))
    (format "<li><span class=\"post-date\">%s</span> - <a href=\"%s\">%s</a></li>\n"
            date-str href title)))

(defun site-post-archive-entry (post)
  "Generate an HTML archive entry for POST."
  (let* ((title (or (plist-get post :title) "Untitled"))
         (date (plist-get post :date))
         (date-str (format-time-string "%B %d, %Y" date))
         (description (or (plist-get post :description) ""))
         (href (concat (plist-get post :slug) ".html")))
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

(defun site-posts-archive (posts)
  "Generate the complete archive from sorted POSTS."
  (let ((posts-by-year
         (seq-group-by (lambda (post)
                         (format-time-string "%Y" (plist-get post :date)))
                       posts)))
    (if posts
        (mapconcat
         (lambda (year-group)
           (let ((year (car year-group))
                 (year-posts (cdr year-group)))
             (concat
              (format "<section class=\"archive-year\">
                        <h2>%s</h2>" year)
              (mapconcat #'site-post-archive-entry year-posts "\n")
              "</section>")))
         posts-by-year
         "\n")
      "<p>No posts yet.</p>")))

(defun site-update-home-index (posts)
  "Update the main index with the most recent POSTS."
  (let ((index-file (site-path "content/index.org")))
    (with-temp-buffer
      (insert-file-contents index-file)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Latest Posts" nil t)
        (let ((start (point))
              (end (save-excursion
                     (if (re-search-forward "^\\* " nil t)
                         (line-beginning-position)
                       (point-max)))))
          (delete-region start end))
        (insert "\n\n")
        (insert (site-posts-list posts))
        (insert "\n"))
      (write-region (point-min) (point-max) index-file))))

(defun site-update-posts-index (posts)
  "Update the posts index with an archive of POSTS."
  (let ((posts-index (site-path "content/posts/index.org")))
    (with-temp-buffer
      (insert-file-contents posts-index)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+begin_export html" nil t)
        (let ((start (point)))
          (when (re-search-forward "^#\\+end_export" nil t)
            (beginning-of-line)
            (delete-region start (point))
            (insert "\n<div class=\"posts-archive\">\n"
                   (site-posts-archive posts)
                   "</div>\n"))))
      (write-region (point-min) (point-max) posts-index))))

(defun site-posts-list (posts)
  "Generate the homepage HTML block from sorted POSTS."
  (let ((recent-posts (seq-take posts 5)))
    (concat "#+begin_export html\n"
            "<div class=\"posts-list\">\n<ul>\n"
            (mapconcat #'site-post-link recent-posts "")
            "</ul>\n"
            (when (> (length posts) 5)
              "<p class=\"more-posts\"><a href=\"posts/\">View all posts →</a></p>")
            "</div>\n"
            "#+end_export")))

(provide 'site-posts)
