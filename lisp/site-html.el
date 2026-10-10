;;; site-html.el --- HTML navigation, styles, and Org ID links -*- lexical-binding: t; -*-

(require 'site-config)
(require 'org-id)
(require 'ox-html)

(defun site-id-link (id desc info)
  "Render ID and DESC relative to the source file in INFO."
  (let* ((file (org-id-find-id-file id))
         (input-file (plist-get info :input-file))
         (target (when file
                   (concat (file-name-sans-extension
                            (file-relative-name file (site-path "content/")))
                           ".html")))
         (href (if (and target input-file)
                   (site-relative-url target input-file)
                 (concat "#" id))))
    (format "<a href=\"%s\">%s</a>" href (or desc id))))

(defun site-tufte-link (original link desc info)
  "Use site ID resolution, delegating other links to ORIGINAL."
  (if (string= (org-element-property :type link) "id")
      (site-id-link (org-element-property :path link) desc info)
    (funcall original link desc info)))

(defun site-setup-id-links ()
  "Refresh Org IDs and register HTML link handling."
  (setq org-id-locations-file (site-path ".org-id-locations")
        org-id-track-globally t
        org-id-extra-files (directory-files-recursively (site-path "content/") "\\.org$"))
  (org-id-update-id-locations org-id-extra-files)
  (org-link-set-parameters
   "id" :export (lambda (id desc format info)
                  (when (eq format 'html) (site-id-link id desc info))))
  (advice-add 'org-tufte-maybe-margin-note-link :around #'site-tufte-link))

(defun site-navbar (info)
  "Generate navigation relative to the input file in INFO."
  (let ((input-file (plist-get info :input-file)))
    (concat "<nav class=\"navbar\"><ul>"
            (mapconcat
             (lambda (item)
               (format "<li><a href=\"%s\">%s</a></li>"
                       (site-relative-url (car item) input-file) (cdr item)))
             '(("index.html" . "Home") ("about.html" . "About")
               ("posts/index.html" . "Posts")) "")
            "</ul></nav>")))

(defun site-html-head (input-file)
  "Generate stylesheet links relative to INPUT-FILE."
  (mapconcat (lambda (css)
               (format "<link rel=\"stylesheet\" href=\"%s\" type=\"text/css\" />"
                       (site-relative-url css input-file)))
             '("tufte.css" "ox-tufte.css" "site.css") "\n"))

(defun site-html-page-class (output backend info)
  "Mark post OUTPUT for its existing spacing rules when exporting BACKEND."
  (if (and (org-export-derived-backend-p backend 'tufte-html)
           (plist-get info :input-file)
           (file-in-directory-p (plist-get info :input-file)
                                (site-path "content/posts/")))
      (replace-regexp-in-string "<body>" "<body class=\"site-posts\">" output t t)
    output))

(provide 'site-html)
