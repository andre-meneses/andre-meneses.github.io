;;; site-publish.el --- Publishing projects and static assets -*- lexical-binding: t; -*-

(require 'site-config)
(require 'site-html)
(require 'ox-publish)

(defun site-publish-html (plist filename pub-dir)
  "Publish FILENAME with relative styles using PLIST into PUB-DIR."
  (org-tufte-publish-to-html
   (plist-put (copy-sequence plist) :html-head (site-html-head filename))
   filename pub-dir))

(defun site-setup-publishing ()
  "Configure page, post, and attachment publishing."
  (setq org-html-validation-link nil
        org-html-head-include-scripts nil
        org-html-head-include-default-style nil)
  (add-hook 'org-export-filter-final-output-functions #'site-html-page-class)
  (let ((html-options '(:publishing-function site-publish-html
                       :with-author t :with-creator t :with-toc nil
                       :section-numbers nil :time-stamp-file nil
                       :html-preamble site-navbar)))
    (setq org-publish-project-alist
          `(("pages"
             :recursive nil
             :base-directory ,(site-path "content")
             :publishing-directory ,(site-path "public")
             ,@html-options)
            ("posts"
             :recursive t :exclude "drafts/"
             :base-directory ,(site-path "content/posts")
             :publishing-directory ,(site-path "public/posts")
             ,@html-options)
            ("static"
             :base-directory ,(site-path "content")
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory ,(site-path "public")
             :recursive t :publishing-function org-publish-attachment)
            ("website" :components ("pages" "posts" "static"))))))

(defun site-copy-assets ()
  "Copy the root stylesheets and bundled web fonts into the output directory."
  (make-directory (site-path "public") t)
  (dolist (css '("tufte.css" "ox-tufte.css" "site.css"))
    (copy-file (site-path css) (site-path (concat "public/" css)) t))
  (dolist (font (directory-files-recursively (site-path "et-book")
                                            "\\.\\(eot\\|svg\\|ttf\\|woff\\|woff2\\)$"))
    (let ((target (site-path (concat "public/" (file-relative-name font site-root)))))
      (make-directory (file-name-directory target) t)
      (copy-file font target t))))

(provide 'site-publish)
