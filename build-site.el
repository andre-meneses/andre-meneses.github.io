;;; build-site.el --- Build the Org website -*- lexical-binding: t; -*-

(add-to-list 'load-path
             (expand-file-name "lisp" (file-name-directory load-file-name)))
(require 'site-config)

(site-setup-dependencies)
(require 'site-posts)
(require 'site-html)
(require 'site-publish)

(site-setup-id-links)
(let ((posts (site-collect-posts)))
  (site-update-home-index posts)
  (site-update-posts-index posts))

(site-setup-publishing)
(org-publish-all t)
(site-copy-assets)
(message "Build complete: %s" (site-path "public"))
