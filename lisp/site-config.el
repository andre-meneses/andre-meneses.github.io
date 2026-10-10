;;; site-config.el --- Paths and dependency setup -*- lexical-binding: t; -*-

(defconst site-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name)))
  "Repository root, resolved from this module's location.")

(defun site-path (relative)
  "Resolve RELATIVE against the repository root."
  (expand-file-name relative site-root))

(defun site-relative-url (target input-file)
  "Return a URL to content-relative TARGET from INPUT-FILE."
  (file-relative-name (site-path (concat "content/" target))
                      (file-name-directory input-file)))

(defun site-setup-dependencies ()
  "Initialize local packages and load the required exporter."
  (require 'package)
  (setq package-user-dir (site-path ".packages")
        package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'htmlize)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'htmlize))
  (require 'htmlize)
  (require 'ox-publish)
  ;; Required: let Emacs report errors and exit with a failing status.
  (require 'ox-tufte))

(provide 'site-config)
