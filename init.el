(package-initialize)

(require 'org)
(require 'ob-tangle)

;; Set the directory that the settings org file will be in
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Load the snippets in the org file
(org-babel-load-file (expand-file-name "settings.org" init-dir))
