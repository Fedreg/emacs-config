; increase garbace collector threshold for startup
(setq gc-cons-threshold (* 50 1000 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Package Repos
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Activate all the packages (in particular autoloads)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GC tuning
(setq gc-cons-threshold 100000000)
(setq inhibit-compacting-font-caches t)

;; For startup profiling
(use-package esup
  :ensure t
  :defer t
  :config
  (setq esup-deph 0))

(defun edit-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode (vim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-fu
  :ensure t
  :config
  (setq evil-undo-system 'undo-fu))

(use-package evil
  :ensure t
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (evil-global-set-key 'normal (kbd "TAB") 'evil-jump-item)
  (evil-global-set-key 'visual (kbd "TAB") 'evil-jump-item)
  (evil-global-set-key 'motion (kbd "TAB") 'evil-jump-item)
  (evil-global-set-key 'normal (kbd "C-z") 'suspend-emacs)
  (evil-global-set-key 'normal (kbd "M-h") 'help-command)
  (evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)
  (evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)
  (evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
  (evil-global-set-key 'normal (kbd "C-l") 'evil-window-right))

(use-package evil-leader
  :ensure t
  :defer t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "e v" 'edit-init-file
    "f"   'ag-project
    "b"   'switch-to-buffer
    "l"   'switch-to-previous-buffer
    "g s" 'magit-status
    "g b" 'magit-blame-echo
    "o"   'org-cycle
    "s"   'save-buffer
    "w x" 'kill-this-buffer
    "w v" 'evil-window-vsplit
    "z"   'fzf))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :ensure t
  :config (evil-commentary-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look & Feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fullscreen on startup
(add-hook 'window-setup-hook 'toggle-frame-fullscreen nil)

(use-package modus-themes
  :ensure t
  :config 
  (load-theme 'modus-vivendi t))

(set-cursor-color "#f00") 

;; y or n for prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Gui
(tool-bar-mode -1)
(menu-bar-mode -1)
; (scroll-bar-mode -1)
; (tooltip-mode -1)

;; No startup banner
(setq inhibit-startup-message t)

(setq initial-major-mode 'fundamental-mode)

;; Font
(add-to-list 'default-frame-alist '(font . "Menlo-16"))

;; Ido Mode (default emacs narrowing; comes with Emacs)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; No Sounds
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :defer t
  :init (projectile-mode +1))

(use-package fzf
  :ensure t
  :defer t)

(use-package ag
  :ensure t
  :defer t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  (add-hook 'ag-mode-hook #'next-error-follow-minor-mode))

(use-package magit
  :ensure t
  :defer t
  :config
   (setq magit-blame-styles
           '((margin
              (margin-format " %s%f" " %C %a" " %H")
              (margin-width . 42)
              (margin-face . magit-blame-margin)
              (margin-body-face magit-blame-dimmed)))))

;; SO that customizations aren't loaaded into this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; No backup files
(setq make-backup-files nil)

;; Auto Save on loss of focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(use-package company
  :ensure t
  :defer t
  :bind (("TAB" . company-indent-or-complete-common))
  :config (global-company-mode))

(use-package xclip
  :ensure t
  :defer t
  :init (xclip-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(use-package cider
  :ensure t
  :defer  t
  :config
  (setq cider-auto-jump-to-error             nil)
  (setq cider-auto-select-error-buffer       nil)
  (setq cider-auto-select-test-report-buffer t)
  (setq cider-repl-display-help-banner       nil)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-pop-to-buffer-on-connect  nil)
  (setq cider-repl-use-pretty-printing       t)
  (setq cider-save-file-on-load              t)
  (setq cider-show-error-buffer              :only-in-repl)
  (setq cider-test-show-report               nil)
  (setq nrepl-hide-special-buffers           t)
  (evil-global-set-key 'normal (kbd "C-]") 'go-forward)
  (evil-global-set-key 'normal (kbd "C-t") 'pop-back))

(defun my-cider-debug-setup ()
  (evil-make-overriding-map cider--debug-mode-map 'normal)
  (evil-normalize-keymaps))

(defun my-cider-nav-setup ()
  (evil-make-overriding-map cider-mode-map 'normal)
  (evil-normalize-keymaps))

(add-hook 'cider--debug-mode-hook 'my-cider-debug-setup 'my-cider-nav-setup)

(defun run-cider-debugger ()
  "Need to use this to work with evil mode"
  (interactive)
  (cider-debug-defun-at-point))

(defun go-forward ()
  (interactive)
  (cider-find-var))

(defun pop-back ()
  (interactive)
  (cider-pop-back))

;; Parens
(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (add-hook 'clojure-mode-hook #'smartparens-mode)
    (show-paren-mode t)))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(defun clj-ns-align ()
  "Align ns requires."
  (interactive)
  (end-of-buffer)
  (when (re-search-backward "^\(ns.*\\(\n.*\\)*\(:require" nil t nil)
    (mark-sexp)
    (align-regexp (region-beginning)
                  (region-end)
                  "\\(\\s-*\\)\\s-:")))

(defun open-repl-buffer ()
  (interactive)
  (evil-window-vsplit)
  (evil-window-next 1)
  (cider-switch-to-repl-buffer))

(defun clear-repl-buffer ()
  (interactive)
  (cider-find-and-clear-repl-output)
  (cider-switch-to-repl-buffer)
  (evil-goto-first-line)
  (cider-switch-to-last-clojure-buffer))

(defun close-repl-buffer ()
  (interactive)
  (evil-window-next 1)
  (evil-quit t))
  
(with-eval-after-load 'evil
  (evil-leader/set-key-for-mode 'clojure-mode
    "a"   'clj-ns-align
    "d"   'cider-doc
    "e '" 'cider-jack-in
    "e b" 'cider-load-buffer
    "e c" 'cider-connect
    "e d" 'run-cider-debugger
    "e e" 'cider-eval-sexp-at-point
    "e p" 'cider-pprint-eval-last-sexp ;; prints in repl
    "e P" 'cider-eval-print-last-sexp ;; prints in buffer
    "e x" 'cider-interrupt
    "s s" 'open-repl-buffer
    "s c" 'clear-repl-buffer
    "s q" 'close-repl-buffer
    "t t" 'cider-test-run-test
    "t n" 'cider-test-run-ns-tests)
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "a"   'clj-ns-align
    "d"   'cider-doc
    "e '" 'cider-jack-in-cljs
    "e b" 'cider-load-buffer
    "e c" 'cider-connect-cljs
    "e e" 'cider-eval-sexp-at-point
    "e p" 'cider-pprint-eval-last-sexp ;; prints in repl
    "e P" 'cider-eval-print-last-sexp ;; prints in buffer
    "e x" 'cider-interrupt
    "s s" 'open-repl-buffer
    "s c" 'clear-repl-buffer
    "s q" 'close-repl-buffer
    "t t" 'cider-test-run-test
    "t n" 'cider-test-run-ns-tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-complete
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-hook #'auto-complete-mode))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)
	    (setq-local show-trailing-whitespace nil)
	    (auto-complete-mode t)))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq-local ac-ignore-case t)
            (auto-complete-mode)))

(setq sql-connection-alist
      '((dev   (sql-product  'postgres)
	       (sql-database "postgres://DB:PASSWORD@SERVER:PORT/DATABASE"))
        
	(stage (sql-product  'postgres)
         (sql-database "postgres://DB:PASSWORD@SERVER:PORT/DATABASE"))

	(prod  (sql-product  'postgres)
	       (sql-database "postgres://DB:PASSWORD@SERVER:PORT/DATABASE"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleanup after load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (* 2 1000 1000))
