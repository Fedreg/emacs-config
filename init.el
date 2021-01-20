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
  :config (setq esup-depth 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode (vim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-fu
  :ensure t
  :config
  (setq evil-undo-system 'undo-fu))

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-l") 'evil-window-right))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "a"     'ag
    "b"     'switch-to-buffer
    "c"     'comment-line
    "f f"   'find-file
    "f s"   'save-buffer
    "l"     'switch-to-previous-buffer
    "m s"   'magit-status
    "m b b" 'magit-blame-echo
    "m b q" 'magit-blame-quit
    "o"     'org-cycle
    "w o"   'other-window
    "w x"   'kill-this-buffer
    "w v"   'evil-window-vsplit
    "x"     'suspend-emacs
    "z"     'fzf))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"))

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

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-city-lights t))

(set-cursor-color "#f00") 

;; y or n for prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

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
  :config (projectile-mode 1))


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
  :defer t)

;; SO that customizations aren't loaaded into this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; No backup files
(setq make-backup-files nil)

(use-package company
  :ensure t
  :defer t
  :bind (("TAB" . company-indent-or-complete-common))
  :config (global-company-mode))

(use-package xclip
  :ensure t
  :defer t
  :config (xclip-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cider
  :ensure t
  :defer  t
  :config
  (setq cider-repl-use-pretty-printing       t)
  (setq cider-repl-display-help-banner       nil)
  (setq cider-auto-jump-to-error             nil)
  (setq cider-auto-select-error-buffer       nil)
  (setq cider-show-error-buffer              :only-in-repl)
  (setq cider-repl-pop-to-buffer-on-connect  nil)
  (setq cider-auto-select-test-report-buffer t)
  (setq cider-test-show-report               nil)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-history-file              (expand-file-name "cider-history" user-emacs-directory)))

(defun my-cider-debug-setup ()
  (evil-make-overriding-map cider--debug-mode-map 'normal)
  (evil-normalize-keymaps))

(add-hook 'cider--debug-mode-hook 'my-cider-debug-setup)

(defun run-cider-debugger ()
  "Need to use this to work with evil mode"
  (interactive)
  (cider-debug-defun-at-point))

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
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(evil-leader/set-key-for-mode 'clojure-mode
  "e '" 'cider-jack-in
  "e b" 'cider-load-buffer
  "e d" 'run-cider-debugger
  "e e" 'cider-eval-last-sexp
  "e p" 'cider-pprint-eval-last-sexp ;; prints in repl
  "e P" 'cider-eval-print-last-sexp ;; prints in buffer
  "e x" 'cider-interrupt
  "g f" 'cider-find-var
  "g b" 'cider-pop-back
  "s s" 'cider-switch-to-repl-buffer
  "s c" 'cider-find-and-clear-repl-output
  "t t" 'cider-test-run-test
  "t n" 'cider-test-run-ns-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-complete
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-hook #'auto-complete-mode))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ob-sql-mode
  :ensure t
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (emacs-lisp . t))))
