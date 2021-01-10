 (require 'package)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Packages
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; List packages you want to install
 (setq package-list '(ag
		      cider
		      evil
		      evil-escape
		      evil-leader
		      fzf
		      projectile
		      modus-themes))

 ;; Package Repos
 (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
 (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

 ;; Activate all the packages (in particular autoloads)
 (package-initialize)

 ;; Update your local package index
 (unless package-archive-contents
   (package-refresh-contents))

 ;;Install all missing packages
 (dolist (package package-list)
   (unless (package-installed-p package)
     (package-install package)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Evil Mode (vim)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (require 'evil)
 (evil-mode t)

 (require 'evil-leader)
 (global-evil-leader-mode)
 (evil-leader/set-leader "SPC")

 (evil-escape-mode 1)
 (setq-default evil-escape-key-sequence "jk")

 (evil-leader/set-key
   "b" 'switch-to-buffer
   "f" 'ag
   "l" 'switch-to-previous-buffer
   "w" 'save-buffer
   "z" 'fzf
   "c" 'comment-line)
   

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
 (load-theme 'modus-vivendi t)
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

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; FS
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; SO that customizations aren't loaaded into this file
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

 ;; Projectile everywhere
 (add-hook 'after-init-hook #'projectile-global-mode)

 ;; No backup files
 (setq make-backup-files nil)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Clojure
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (setq cider-repl-use-pretty-printing       t)
 (setq cider-repl-display-help-banner       nil)
 (setq cider-auto-jump-to-error             nil)
 (setq cider-auto-select-error-buffer       nil)
 (setq cider-show-error-buffer              :only-in-repl)
 (setq cider-repl-pop-to-buffer-on-connect  nil)
 (setq cider-auto-select-test-report-buffer t)
 (setq cider-test-show-report               nil)
 (setq cider-repl-display-in-current-window t)
 (setq cider-repl-history-file              (expand-file-name "cider-history" user-emacs-directory))

 (defun my-cider-debug-setup ()
   (evil-make-overriding-map cider--debug-mode-map 'normal)
   (evil-normalize-keymaps))

 (add-hook 'cider--debug-mode-hook 'my-cider-debug-setup)

 (defun run-cider-debugger()
   "Need to use this to work with evil mode"
   (interactive)
   (cider-debug-defun-at-point))

 (global-auto-revert-mode t)

 (evil-leader/set-key-for-mode 'clojure-mode
   "e '" 'cider-jack-in
   "e b" 'cider-load-buffer
   "e d" 'run-cider-debugger
   "e e" 'cider-eval-last-sexp
   "e p" 'cider-pprint-eval-last-sexp
   "g f" 'cider-find-var
   "g b" 'cider-pop-back
   "s s" 'cider-switch-to-repl-buffer
   "s c" 'cider-find-and-clear-repl-output
   "t t" 'cider-test-run-test
   "t n" 'cider-test-run-ns-tests)
