;;; initial_package --- Summary
;; Setting up emacs
;;; Commentary:
;;; Code:



;;Changes which don't require any packages

;; Enable debug on error
;;http://www.math.utah.edu/docs/info/emacs-lisp-intro_18.html
;;http://www.cs.cmu.edu/~keng/emacs.config
(setq debug-on-error t)

;;http://www.emacswiki.org/emacs/FullScreen
;;set default fullscreen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
)
; '(safe-local-variable-values (quote ((encoding . iso-8859-1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x j") 'json-pretty-print-buffer)

;;Use CMD+Option with arrow keys to move between windows
;;http://www.emacswiki.org/emacs/WindMove
(if (display-graphic-p)
    (progn
      (global-set-key (kbd "<M-s-left>") 'windmove-left)
      (global-set-key (kbd "<M-s-right>") 'windmove-right)
      (global-set-key (kbd "<M-s-up>") 'windmove-up)
      (global-set-key (kbd "<M-s-down>") 'windmove-down)
      )
  )

;;Enable viewing pdf, pictures in emacs
;;http://www.idryman.org/blog/2013/05/20/emacs-and-pdf/
;;brew install ghostscript
;; view docs, enabled only in graphical windows
(if (display-graphic-p)
    (progn
      (fset 'doc-prev "\C-xo\C-x[\C-xo")
      (fset 'doc-next "\C-xo\C-x]\C-xo")
      (global-set-key (kbd "M-[") 'doc-prev)
      (global-set-key (kbd "M-]") 'doc-next)
      )
  )

;;yes or no as y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;;Enable follow symbolic link by default
;;http://stackoverflow.com/questions/15390178/emacs-and-symbolic-links
;;No need to answer the question
;;Symbolic link to Git-controlled source file; follow link? (y or n)
;;defaults to y
(setq vc-follow-symlinks t)

;;http://www.emacswiki.org/emacs/LineNumbers
;;set line numbers
(global-linum-mode 1)

;;https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
;;display column number
(setq column-number-mode t)

;;remove tool bar
(tool-bar-mode -1)

;;while running on terminal, disable menu-bar
(if (display-graphic-p)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

;;Ediff split windows vertically
(setq ediff-split-window-function 'split-window-horizontally)

;;Empty scratch buffer message
(setq initial-scratch-message "")

;;add color to shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;http://www.emacswiki.org/emacs/CuaMode
;;(cua-mode t)
;;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;;(transient-mark-mode 1) ;; No region when it is not highlighted
;;(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;don't make backup files
(setq make-backup-files nil)

;;highlight parenthesis
(show-paren-mode t)

;;enable electric pair mode
;;http://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
;;https://github.com/capitaomorte/autopair#electric-pair-mode-in-emacs-244
(electric-pair-mode t)

;;C-k kills whole line and newline if at beginning of line
;;http://xenon.stanford.edu/~manku/emacs.html
(setq kill-whole-line t)



;;install default packages

;;http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
;; From doc of package-initialize
;; If called as part of loading ‘user-init-file’, set
;; ‘package-enable-at-startup’ to nil, to prevent accidentally
;; loading packages twice.
(setq package-enable-at-startup nil)
;;required for `package-installed-p`
(require 'package)
;;http://www.flycheck.org/en/latest/user/installation.html
(add-to-list 'package-archives
              '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;;http://www.aaronbedra.com/emacs.d/
;;set environment
(require 'cl)
;;list the packages to install
(defvar my-packages '(
		      use-package
		      ido-vertical-mode
		      yasnippet
		      auto-complete
		      ;; flycheck ;;use-package to manage flycheck
		      ;; magit ;;use-package to manage magit
		      exec-path-from-shell
		      zenburn-theme
		      jedi ;; Python setup
		      ;; csv-mode
		      ;; json-mode ;;already included in emacs 25.1
		      ;; multiple-cursors
		      ;; seti-theme ;;not present in melpa stable
		      
		      )
  "Default Packages.")
;;check if package is installed
(defun my-packages-installed-p ()
  "Return nil if package not installed."
  (loop for pkg in my-packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))
;;install package if not installed
(unless (my-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;enable ido mode
;; To Disable Ido mode
;; You can either press C-j to accept what you have typed so far,
;; or C-f which will drop you into regular find-file
;; just to add, you can press C-z while in ido mode to disable its auto-completion.

;; To create new directories while creating new files in Ido mode
;;  - Type C-x C-f as usual
;;  - provide non-existent path
;;  - Press M-m which will prompt for new directory to create
;;  - Specify filename in new directory
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys (quote C-n-C-p-up-and-down))

;; yasnippet
;; https://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/
;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;;turn on autocomplete
;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(ac-config-default)
;; set the trigger key so that it can work together with yasnippet on tab key,
;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;exec-path-from-shell installation
;;Copy settings of PATH from bash
;;To synchronize path for bash and for emacs
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;;Required for jedi installation (virtualenv)
;;Required for flycheck http://www.flycheck.org/en/latest/user/troubleshooting.html#flycheck-macos-exec-path-from-shell

;; ;;http://www.flycheck.org/manual/latest/Quickstart.html#Quickstart
;; ;; For python => pip install pylint
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;;http://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;https://github.com/bbatsov/zenburn-emacs
;;turn on zenburn theme
(if (display-graphic-p)
    (load-theme 'zenburn t)
  ;; (load-theme 'seti t)
)
;; (load-theme 'zenburn t)

;;https://github.com/jwiegley/use-package
;;https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )



;; ;;Configurations

;; ;;Python configuration
;; ;;allow UTF-8 uppercase encoding in emacs
;; ;;http://stackoverflow.com/questions/14031724/how-to-make-emacs-accept-utf-8-uppercase-encoding
;; (define-coding-system-alias 'UTF-8 'utf-8)

;; ;;http://emacswiki.org/emacs/PythonProgrammingInEmacs#toc18
;; ;;Unicode on Mac OS X
;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (setenv "LC_All" "en_US.UTF-8")))

;;Jedi mode
;;Steps before installing jedi
;; pip install virtualenv
;; pip install jedi
;; pip install epc
;; M-x jedi:install-server
;;http://tkf.github.io/emacs-jedi/latest/
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; ;;Simple Emacs Spreadsheet
;; ;;ses-mode
;; ;;http://www.emacswiki.org/emacs/SimpleEmacsSpreadsheet
;; ;;Used for Project Euler

;; ;;Enable multiple cursors
;; ;;https://github.com/magnars/multiple-cursors.el
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)






;; ;;Added csv mode
;; (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;; (autoload 'csv-mode "csv-mode"
;;   "Major mode for editing comma-separated value files." t)
