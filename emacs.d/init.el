;;; initial_package --- Summary
;; Setting up emacs
;;; Commentary:
;;; Code:


;;Changes which don't require any packages

;;Detach custom file
;;http://emacsblog.org/2008/12/06/quick-tip-detaching-the-custom-file/
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Enable debug on error
;;http://www.math.utah.edu/docs/info/emacs-lisp-intro_18.html
;;http://www.cs.cmu.edu/~keng/emacs.config
(setq debug-on-error t)

;;http://www.emacswiki.org/emacs/FullScreen
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;Setting transparent on UI mode
;;http://stackoverflow.com/questions/21946382/how-to-get-transparent-window-in-gnu-emacs-on-osx
(if (display-graphic-p)
    (add-to-list 'default-frame-alist '(alpha 85 85))
  )

;;key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x j") 'json-pretty-print-buffer-ordered)

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

;;Disable vc in tramp mode
;;Makes tramp mode faster :)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

;;https://www.youtube.com/watch?v=5FQwQ0QWBTU
;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Semantic.html
;;turn on semantic mode
(semantic-mode 1)

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

;;http://aaronbedra.com/emacs.d/#splash-screen
;;Empty splash screen and scratch buffer message
;;(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

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

;;Auto update buffers
;;https://www.emacswiki.org/emacs/RevertBuffer
(global-auto-revert-mode 1)


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


;;list the packages to install
(defvar my-packages '(
		      use-package
		      )
  "Default Packages.")
;;check if package is installed
(defun my-packages-installed-p ()
  "Return nil if package not installed."
  (cl-loop for pkg in my-packages
        when (not (package-installed-p pkg)) do (cl-return nil)
        finally (cl-return t)))
;;install package if not installed
(unless (my-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


;;https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))
;(setq use-package-always-ensure t) ;;causes problems


;;https://github.com/bbatsov/zenburn-emacs
;;turn on zenburn theme
(use-package zenburn-theme
  :ensure t
;;  :if window-system
  :config (load-theme 'zenburn t))

;;csv-mode
(use-package csv-mode
  :ensure t)

;;resize window
(use-package windsize
  :ensure t)

;;which-key mode
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

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
(use-package ido
  :ensure t
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (use-package ido-vertical-mode
    :ensure t
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys (quote C-n-C-p-up-and-down))
    ;; (setq ido-use-faces t)
    ;; (set-face-attribute 'ido-vertical-first-match-face nil
    ;; 			:background "white"
    ;; 			:foreground "#00b000")
    ;; (set-face-attribute 'ido-vertical-only-match-face nil
    ;; 			:background "#00b000"
    ;; 			:foreground "white")
    ;; (set-face-attribute 'ido-vertical-match-face nil
    ;; 			:foreground "#00b000")
    )
  (use-package smex
    :ensure t
    :init (setq smex-save-file "~/.emacs.d/smex-items")
    :bind (("M-x" . smex)
	   ("M-X" . smex-major-mode-commands))

    :config (smex-initialize))
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching t))
  :config
  (add-to-list 'ido-ignore-files "\\.DS_Store"))

;; yasnippet
;; https://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/
;; should be loaded before auto complete so that they can work together
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;;turn on autocomplete
;; should be loaded after yasnippet so that they can work together
(use-package auto-complete
  :ensure t
  :init
  (use-package auto-complete-config
    :config
    (ac-config-default)
    ;;Delay to show quick help (default: 1.5)
    (setq ac-quick-help-delay 0.5)
    ;; set the trigger key so that it can work together with yasnippet on tab key,
    ;; if the word exists in yasnippet, pressing tab will cause yasnippet to
    ;; activate, otherwise, auto-complete will
    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")))

;;exec-path-from-shell installation
;;Copy settings of PATH from bash
;;To synchronize path for bash and for emacs
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;;Required for jedi installation (virtualenv)
;;Required for flycheck http://www.flycheck.org/en/latest/user/troubleshooting.html#flycheck-macos-exec-path-from-shell

;; ;;http://www.flycheck.org/manual/latest/Quickstart.html#Quickstart
;; ;; For python => pip install pylint
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;;http://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;;https://github.com/mattss/emacs.d/blob/master/init.el
(use-package flymake :disabled t)

;;https://github.com/jwiegley/use-package
;;https://github.com/bradwright/emacs-d/blob/master/packages/init-magit.el
;;Magit reference: https://magit.vc/manual/magit-refcard.pdf
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )

;;Enable multiple cursors
;;https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;;Projectile mode
;;https://github.com/bbatsov/projectile
;;https://github.com/rejeep/emacs/blob/master/init.el
;;Shortcuts
;; C-c p s : Switch to project
;; C-c p f : List files in project
;; C-c p k : Kill all buffers from project
(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-enable-caching t)
    ;(setq projectile-require-project-root nil)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store"))
  (projectile-mode +1))


;;http://mads-hartmann.com/2016/05/12/emacs-tree-view.html
;;https://www.emacswiki.org/emacs/NeoTree
;;https://github.com/jaypei/emacs-neotree
(use-package neotree
  :ensure t
  :init
  (setq neo-smart-open t)
  ;; autorefresh disabled cuz it would revert to default directory as root
  ;; need to toggle to refresh side panel as autorefresh is disabled.
  (setq neo-autorefresh nil)
  (setq neo-window-width 40)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (global-set-key [f8] 'neotree-toggle)
  (global-set-key [f5] 'neotree-show)
  :config
  ;; overriding default definition of neotree-show to show proper git root
  ;; https://github.com/jaypei/emacs-neotree/pull/263/files
  (defun neotree-show ()
  "Show the NeoTree window."
  (interactive)
  (let ((cw (selected-window))
         (path (buffer-file-name)))  ;; save current window and buffer
    (if neo-smart-open
      (progn
        (when (and (fboundp 'projectile-project-p)
              (projectile-project-p)
              (fboundp 'projectile-project-root))
          (neotree-dir (projectile-project-root)))
        (neotree-find path))
      (neo-global--open))
    (neo-global--select-window)
    (when neo-toggle-window-keep-p
      (select-window cw))))
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

;; https://stackoverflow.com/questions/25669809/how-do-you-run-python-code-using-emacs
;; In a python buffer:

;;     C-c C-z : open a python shell
;;     C-c C-c : run the content of the buffer in the opened python shell
;;     C-c C-r : run the selected region in the python shell

;; IPython as default python interpreter
;; https://stackoverflow.com/questions/17817019/how-to-open-ipython-interpreter-in-emacs
(when (executable-find "ipython")
  ;; (setq python-shell-interpreter "ipython"))
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "--simple-prompt -i"))

;; Also, checkout
;; https://masteringemacs.org/article/compiling-running-scripts-emacs


;;Jedi mode
;;Steps before installing jedi
;; pip install virtualenv
;; pip install jedi ;not needed?
;; pip install epc  ;not needed?
;; M-x jedi:install-server
;;http://tkf.github.io/emacs-jedi/latest/
;; C-c ? => jedi:show-doc
;; C-c . => jedi:goto-definition
;; C-c , => jedi:goto-definition-pop-marker
(use-package jedi
  :ensure t
  :init
  ;Hook up to autocomplete
  (add-to-list 'ac-sources 'ac-source-jedi-direct)
  ;Enable for python mode
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; ansible mode
;; https://github.com/k1LoW/emacs-ansible
(use-package ansible
  :ensure t)


;; yaml mode
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :ensure t
  :init
  ;Enable for yaml mode
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))


;; ;;haskell mode
;; ;;https://github.com/chenglou/emacs.d/tree/master/use-package example
;; (use-package haskell-mode
;;   :commands haskell-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode)))


;;notmuch setup for mail
;;https://github.com/DamienCassou/emacs.d/blob/master/init.el
(eval-and-compile
  (setq-default notmuch-command (executable-find "notmuch")))

(use-package notmuch
  :if notmuch-command
  :bind (("C-c m" . notmuch))
  :init (setq notmuch-search-oldest-first nil))


;;js2-mode for javascript editing
;;https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
(use-package js2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; Better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (use-package js2-refactor
    :ensure t
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill)))

;; tide mode for typescript
(use-package tide
  :ensure t)

;; rust-mode for rust
(use-package rust-mode
  :ensure t)


;;Setup email sending from gmail using message mode
;;https://www.emacswiki.org/emacs/MessageMode
;;This requires explicit permission to allow apps to access gmail
;;https://myaccount.google.com/lesssecureapps
(setq user-mail-address "rabin.nankhwa@gmail.com"
      user-full-name "Rabin Nankhwa"
      message-send-mail-function 'message-smtpmail-send-it
      ;; TODO
      ;; smtp settings only valid for message mode
      ;; bind these definitions to start of message package???
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; add Cc and Bcc headers to the message buffer
      message-default-mail-headers "Cc: \nBcc: \n"
      ;; postponed message is put in the following draft folder
      message-auto-save-directory "~/.mail"
      ;; get extra auth info from ~/.authinfo
      ;; https://www.gnu.org/software/emacs/manual/html_node/smtpmail/Authentication.html
      )


(provide 'init)
;;; init.el ends here
