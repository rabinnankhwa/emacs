;;http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
;;required for `package-installed-p`
(require 'package)
(package-initialize)

;;http://caisah.info/emacs-for-python/
;;set packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;;http://www.emacswiki.org/emacs/FullScreen
;;set default fullscreen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )

;;install default packages
;;http://www.aaronbedra.com/emacs.d/
;;set environment
(require 'cl)
;;list the packages to install
(defvar my-packages '(auto-complete
		      exec-path-from-shell
		      ido-vertical-mode
		      jedi
		      yasnippet
		      zenburn-theme)
  "Default Packages")
;;check if package is installed
(defun my-packages-installed-p ()
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

;;add color to shell text
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;http://www.emacswiki.org/emacs/CuaMode
;;enable default full screen
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;http://www.emacswiki.org/emacs/LineNumbers
;;set line numbers
(global-linum-mode 1)

;;https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
;;display column number
(setq column-number-mode t)

;;remove tool bar
(tool-bar-mode -1)

;;don't make backup files
(setq make-backup-files nil)

;;yes or no as y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;;key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;highlight parenthesis
(show-paren-mode t)

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

;;https://github.com/bbatsov/zenburn-emacs
;;turn on zenburn theme
(load-theme 'zenburn t)


;;Python configuration
;;http://emacswiki.org/emacs/PythonProgrammingInEmacs#toc18
;;Unicode on Mac OS X
(setenv "LC_CTYPE" "UTF-8")

;;exec-path-from-shell installation
;;Copy settings of PATH from bash
;;To synchronize path for bash and for emacs
;;Required for jedi installation (virtualenv)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;Jedi mode
;;Steps before installing jedi
;; pip install virtualenv
;; pip install jedi
;; pip install epc
;; M-x jedi:install-server
;;http://tkf.github.io/emacs-jedi/latest/
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;;Simple Emacs Spreadsheet
;;ses-mode
;;http://www.emacswiki.org/emacs/SimpleEmacsSpreadsheet
;;Used for Project Euler
