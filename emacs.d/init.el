;;http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
;;required for `package-installed-p`
(require 'package)
(package-initialize)

;;http://caisah.info/emacs-for-python/
;;set packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;;enable ido mode
(require 'ido)
(ido-mode t)

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
(defvar my-packages '(auto-complete zenburn-theme)
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

;;turn on autocomplete
(require 'auto-complete-config)
(ac-config-default)

;;https://github.com/bbatsov/zenburn-emacs
;;turn on zenburn theme
(load-theme 'zenburn t)


;;Python configuration
;;http://emacswiki.org/emacs/PythonProgrammingInEmacs#toc18
;;Unicode on Mac OS X
(setenv "LC_CTYPE" "UTF-8")
