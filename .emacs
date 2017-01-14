(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

(use-package darcula-theme
  :ensure t
  :config
  ;; your preferred main font face here
  (set-frame-font "Inconsolata-14"))

(use-package projectile
  :ensure t)

;; org-mode keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c\C-x\C-o" 'org-clock-out)

;; org-mode browser
(setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "chromium-browser")

;; http://stackoverflow.com/questions/8812520/defining-unscheduled-todos-as-stuck-projects-in-emacs-org-mode
(setq org-stuck-projects
	  '("TODO={.+}/-DONE-SOMEDAY" nil nil "SCHEDULED:\\|DEADLINE:"))

;; org-mode configuration
(setq org-todo-keywords
'((sequence "TODO" "CHECK" "LEARN" "|" "DONE" "SOMEDAY")))

(setq org-agenda-files
 (cons "~/Documents/org" nil))

(setq org-log-done 'time)

;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "") (awk-mode . "awk") (other . "gnu"))))
 '(warning-minimum-level :debug))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


