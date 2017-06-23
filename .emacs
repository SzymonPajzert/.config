(require 'package)
;; turn on if efficiency drops
;; (setq package-enable-at-startup nil)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

			 
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(setq-default c-basic-offset 4
	      tab-width 4
	      indent-tabs-mode t)

;; TODO add to dependencies
;; (add-to-list 'load-path "~/.emacs.d/dockerfile-mode/")
;; (require 'dockerfile-mode)
;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package darcula-theme :ensure t :config
  ;; your preferred main font face here
  ;; (set-frame-font "Inconsolata-12")
  )

(use-package projectile :ensure t)

(use-package haskell-mode :ensure t)

;; (use-package auctex :ensure t)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(use-package hindent :ensure t)
(add-hook 'haskell-mode-hook #'hindent-mode)

(setq calendar-week-start-day 1)

;; org-mode customization
(setq org-agenda-time-grid '((daily weekly require-timed)
                            "--------------------"
							(800 1000 1200 1400 1600 1800 2000 2200)))

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; org-mode keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c\C-x\C-o" 'org-clock-out)

;; org-mode browser
(setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "google-chrome")

;; http://stackoverflow.com/questions/8812520/defining-unscheduled-todos-as-stuck-projects-in-emacs-org-mode
(setq org-stuck-projects
	  '("TODO={.+}/-DONE-SOMEDAY-FREE-FAILED-CANCELLED-COLLECT" nil ("book") "SCHEDULED:\\|DEADLINE:"))

;; org-mode configuration
(setq org-todo-keywords
'((sequence "TODO(t)" "FREE(f)" "|" "DONE(d)" "AWAITING(a)" "FAILED(l)" "CANCELLED(c)" "SOMEDAY(s)")))

(setq org-agenda-files
 '("~/Documents/org" "~/Documents/org/studia"))

(setq org-log-done 'time)
(setq org-deadline-warning-days 28)

;; org-mode capture

(setq org-directory "~/Documents/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-default-todos-file (concat org-directory "/todo.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline 'org-default-todos-file "Tasks")
		 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree 'org-default-notes-file)
		 "* %?\nEntered on %U\n  %i\n  %a")))


;; Org habits
(defun my-after-load-org ()
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-id))
(eval-after-load "org" '(my-after-load-org))

;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(use-package neotree :ensure t)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(use-package all-the-icons :ensure t)
(require 'all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons))

;; Auto refresh all the buffers
;; Adapted from http://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(load "~/.emacs.d/swipl/prolog.el")
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "") (awk-mode . "awk") (other . "gnu"))))
 '(neo-autorefresh nil)
 '(neo-hidden-regexp-list
   (quote
	("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.hi$" "\\.o$")))
 '(neo-window-fixed-size nil)
 '(package-selected-packages
   (quote
	(all-the-icons org request hindent auctex use-package projectile neotree haskell-mode ensime darcula-theme)))
 '(warning-minimum-level :debug))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


