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

(add-to-list 'load-path "~/.emacs.d/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package darcula-theme
  :ensure t
  :config
  ;; your preferred main font face here
  (set-frame-font "Inconsolata-12"))

(use-package projectile
  :ensure t)

(use-package haskell-mode
  :ensure t)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Tuareg mode
(load "/home/svp/.opam/system/share/emacs/site-lisp/tuareg-site-file.el")

;; Merlin - tuareg
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

;; caml-mode
(add-to-list 'load-path "/home/svp/.opam/system/share/emacs/site-lisp/")

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
	  browse-url-generic-program "google-chrome-stable")

;; http://stackoverflow.com/questions/8812520/defining-unscheduled-todos-as-stuck-projects-in-emacs-org-mode
(setq org-stuck-projects
	  '("TODO={.+}/-DONE-SOMEDAY-FREE-FAILED-CANCELLED" nil ("book") "SCHEDULED:\\|DEADLINE:"))

;; org-mode configuration
(setq org-todo-keywords
'((sequence "FREE(f)" "TODO(t)" "|" "DONE(d)" "AWAITING(a)" "FAILED(f)" "CANCELLED(c)" "SOMEDAY(s)")))

(setq org-agenda-files
 '("~/Documents/org" "~/Documents/org/studia"))

(setq org-log-done 'time)

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

(use-package neotree
  :ensure t)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "") (awk-mode . "awk") (other . "gnu"))))
 '(neo-window-fixed-size nil)
 '(warning-minimum-level :debug))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


