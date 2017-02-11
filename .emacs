
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
  (set-frame-font "Inconsolata-12"))

(use-package projectile
  :ensure t)

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
	  browse-url-generic-program "chromium-browser")

;; http://stackoverflow.com/questions/8812520/defining-unscheduled-todos-as-stuck-projects-in-emacs-org-mode
(setq org-stuck-projects
	  '("TODO={.+}/-DONE-SOMEDAY-FAILED-CANCELLED" nil nil "SCHEDULED:\\|DEADLINE:"))

;; org-mode configuration
(setq org-todo-keywords
'((sequence "TODO(t)" "CHECK(c)" "LEARN(l)" "|" "DONE(d)" "SOMEDAY(s)" "FAILED(f)" "ABORTED(a)")))

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
  (add-to-list 'org-modules 'org-habit))
(eval-after-load "org" '(my-after-load-org))

;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; http://joelmccracken.github.io/entries/emacs-hack-set-todo-done-yesterday/

(defun org-todo-toggle-yesterday ()
  ;; this function is interactive, meaning a "command" that we call
  ;; as an emacs user (allows us to do "M-x org-todo-toggle-yesterday")
  (interactive)

  (let ((time-in-question (decode-time))) 
    ;; time-in-question is the current time, decoded into convenient fields

    ;; decrease the field by one which represents the day -- make it "yesterday"
    (setq (nth 3 time-in-question) (nth 3 time-in-question) - 1)

    ;; now, re-encode that time
    (setq time-in-question (apply 'encode-time time-in-question))

    (flet ((current-time () time-in-question))
      ;; flet temporarily binds current-time to this version, which
      ;; returns the time from yesterday 

      (org-todo)
      ;; toggles the todo heading
      )))

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


