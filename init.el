;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
;; Get rid of start up message
(setq inhibit-startup-message t)

(scroll-bar-mode -1)     ;disable visible scrollbar
;; (tool-bar-mode -1)       ; Disable the tool bar
;; (tooltip-mode -1)        ; Disable tool tips
(set-fringe-mode 10)     ;Give some breathing room
;; (menu-bar-mode -1)       ;Disable the menu bar
(setq visible-bell t)    ; make bell visible instead of audio

(setq backup-by-copying t)   ;; Attempt to avoid dropbox warning messages

;; text appearance

(load-theme 'tango-dark)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(require 'graphene)

(require 'org-macs)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hood))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . 'swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-day-and-date t)
 '(global-command-log-mode t)
 '(ivy-mode t)
 '(org-agenda-files
   (quote
    ("c:/Users/bert/Dropbox/Org/org1.org" "c:/Users/bert/Dropbox/Org/TutorialOrg.org")))
 '(package-selected-packages
   (quote
    (help-find-org-mode counsel org-bullets graphene flyspell-correct ergoemacs-mode which-key rainbow-delimiters doom-modeline ivy command-log-mode use-package org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; (set-face-attribute 'default nil :font "Fira Code Retina" :height 280)


                                        ; Org-mode config
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
