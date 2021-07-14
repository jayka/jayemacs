;;; init.el -*- lexical-binding: t -*-
;; Author: jayka (Ajay Kaarthick Premkumar)

;;; Commentary:
;; My personal Emacs config

;;; Code:

;; (setq-default mac-command-modifier 'meta
              ;; mac-option-modifier 'super)
(setq-default tls-checktrust t)

;; Trusting bluecoat certs certificates
;;(setq gnutls-trustfiles '("/Users/ajpremkumar/Documents/MacOSCerts/certs.pem"))

(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

(setq-default user-full-name "Ajay Kaarthick Premkumar")

;; custom custom file
(setq custom-file (concat user-emacs-directory "custom.el")
      ;;initial-buffer-choice (concat user-emacs-directory "init.el")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
      undo-tree-auto-save-history t
      undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups/") t)))


;; (with-eval-after-load 'gnutls
  ;; (add-to-list 'gnutls-trustfiles "/Users/ajpremkumar/Documents/MacOSCerts/certs.pem"))

;;; Multi-Emacs config loading
;; When running multiple emacs configs put the below section in
;; ~/.emacs.d/init.el
;;(progn
;;  (require 'cl-lib)
;;  (setq-default user-emacs-directory (car
;;                                      (cl-loop for e in command-line-args
;;                                               with res = nil
;;                                               do (if (string-prefix-p "-VV" e)
;;                                                      (push (substring e 3) res))
;;                                               finally return res)))
;;  (load-file (concat user-emacs-directory "init.el")))

;; load-path directories
(let ((default-directory (concat user-emacs-directory "pkgs/")))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (concat user-emacs-directory "init/"))


(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(package-initialize)


;; Packages
(load "common")
(j/load 'settings)

;; use-package dependencies
(require 'use-package)
(setq use-package-always-ensure t)
(require 'diminish)
(require 'delight)
(require 'page-break-lines)
(use-package which-key)
(use-package bind-map)
(require 'jayemacs)


;;  Evil
(use-package evil
  :init
  (j/csetv evil-want-fine-undo "Yes"
           evil-kill-on-visual-paste t
           evil-want-Y-yank-to-eol t
           evil-want-C-u-scroll t
           evil-vsplit-window-right t
           evil-split-window-below t)

  (setq-default evil-symbol-word-search t)
  :bind
  (:map evil-motion-state-map
        ("U"   . 'undo-tree-redo))
  (:map evil-insert-state-map
        ("C-e" . 'end-of-line)
        ("C-a" . 'beginning-of-line)
        ("C-j" . 'evil-next-visual-line)
        ("C-k" . 'evil-previous-visual-line)
        ("C-h" . 'backward-char)
        ("C-l" . 'forward-char))
  :config
  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "g o") 'evil-jump-backward)
  (define-key evil-motion-state-map (kbd "g p") 'evil-jump-forward))

(use-package evil-escape
  :defer t
  :no-require t
  :after (evil))

(use-package evil-surround
  :no-require t
  :after (evil))

(use-package evil-visualstar
  :defer t
  :no-require t
  :after (evil))


;; All which-key names are here
(j/which-key-roots (jayemacs-leader-key jayemacs-alt-leader-key)
                   (nil '("root" . "Jayemacs Root"))
                   ("b" '("buffer" . "Buffer"))
                   ("c" '("code/comment" . "Code/Comment"))
                   ("w" '("window" . "Window"))
                   ("e" '("eval/error" . "Eval/Error"))
                   ("F" '("frame" . "Frame"))
                   ("f" '("file/directory" . "File/Directory"))
                   ("F" '("frames" . "Frames"))
                   ("f t" '("treemacs" . "Treemacs"))
                   ("p" '("project" . "Project"))
                   ("j" '("jump" . "Jump"))
                   ("s" '("search" . "Search"))
                   ("m" '("major-mode" . "Major Mode"))
                   ("o" '("org-mode" . "Org Mode"))
                   ("h" '("help" . "Help"))
                   ("g" '("git" . "Git"))
                   ("r" '("refactor" . "Refactor"))
                   ("q" '("quit/restart" . "Quit/Restart Emacs"))
                   ("t" '("toggle" . "Toggle")))


(use-package key-chord
  :no-require t
  :defer t)

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables (add-to-list 'exec-path-from-shell-variables "GOPATH"))
  (exec-path-from-shell-initialize))

(use-package ivy
  :defer t
  :no-require t
  :init
  (j/csetv ivy-use-virtual-buffers t
           ivy-modified-buffer t)
  :bind
  (:map ivy-minibuffer-map
        ("C-j" . 'ivy-next-line)
        ("C-k" . 'ivy-previous-line)
        ("C-d" . 'ivy-scroll-up-command)
        ("C-u" . 'ivy-scroll-down-command))
  ;; ("ESC" . 'minibuffer-keyboard-quit))
  (:map ivy-switch-buffer-map
        ("C-j" . 'ivy-next-line)
        ("C-k" . 'ivy-previous-line)
        ("C-S-k" . 'ivy-switch-buffer-kill)
        ("C-d" . 'ivy-scroll-up-command)
        ("C-u" . 'ivy-scroll-down-command))
  (:map global-map
        ("s-." . 'ivy-resume)))

(use-package ag
  :defer t
  :no-require t)
(use-package smex
  :defer t
  :no-require t)
(use-package counsel
  :defer t
  :no-require t)
(use-package swiper
  :defer t
  :no-require t)
(use-package hydra
  :defer t
  :no-require t
  :config
  (defhydra j/hydra-window-adjust-menu ()
    "window adjust"
    (">" (evil-window-increase-width 1) "increase width")
    ("<" (evil-window-decrease-width 1) "decrease width")
    ("+" (evil-window-increase-height 1) "increase height")
    ("-" (evil-window-decrease-height 1) "decrease height")
    ("l" windmove-right "right window")
    ("h" windmove-left "left window")
    ("k" windmove-up "up window")
    ("j" windmove-down "down window")
    ("d" delete-window "delete current window")
    ("b" balance-windows "balance all windows")
    ("q" nil "quit")))

(use-package ace-window
  :no-require t
  :config
    (setq aw-scope 'frame))
(use-package ace-link
  :defer t
  :no-require t)

(use-package expand-region
  :defer t
  :no-require t
  :config
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

(use-package treemacs
  :defer t
  :no-require t
  :config
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (setq mode-line-format nil)
              ;; (setq treemacs--width-is-locked nil)
                    ;; treemacs-icon-root "") ; remove mode line
              ;;(linum-mode 0)  ; disable line numbers
              ;;(fringe-mode 0)  ; no fringes
              (use-package treemacs-evil)
              (use-package treemacs-projectile))))


(use-package helpful
  :defer t
  :no-require t
  :init
  (evil-set-initial-state 'helpful-mode 'motion))

  ;; (eviljay-init-and-setup-mode 'helpful-mode))


(use-package restart-emacs
  :defer t
  :no-require t)

;; Editing
(use-package iedit
  :defer t
  :no-require t
  :init
  (defun j/iedit-mode-toggle ()
    (interactive)
    (if (bound-and-true-p iedit-mode)
        (iedit-quit)
      (iedit-mode))))

(use-package rainbow-delimiters
  :no-require t)

;; Version Control
(use-package magit
  :defer t
  :commands magit-status)

;; Project
(use-package projectile
  :defer t
  :no-require t
  :init
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recentf))

;; Org
(use-package org
  :defer t
  :no-require t
  :ensure org-plus-contrib
  :init
  ;; set org agenda files
  (setq org-directory "~/repo/ajays/org")
  (setq org-agenda-files (list "~/repo/ajays/org/"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; org templates
  (setq org-capture-templates '(("s" "Insert Solution, Impact, Questions." plain (file "") "%[/Users/ajpremkumar/repo/ajays/org/templates/dc.org]")
                                ("f" "Follow Up" entry (file+headline "" "Follow Ups") "* TODO %t %?"))))


(use-package org-bullets
  :defer t
  :no-require t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; Completion
(use-package company
  :defer t
  :no-require t
  :bind
  (:map company-active-map
        ("C-j" . 'company-select-next-or-abort)
        ("C-k" . 'company-select-previous-or-abort)))

;; Languages

(bind-map jayemacs-prog-mode-map
  :major-modes (emacs-lisp-mode)
  :evil-keys ("SPC SPC")
  :bindings
  ("b" 'eval-buffer
   "e" 'eval-last-sexp))

(use-package clj-refactor
  :after cider)

(use-package cider
  :no-require t
  :commands (cider-jack-in cider-eval-buffer)
  :init
  (bind-map jayemacs-clojure-mode-map
    :major-modes (clojure-mode)
    :evil-keys ("SPC SPC")
    :bindings
    ("c" 'cider-jack-in))
  (bind-map jayemacs-clojurescript-mode-map
    :major-modes (clojurescript-mode)
    :evil-keys ("SPC SPC")
    :bindings
    ("c" 'cider-jack-in-cljs))
  :config
  (bind-map jayemacs-cider-goto-mode-map
    :major-modes (clojure-mode cider-repl-mode)
    :minor-modes (cider-mode)
    :evil-keys ("g")
    :bindings
    ("d" 'cider-find-dwim)
    ("s" 'cider-doc)
    ("D" 'cider-clojuredocs-web))
  (bind-map jayemacs-cider-mode-map
    :major-modes (clojure-mode)
    :minor-modes (cider-mode)
    :evil-keys ("SPC SPC")
    :bindings
    ("b" 'cider-eval-buffer)
    ("e" 'cider-eval-last-sexp)
    ("s" 'cider-scratch))
  (eviljay-init-and-setup-modes
   'cider-stacktrace-mode))

(use-package go-mode
  :defer t
  :no-require t
  :init
  (use-package company-go)
  (use-package go-imports)
  (use-package go-playground)
  (use-package go-playground-cli)
  (use-package go-projectile)
  (use-package go-scratch)
  (use-package go-rename))

(use-package rust-mode
  :defer t)

(use-package lsp-mode
  :hook (
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
  (use-package dap-mode))
  

(use-package json-mode
  :defer t
  :no-require t)
(use-package js2-mode
  :defer t
  :no-require t)
(use-package fish-mode
  :defer t
  :no-require t
  :config
  (setq fish-indent-offset 2))
(use-package lispy
  :defer t
  :no-require t
  :init
  (add-hook 'lispy-mode-hook (lambda ()
                               (define-key lispy-mode-map "\C-S-k" 'lispy-kill))))

;;(require 'openapi-yaml-mode)

(use-package yaml-mode
  :defer t
  :no-require t)

;; Syntax checking
(use-package flycheck
  :defer t
  :no-require t)

(use-package parinfer
  :load-path "pkgs/parinfer"
  :init
  (setq parinfer-extensions
        '(defaults
           evil))
  (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  :config
  (parinfer--switch-to-paren-mode))

(use-package origami
  :no-require t
  :init
  (add-hook 'json-mode-hook (lambda() (origami-mode 1)))
  (add-hook 'prog-mode-hook (lambda() (origami-mode 1))))

;; Utilities

(use-package all-the-icons)

(use-package undo-tree
  :defer t
  :no-require t)

(use-package plantuml-mode
  :defer t
  :no-require t)

(use-package ledger-mode
  :defer t
  :no-require t)

(use-package auto-compile
  :no-require t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; (use-package dumb-jump
;;   :no-require t
;;   :bind
;;   (:map evil-motion-state-map
;;         ("gd"  . 'dumb-jump-go))
;;   :config
;;   (setq dumb-jump-selector 'ivy))

(use-package popwin
  :config
  (progn
    (push '("*Org Agenda*" :position right :width 40 :dedicated t :stick t) popwin:special-display-config)
    (push '("*xref*" :position bottom :height 14 :stick t) popwin:special-display-config)))
  


(use-package nix-mode
  :defer t
  :no-require t
  :mode "\\.nix\\'")

;; Global Usability customizations
(bind-key "s-=" 'text-scale-increase)
(bind-key "s-+" 'text-scale-increase)

(bind-key "s--" 'text-scale-decrease)
(bind-key "s-_" 'text-scale-decrease)

(bind-key "s-u" 'universal-argument)
(bind-key "s-k" 'j/move-line-up)
(bind-key "s-j" 'j/move-line-down)

;; which-key bindings
(bind-key "M" 'which-key-show-keymap help-map)
(bind-key "T" 'which-key-show-top-level help-map)

(bind-key "M-x" 'counsel-M-x)

(bind-key "M-`" 'other-frame)

;; Prefix keys

(bind-map help-map
  :evil-keys ("SPC h"))

(evil-define-command evil-vsplit-buffer (buffer)
  "Splits window vertically and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-window-vsplit)
  (evil-buffer buffer))



(j/bind-key jayemacs-root-map
            ;; Window switching
            ("1" . 'j/switch-to-window)
            ("2" . 'j/switch-to-window)
            ("3" . 'j/switch-to-window)
            ("4" . 'j/switch-to-window)
            ("5" . 'j/switch-to-window)
            ("6" . 'j/switch-to-window)
            ("7" . 'j/switch-to-window)
            ("8" . 'j/switch-to-window)
            ("9" . 'j/switch-to-window)
            ;; Code/Comment
            ("cl" . 'comment-line)
            ;; File/Directory/Find
            ("ff" . 'counsel-find-file)
            ("fl" . 'counsel-find-library)
            ("fr" . 'counsel-recentf)
            ("fii" . 'j/open-init-file)
            ("fid" . 'j/open-init-dir)
            ("fs" . 'save-buffer)
            ("fS" . 'write-file)
            ("fR" . 'rename-file)
            ("fd" . 'dired-jump)
            ("fD" . 'dired)
            ;; Treemacs
            ("0"  . 'treemacs-select-window)
            ("ftt" . 'treemacs)
            ("ftf" . 'treemacs-find-file)
            ("fta" . 'treemacs-add-project-to-workspace)
            ("ftd" . 'treemacs-remove-project-from-workspace)
            ("ftD" . 'treemacs-remove-workspace)
            ("ftr" . 'treemacs-rename-project)
            ("ftR" . 'treemacs-rename-workspace)
            ;; Frames
            ("Ff" . 'make-frame)
            ("Fd" . 'delete-frame)
            ("FD" . 'delete-other-frames)
            ;; Project
            ("pf" . 'projectile-find-file)
            ("pa" . 'j/add-projectile-project-to-treemacs)
            ("pp" . 'projectile-switch-project)
            ;; Frame
            ;; Buffer
            ("TAB" . 'evil-switch-to-windows-last-buffer)
            ("bb" . 'j/switch-to-buffer)
            ("be" . 'erase-buffer)
            ("bk" . 'kill-this-buffer)
            ("bS" . 'save-some-buffers)
            ("br" . 'revert-buffer)
            ("bR" . 'rename-buffer)
            ("bY" . 'j/yank-whole-buffer)
            ("bm" . 'j/switch-to-messages-buffer)
            ;; Window
            ("wb" . 'j/hydra-window-adjust-menu/body)
            ("wk" . 'kill-buffer-and-window)
            ("wd" . 'ace-delete-window)
            ("wD" . 'ace-delete-other-windows)
            ("wx" . 'ace-swap-window)
            ("wm" . 'delete-other-windows)
            ("wss" . 'evil-split-buffer)
            ("wsS" . 'evil-window-split)
            ("wsv" . 'evil-vsplit-buffer)
            ("wsV" . 'evil-window-vsplit)
            ("ww" . 'ace-window)
            ("wu" . 'winner-undo)
            ("wU" . 'winner-redo)
            
            ;; Jump
            ("jj" . 'evil-avy-goto-char-timer)
            ("jl" . 'evil-avy-goto-line)
            ;; Search
            ("ss" . 'swiper)
            ("sS" . 'swiper-all)
            ("sa" . 'swiper-thing-at-point)
            ("sA" . 'swiper-all-thing-at-point)
            ("sd" . 'j/ag-dir)
            ("sD" . 'j/ag-dir-thing-at-pt)
            ("sp" . 'j/ag-project-dir)
            ("sP" . 'j/ag-project-dir-thing-at-pt)
            ;; Visual
            ("v" . 'er/expand-region)
            ;; Org mode
            ("oa" . 'org-agenda)
            ("oc" . 'org-capture)
            ;; Help
            ("hf" . 'helpful-callable)
            ("hk" . 'helpful-key)
            ("hv" . 'helpful-variable)
            ("ht" . 'helpful-at-point)
            ;; Git
            ("gs" . 'magit-status)
            ("gd" . 'magit-file-dispatch)
            ("gD" . 'magit-dispatch)
            ("gb" . 'magit-blame)
            ;; Eval
            ("ep" . 'eval-print-last-sexp)
            ("ee" . 'eval-last-sexp)
            ("eb" . 'eval-buffer)
            ("en" . 'flycheck-next-error)
            ("eN" . 'flycheck-previous-error)
            ("el" . 'flycheck-list-errors)
            ;; Rename/Refactor
            ("re" . 'j/iedit-mode-toggle)
            ;; Quit/Restart
            ("qq" . 'j/quit)
            ("qQ" . 'j/quit-save-desktop)
            ("qr" . 'j/restart)
            ("qi" . 'j/restart-no-init)
            ("qR" . 'j/restart-save-desktop)
            ;; Toggle
            ("tf" . 'j/flycheck-toggle)
            ("tp" . 'paredit-mode)
            ("tl" . 'linum-mode)
            ("tw" . 'read-only-mode))



;; Themes
(add-hook 'after-init-hook
          (lambda()
            (progn
              (let ((themes '(gruvbox-theme color-theme-sanityinc-tomorrow kaolin-themes)))
                (dolist (theme themes)
                  ;;(message "installing theme: %s" theme)
                  (eval `(use-package ,theme :defer t))))

              ;; Activate theme
              (load-theme 'sanityinc-tomorrow-night t)
              ;; (load-theme 'gruvbox-dark-medium t)
                ;; (load-theme 'kaolin-dark t)
              (kaolin-treemacs-theme)
              ;; UI Setup
              ;; set a normal readable font size
              (set-face-attribute 'default nil :font "Hack Nerd Font" :weight 'normal :height 120))))
              ;; (custom-theme-set-faces 'sanityinc-tomorrow-night ((t (:background ))))


;; Universal modes

(progn
  (desktop-save-mode +1)
  (minibuffer-depth-indicate-mode +1)
  (blink-cursor-mode -1)
  (evil-mode +1)
  (evil-escape-mode +1)
  (global-evil-surround-mode)
  (global-evil-visualstar-mode)
  (ivy-mode +1)
  (recentf-mode +1)
  (which-key-mode +1)
  (global-undo-tree-mode t)
  (global-font-lock-mode)
  (global-company-mode)
  (projectile-mode)
  (winner-mode +1)
  (column-number-mode +1)
  (line-number-mode +1)
  (ace-link-setup-default)
  (show-paren-mode +1)
  (global-page-break-lines-mode)
  (global-flycheck-mode)
  ;; (treemacs)
  ;; (popwin-mode 1)
  ;; (push '("*Process List*" :position bottom) popwin:special-display-config)
  ;; (push '(dired-mode :position bottom) popwin:special-display-config)

  ;; (savehist-mode +1)
  ;; paredit
  (add-hook 'prog-mode-hook '(lambda ()
                               ;; (lispy-mode +1)
                               (rainbow-delimiters-mode-enable)
                               (prettify-symbols-mode +1)
                               (linum-mode +1)))

  ;; diminish obvious minor modes
  (diminish 'which-key-mode)
  (diminish 'evil-escape-mode)
  (diminish 'undo-tree-mode)
  (diminish 'ivy-mode)
  (diminish 'company-mode)
  (diminish 'eldoc-mode)
  (diminish 'page-break-lines-mode)

  ;; Other initial states

  (eviljay-init-and-setup-modes
   ;; 'helpful-mode
   'xref--xref-buffer-mode
   'magit-status-mode
   'magit-submodule-list-mode
   'magit-log-mode
   ;; 'magit-blob-mode
   'magit-diff-mode
   'magit-refs-mode
   'magit-blame-mode
   'magit-cherry-mode
   'magit-reflog-mode
   'magit-log-select-mode
   'magit-revision-mode
   'help-mode
   'package-menu-mode
   'Custom-mode))


;; (defconst emacs-load-time (- (current-time) emacs-start-time))
(message "Jayemacs loaded in %s" (emacs-init-time))


(run-with-idle-timer 2 nil #'garbage-collect)

;; Local Variables;
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; coding: utf8

;;; init.el ends here
