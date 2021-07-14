;;; settings.el --- Global Settings go here -*- lexical-binding: t -*-
;; Settings
(j/csetv ;package-enable-at-startup nil
 inhibit-startup-screen t
 ring-bell-function (lambda() (invert-face 'mode-line) (run-with-timer 0.1 nil #'invert-face 'mode-line)) ; no ding sound. only flash mode-line
 visible-bell nil
 ;; indent uses spaces to tabs
 indent-tabs-mode nil
 tool-bar-mode nil
 menu-bar-mode t
 scroll-bar-mode nil
 tab-width 2
 js-indent-level 2
 scroll-step 1
 enable-recursive-minibuffers t
 eval-expression-print-length nil
 eval-expression-print-level nil
 disabled-command-function nil
 left-fringe-width 4
 load-prefer-newer t)

;; start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(fset 'yes-or-no-p 'y-or-n-p)

;; save minibuffer history
(savehist-mode 1)

;; dired customizations
(require 'dired)
(add-hook 'dired-mode-hook
          (lambda ()
            ;; shortcut for navigating to parent directory
            (define-key dired-mode-map (kbd "u") (lambda () (interactive) (find-alternate-file "..")))
            (define-key dired-mode-map (kbd "U") 'dired-unmark)
            (define-key dired-mode-map (kbd "^") 'dired-unmark-all-marks)))

(add-hook 'text-mode-hook
          (lambda ()
            (toggle-truncate-lines 0)
            (toggle-word-wrap 0)))

(advice-add 'xref-goto-xref :before
          (lambda ()
            (setq-local xref--original-window-intent 'window)))
            

;; Make Help window active when it pops up
(advice-add 'help-mode-setup :after (lambda () (switch-to-buffer-other-window (current-buffer))))

;; Prompt before killing modified/unsaved buffers
(defun -j/kill-buffer-ask-query-function ()
  (let ((buf-name (buffer-name (current-buffer))))
    ;; (message "name: %s" buf-name)
    ;;(or ;;(string-prefix-p " *" buf-name)
      ;;  (and (buffer-file-name) (not (buffer-modified-p)))
        ;;(and (not (buffer-file-name)) buffer-read-only)
    (if (and (boundp 'j--temp-buffer-identifier)
             j--temp-buffer-identifier
             (buffer-modified-p))
        (yes-or-no-p
         (format "Buffer %s is un-saved/modified. Kill?" (buffer-name (current-buffer))))
        t)))

(add-to-list 'kill-buffer-query-functions '-j/kill-buffer-ask-query-function t)
(add-to-list 'kill-emacs-query-functions '-j/kill-buffer-ask-query-function t)

;; Global key-bindings
(global-set-key (kbd "s-`") 'other-window)
