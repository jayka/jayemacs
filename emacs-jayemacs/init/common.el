;;; common.el --- Common helper functions -*- lexical-binding: t -*-
;; Author: jayka (Ajay Kaarthick Premkumar)

;;; Commentary:
;; Common Helper functions to be used across Jayemacs.

;;; Code:

(require 'cl-lib)

(defmacro j/load (&rest files)
  "Load all FILES.  Convenience macro."
  (macroexp-progn
   (mapcar (lambda (file) `(load (symbol-name ,file))) files)))

(defmacro j/which-key-roots (leader-key &rest bindings)
  "Add bindings to `which-key-add-key-based-replacements`."
  (macroexp-progn
   (mapcar (lambda (key)
             (macroexp-progn
              (mapcar (lambda (binding)
                        `(which-key-add-key-based-replacements (concat ,key " " ,(car binding)) ,(cadr binding)))
                      bindings)))
           leader-key)))
  
(defmacro j/csetv (&rest rest)
  "Call `custom-set-variable` for all SYM VAL in REST."
  `(cl-loop for (SYM VAL) on '(,@rest) by #'cddr do
            (customize-set-variable SYM VAL)))

(defmacro j/bind-key (keymap &rest args)
  "Bind all KEYs to FUNCTIONs in ARGS via `bind-key` into KEYMAP."
  `(progn
     ,@(mapcar (lambda (x) `(bind-key ,(car x) ,(cdr x) ,keymap))
               args)))

(defun rebind-key (old-key new-key keymap)
  "Unbinds OLD-KEY binding to NEW-KEY binding in KEYMAP."
  (let ((command (lookup-key keymap old-key)))
    (unbind-key old-key keymap)
    (bind-key new-key command keymap)))

(defun j/yank-whole-buffer ()
  "Copy whole buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun j/switch-to-buffer ()
  "Set it as temporary buffer if its not a filebuffer"
  (interactive)
  (call-interactively #'ivy-switch-buffer)
  (if (not (buffer-file-name))
    (setq-local j--temp-buffer-identifier t)))
  

(defun j/switch-to-messages-buffer ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun j/restart-save-desktop ()
  (interactive)
  (j/quit-save-desktop nil t))

(defun j/restart ()
  (interactive)
  (j/quit-save-desktop t t))

(defun j/restart-no-init ()
  (interactive)
  (j/quit-save-desktop t t t))

(defun j/quit ()
  (interactive)
  (j/quit-save-desktop t))

(defun j/quit-save-desktop (&optional no-save restart no-init)
  (interactive)
  (if no-save
      (progn
        (desktop-remove)
        (desktop-save-mode-off))
    (desktop-save-mode 1))
  (if restart
      (if no-init
          (restart-emacs '("-Q"))
        (restart-emacs))
    (save-buffers-kill-terminal)))

(defun j/open-init-file ()
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun j/open-bash-profile-rc ()
  (interactive)
  (find-file "~/.bashrc"))

(defun j/open-init-dir ()
  (interactive)
  (counsel-find-file (concat user-emacs-directory "init/")))

(defun j/ag-dir (&optional search-dir use-thing-p ag-prompt)
  (interactive)
  (let* ((search-directory
          (or search-dir (read-directory-name "Search in directory: ")))
         (ag-prompt
          (concat "[" (file-name-nondirectory (directory-file-name search-directory))
                  "] Search: "))
         (thing
          (cond
           (use-thing-p (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                              (t (symbol-name (symbol-at-point)))))
           (t nil))))
    (counsel-ag thing search-directory nil ag-prompt)))

(defun j/ag-dir-thing-at-pt ()
  (interactive)
  (j/ag-dir nil t nil))

(defun j/ag-project-dir ()
  (interactive)
  (let ((project-search-dir (projectile-project-root)))
    (j/ag-dir project-search-dir nil project-search-dir)))

(defun j/ag-project-dir-thing-at-pt ()
  (interactive)
  (let ((project-search-dir (projectile-project-root)))
    (j/ag-dir project-search-dir t)))

(defun j/flycheck-toggle ()
  "Toggle ON/OFF FlyCheck mode."
  (interactive)
  (j/toggle-mode flycheck-mode))

(defmacro j/toggle-mode (mode)
  "Toggle ON/OFF MODE."
  `(if (bound-and-true-p ,mode)
      (,mode 0)
    (,mode 1)))

(define-globalized-minor-mode global-rainbow-delimiters-mode rainbow-delimiters-mode rainbow-delimiters-mode-enable)

(defun j/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun j/move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun j/switch-to-window ()
  "Switch immediately to window number arg"
  (interactive)
  (use-package ace-window)
  (aw-switch-to-window (nth (- (elt (this-command-keys-vector) (- (length (this-command-keys-vector)) 1)) 49) (aw-window-list))))

;; (require 'hydra)

;; (defhydra j/hydra-switch-buffer-menu ()
;;   "switch buffer"
;;   ("TAB" evil-prev-buffer)
;;   ("q" (lambda() (interactive) (switch-to-buffer j/hydra-switch-buffer-menu--current-buffer) (hydra--body-exit))))
;; 
;; 
;; 
;; (defun j/hydra-switch-buffer-menu ()
;;   (interactive)
;;   (setq j/hydra-switch-buffer-menu--current-buffer (current-buffer))
;;   (j/hydra-switch-buffer-menu/body))

;;; common.el ends here

