;;; jayemacs.el --- Jayemacs mode -*- lexical-binding: t -*- 
;; Version: 1.0

;;; Commentary:

;; My Jayemacs mode. write something useful later

;;; Code:

(require 'use-package)
(use-package evil)
(use-package bind-map)


(defvar jayemacs-leader-key "SPC" "This key belongs to ME.")
(defvar jayemacs-alt-leader-key "M-m" "M-m key belongs to ME.")
(defvar jayemacs-root-map (make-sparse-keymap) "Root keymap.")


(evil-define-state eviljay
  "Eviljay state"
  :tag "<J> "
  :message "---Jayemacs---"
  :cursor 'box
  :enable (emacs)
  (cond
   ((evil-eviljay-state-p)
    (message "Evil jay state p"))
   (t (message "Evil jay state t"))))

;; Root prefix
(bind-map jayemacs-root-map
  :keys (jayemacs-alt-leader-key)
  :evil-keys (jayemacs-leader-key)
  :evil-modes (eviljay)
  :override-minor-modes t
  :override-mode-name jayemacs-leader-override-mode)


(defmacro get-symbol-with (mode str)
  "Concatenate MODE & STR and return the symbol."
  `(intern (concat (symbol-name ,mode) ,str)))

(defmacro eviljay-lookup (mode key)
  `(lookup-key (symbol-value (get-symbol-with mode "-map")) ,key))


(defun eviljay-init-and-setup-mode (mode &optional hook)
  (evil-set-initial-state mode 'eviljay)
  (add-hook (get-symbol-with mode "-hook")
            (lambda ()
              (cond
               ;; ((evil-eviljay-state-p) (evil-normal-state))
               (t (message "Entering eviljay for mode %s" (symbol-name mode))
                  (setq-local evil-eviljay-state-local-map-tmp (make-sparse-keymap))
                  ;; (evil-eviljay-state)
                  ;; (message "Running eviljay for mode %s" (symbol-name mode))
                  (dolist (key (list "j" "k" "l" "h" "/" "n" "N" "*" "q" "C-d" "C-u" jayemacs-leader-key))
                    (let ((command (eviljay-lookup mode key)))
                      (and command
                           (cond ((null (eviljay-lookup mode (upcase key))) (define-key evil-eviljay-state-local-map-tmp (upcase key) command))
                                 ((null (eviljay-lookup mode (concat "C-" key))) (define-key evil-eviljay-state-local-map-tmp (concat "C-" key) command))
                                 ((null (eviljay-lookup mode (concat "C-S-" key))) (define-key evil-eviljay-state-local-map-tmp (concat "C-S-" key) command))
                                 ((null (eviljay-lookup mode (concat "s-" key))) (define-key evil-eviljay-state-local-map-tmp (concat "s-" key) command))))))
                  
                  (define-key evil-eviljay-state-local-map-tmp " " jayemacs-root-map)
                  (define-key evil-eviljay-state-local-map-tmp "j" 'evil-next-visual-line)
                  (define-key evil-eviljay-state-local-map-tmp "k" 'evil-previous-visual-line)
                  (define-key evil-eviljay-state-local-map-tmp "h" 'evil-backward-char)
                  (define-key evil-eviljay-state-local-map-tmp "l" 'evil-forward-char)
                  (define-key evil-eviljay-state-local-map-tmp "/" 'evil-search-forward)
                  (define-key evil-eviljay-state-local-map-tmp "n" 'evil-search-next)
                  (define-key evil-eviljay-state-local-map-tmp "N" 'evil-search-previous)
                  (define-key evil-eviljay-state-local-map-tmp "*" 'evil-search-word-forward)
                  (define-key evil-eviljay-state-local-map-tmp "q" 'quit-window)
                  (define-key evil-eviljay-state-local-map-tmp "\C-d" 'evil-scroll-down)
                  (define-key evil-eviljay-state-local-map-tmp "\C-u" 'evil-scroll-up)
                  ;; (message "After: %s" evil-eviljay-state-local-map)
                  (setq evil-eviljay-state-local-map (make-composed-keymap (list evil-eviljay-state-local-map-tmp evil-motion-state-map))))))))
                  ;; (message "Before: %s" evil-eviljay-state-local-map))))))





(defun eviljay-init-and-setup-modes (&rest modes)
  (dolist (mode modes) (eviljay-init-and-setup-mode mode)))


(provide 'jayemacs)


;;; jayemacs.el ends here
