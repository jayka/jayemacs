;;; early-init.el
;; Author: jayka (Ajay Kaarthick Premkumar)

;;; Commentary:
;; My personal Emacs config

;;; Code:


; Save default user directory
(setq-default user-main-emacs-directory user-emacs-directory)
(setq-default current-emacs-file (concat user-main-emacs-directory ".current"))


(defun switch-emacs (n)
  (interactive
   (list
    (read-string
       (format "%s : "
           (let ((emacs-dirs
                  (directory-files user-main-emacs-directory nil "^emacs-")))
             (cl-loop for i from 1 to (length emacs-dirs)
                      for d in emacs-dirs
                      collect (if (> i 1)
                                  (format ", %s) %s" i d)
                                (format "%s) %s" i d))))))))
  (let ((new-emacs
          (nth (- (string-to-number n) 1)
               (directory-files user-main-emacs-directory nil "^emacs-"))))
    (with-temp-buffer
      (insert new-emacs)
      (write-region nil nil current-emacs-file))
    (message "Emacs switched to %s. Restart for changes to take effect" new-emacs)))


(let ((currentemacs
       (let ((cfile current-emacs-file))
         (or (file-exists-p cfile) (write-region nil nil cfile))
         (with-temp-buffer
           (insert-file-contents cfile)
           (buffer-string)))))
  
  (setq-default
   user-emacs-directory (concat
                         user-emacs-directory
                         (car (split-string
                               currentemacs      
                               "\n"))
                         "/")))

(message "%s" user-emacs-directory)

(message "%s" user-init-file)
(message "%s" load-path)


;; One less file to load at startup
(setq site-run-file nil)

;; load appropriate file
(message "loading %s" current-emacs-file)

(setq load-file-name (concat user-emacs-directory "early-init.el"))
(and (file-exists-p (concat user-emacs-directory "early-init.el"))
     (with-temp-buffer
       (insert-file-contents (concat user-emacs-directory "early-init.el"))
       (eval-buffer)))

;;(load (concat user-emacs-directory "/early-init.el"))

;;(and (file-exists-p (concat user-emacs-directory "/early-init.el"))
;;     (load (concat user-emacs-directory "/early-init.el")))
;; (defun switch-emacs (emacs-to-switch)
;;   (interactive
;;    (list (directory-files user-main-emacs-directory nil "^emacs-")

;;          (read-string (format " %s" (directory-files user-main-emacs-directory nil "^emacs-")))))) 
;; ;;(setq-default package-user-dir "~/.emacs.d/usr/elpa/")
;;(setq testmsg "early inited")
;;(message "Early Init inited")
