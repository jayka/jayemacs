;;; init.el -*- lexical-binding: t -*-
;; Author: jayka (Ajay Kaarthick Premkumar)

;;; Commentary:
;; Choose the emacs you wish to run!!

;;; Code:

(and (boundp 'user-emacs-directory)
     (load-file (concat user-emacs-directory "init.el")))

