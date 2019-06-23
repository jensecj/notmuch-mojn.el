;;; notmuch-mojn.el. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords:
;; Package-Version: 20190623
;; Version: 0.1

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(require 'notmuch-mojn-core)
(require 'notmuch-mojn-mute)

;;;; Settings / Vars

(defvar notmuch-mojn-refresh-hook '(notmuch-mojn-mute-retag-messages)
  "Hooks to run after refreshing the notmuch database.")

;;;; Core

(defun notmuch-mojn-update-entries ()
  (setq tabulated-list-entries nil)
  (let* ((data (notmuch-mojn--get-mail-data))
         (idx 0))
    (dolist (d data)
      (let ((key (map-elt d :key ""))
            (name (map-elt d :name ""))
            (count (map-elt d :count ""))
            (unread (map-elt d :unread-count "")))
        (add-to-list 'tabulated-list-entries `(,idx [,key ,name ,(format "%s / %s" unread count)]))
        (incf idx)))))

(defun notmuch-mojn-visit-entry (entry)
  (when-let ((query (map-elt entry :query))
             (name (map-elt entry :name))
             (sort-order (map-elt entry :sort-order)))
    (notmuch-search query (not (eq sort-order 'newest-first)))))

(defun notmuch-mojn-visit-entry-at-point ()
  (interactive)
  (when-let* ((data (notmuch-mojn--get-mail-data))
              (id (tabulated-list-get-id))
              (entry (nth id data)))
    (notmuch-mojn-visit-entry entry)))

(defun notmuch-mojn-refresh (&optional silent)
  "Calls `notmuch' to refresh the mailbox."
  (interactive)
  (message "refreshing notmuch...")
  (unless (process-live-p (get-process "notmuch-new"))
    (let ((res (notmuch/cmd "new")))
      (unless silent
        (message "%s" res))
      (run-hooks 'notmuch-mojn-refresh-hook)))
  (notmuch-mojn-update-entries)
  ;; (set-window-start (selected-window) (point-min)) ; because it moves down for some reason?
  (notmuch-refresh-this-buffer)
  (revert-buffer))

;;;; The Mode

(define-derived-mode notmuch-mojn-mode tabulated-list-mode "notmuch-mojn"
  "Notmuch hello variant, based on `tabulated-list-mode'."
  (setq tabulated-list-format [("Key" 5 t)
                               ("Name" 15 t)
                               ("Mail" 15 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . t))
  (setq tabulated-list--header-string " ")
  (setq header-line-format " ")

  (tabulated-list-init-header))

;;;###autoload
(defun notmuch-mojn ()
  (interactive)
  (switch-to-buffer "*notmuch mojn*" nil)
  (notmuch-mojn-mode)

  (notmuch-mojn-update-entries)

  ;; (setq tabulated-list-entries '((1 ["i"  "inbox"    "2 / 12"]) (2 ["a"  "all mail" "3 / 50"])))

  (tabulated-list-print t)
  (goto-char (point-min)))

;;;; Keybindings

(define-key notmuch-mojn-mode-map (kbd "<return>") #'notmuch-mojn-visit-entry-at-point)

(define-key notmuch-mojn-mode-map (kbd "j") #'notmuch-jump-search)
(define-key notmuch-mojn-mode-map (kbd "s") #'notmuch-search)

(define-key notmuch-mojn-mode-map (kbd "g") #'notmuch-mojn-refresh)
(define-key notmuch-mojn-mode-map (kbd "G") #'jens/notmuch-fetch-mail)


(provide 'notmuch-mojn)
