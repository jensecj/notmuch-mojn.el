;;; notmuch-mojn.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jens C. Jensen

;; Author: Jens C. Jensen <dev@mhqv.net>
;; Keywords: notmuch, mail
;; Package-Version: 20231024
;; Version: 0.2.1
;; Package-Requires: ((emacs "30.0.50") (dash "2.19.1")  (s "1.13.1"))

;;; Commentary:

;;; Code:

(require 'tabulated-list)
(require 'cl-lib)

(require 'dash)
(require 's)

(require 'notmuch-mojn-core)
(require 'notmuch-mojn-mute)

;;;; Settings

(defvar notmuch-mojn-pre-refresh-hook '()
  "Hooks to run before refreshing the notmuch database.")

(defvar notmuch-mojn-post-refresh-hook '(notmuch-mojn-revert-buffer)
  "Hooks to run after refreshing the notmuch database.")

(defvar notmuch-mojn-pre-fetch-hook '()
  "Hooks to run before fetching new mail.")

(defvar notmuch-mojn-post-fetch-hook '(notmuch-mojn-refresh)
  "Hooks to run after fetching new mail.")

(defvar notmuch-mojn-really-delete-mail nil
  "Whether to really delete email when calling `notmuch-mojn-delete-mail'.")

(defvar notmuch-mojn-fetch-function #'notmuch-mojn--fetch-mail-dwim
  "Function to use to fetch new mail.")

(defvar notmuch-mojn-buffer "*notmuch-mojn*"
  "Name of the notmuch-mojn buffer.")

(defvar notmuch-mojn-fetch-buffer "*notmuch-mojn-fetch-mail*"
  "Name of the buffer used by notmuch-mojn to fetch mail.")

(defface notmuch-mojn-unread-face
  '((t (:inherit notmuch-search-unread-face)))
  "Face used for entries which have unread mails.")

(defvar notmuch-mojn-fetch-command '("mbsync" "-q" "-V")
  "Command used to fetch new mail.")

(defvar notmuch-mojn-accounts-alist '((all . "-a"))
  "Alist of (MAIL ACCOUNT . ARG) for")

;;;; Core

(defun notmuch-mojn--fetch-mail (command)
  "Fetch new mail by shelling out to COMMAND."
  (let ((buf (get-buffer-create notmuch-mojn-fetch-buffer)))
    (make-process
     :name "notmuch-mojn-fetch-mail"
     :buffer buf
     :command command
     :sentinel (lambda (proc change)
                 (cond
                  ((string-match "finished" change)
                   (kill-buffer buf);; FIXME: buffer is empty but not killed?
                   (run-hooks 'notmuch-mojn-post-fetch-hook))
                  ((string-match "\\(exited|failed\\)" change)
                   (message "non-zero exit code: %s" change)))))
    (view-buffer buf #'kill-buffer)))

(defun notmuch-mojn--fetch-mail-dwim ()
  (when-let ((pick (completing-read "account: "
                                    (mapcar #'car notmuch-mojn-accounts-alist)
                                    nil t)))
    (notmuch-mojn--fetch-mail
     (-snoc notmuch-mojn-fetch-command
            (alist-get pick notmuch-mojn-accounts-alist nil nil #'string=)))))

(defun notmuch-mojn-fetch-mail ()
  "Calls `notmuch-mojn-fetch-function' to fetch new mail from the mailserver."
  (interactive)
  (message "notmuch-mojn: Fetching new mail.")
  (run-hooks 'notmuch-mojn-pre-fetch-hook)
  (funcall notmuch-mojn-fetch-function))

(defun notmuch-mojn-delete-mail ()
  "Delete the actual files on disk, for mail tagged with `deleted'."
  (interactive)
  (if notmuch-mojn-really-delete-mail
      (let* ((files (notmuch/get-files "tag:deleted")))
        (dolist (f files)
          (when (and (f-exists-p f) (f-file-p f))
            (message "deleting %s" f)
            (f-delete f)))
        (notmuch-mojn-refresh))
    (message "To delete mail, you need to set `notmuch-mojn-really-delete-mail' to `t'")))

;; TODO: rename notmuch-mojn-refresh to notmuch-mojn-poll? to reflect notmuch.el
(defun notmuch-mojn-refresh (&optional silent)
  "Calls `notmuch' to refresh the mailbox."
  (interactive)
  (unless silent
    (message "refreshing notmuch database..."))

  (run-hooks 'notmuch-mojn-pre-refresh-hook)

  (unless (process-live-p (get-process "notmuch-new"))
    (let ((res (notmuch/cmd "new")))
      (unless silent
        (message "%s" res))))

  (run-hooks 'notmuch-mojn-post-refresh-hook))

;;;; UI helpers

(defun notmuch-mojn--build-list-entry (entry)
  "Build a list entry for `tabulated-list-entries' from a
saved-search entry."
  (if-let* ((is-blank (not (map-elt entry :blank)))
            (name (map-elt entry :name ""))
            (key (map-elt entry :key ""))
            (count (map-elt entry :count 0))
            (unread (map-elt entry :unread-count 0))
            (mail-text (if (= 0 unread) (format "%s" count)
                         (propertize (format "%s (%s)" count unread) 'face 'notmuch-mojn-unread-face))))
      `[,key ,name ,mail-text]
    `["" "" ""]))

(defun notmuch-mojn-update-entries ()
  "Update the `tabulated-list-entries' for mojn,
recounting (un)read mail, etc."
  (setq tabulated-list-entries nil)
  (let* ((data (notmuch-mojn-get-saved-searches))
         (idx 0))
    (dolist (d data)
      (add-to-list 'tabulated-list-entries `(,idx ,(notmuch-mojn--build-list-entry d)) t)
      (cl-incf idx))))

(defun notmuch-mojn-search-entry (entry &optional tree)
  "Visit a saved-search entry with `notmuch-search'."
  (when-let ((is-blank (not (map-elt entry :blank)))
             (query (map-elt entry :query))
             (sort-order (map-elt entry :sort-order 'newest-first)))
    (if tree
        (notmuch-tree query)
      (notmuch-search query (not (eq sort-order 'newest-first))))))

(defun notmuch-mojn-search-entry-at-point (&optional tree)
  "View all mails for the saved-search entry at point."
  (interactive)
  (when-let* ((data (notmuch-mojn-get-saved-searches))
              (id (tabulated-list-get-id))
              (entry (nth id data)))
    (notmuch-mojn-search-entry entry tree)))

(defun notmuch-mojn-search-unread-entry-at-point (&optional tree)
  "View all unread mails for the saved-search entry at point."
  (interactive)
  (when-let* ((data (notmuch-mojn-get-saved-searches))
              (id (tabulated-list-get-id))
              (entry (nth id data))
              (q (map-elt entry :query)))
    (map-put! entry :query (concat q " and tag:unread"))
    (notmuch-mojn-search-entry entry tree)))

(defun notmuch-mojn-tree-entry-at-point ()
  "Visit the entry at point in tree view."
  (interactive)
  (notmuch-mojn-search-entry-at-point 'tree))

(defun notmuch-mojn-revert-buffer ()
  "Rebuild the buffer, updating entries if something has changed"
  (interactive)
  (when-let ((buf (get-buffer notmuch-mojn-buffer)))
    (with-current-buffer buf
      (notmuch-mojn-update-entries)
      (notmuch-refresh-this-buffer)
      (ignore-errors
        (revert-buffer)))))

;;;; Mode setup

(defvar notmuch-mojn-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; keys to replicate `notmuch-hello' behaviour
    (define-key map (kbd "j") #'notmuch-jump-search)
    (define-key map (kbd "s") #'notmuch-search)
    (define-key map (kbd "m") #'notmuch-mua-new-mail)
    ;; mojn specific keys
    (define-key map (kbd "<return>") #'notmuch-mojn-search-entry-at-point)
    (define-key map (kbd "C-<return>") #'notmuch-mojn-search-unread-entry-at-point)
    (define-key map (kbd "z") #'notmuch-mojn-tree-entry-at-point)
    (define-key map (kbd "g") #'notmuch-mojn-revert-buffer)
    (define-key map (kbd "u") #'notmuch-mojn-refresh)
    (define-key map (kbd "G") #'notmuch-mojn-fetch-mail)
    (define-key map (kbd "D") #'notmuch-mojn-delete-mail)
    map)
  "Local keymap for `notmuch-mojn-mode' buffers.")

(define-derived-mode notmuch-mojn-mode
  tabulated-list-mode "notmuch-mojn"
  "Notmuch hello variant, based on `tabulated-list-mode'."
  (setq tabulated-list-format [("Key" 5 nil) ("Name" 20 nil) ("Mail" 20 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

;;;###autoload
(defun notmuch-mojn ()
  "Open the `notmuch-mojn' dashboard."
  (interactive)
  (switch-to-buffer notmuch-mojn-buffer nil)
  (notmuch-mojn-mode)

  (notmuch-mojn-revert-buffer)

  (tabulated-list-print t)
  (goto-char (point-min)))

(provide 'notmuch-mojn)
