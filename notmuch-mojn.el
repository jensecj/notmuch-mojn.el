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
(require 'cl-lib)
(require 'tabulated-list)

(require 'notmuch-mojn-core)
(require 'notmuch-mojn-mute)

;;;; Settings / Vars

(defvar notmuch-mojn-refresh-hook '(notmuch-mojn-mute-retag-messages)
  "Hooks to run after refreshing the notmuch database.")

(defface notmuch-mojn-unread-face
  '((t (:inherit notmuch-search-unread-face)))
  "Face used for entries which have unread mails.")

;;;; Core

(defun notmuch-mojn-fetch-mail ()
  "Calls `mbsync' to fetch new mail from the mailserver."
  (interactive)
  ;; TODO: make async, and show results in echo-area, ala. `mu4e'.
  (message "notmuch: Fetching new mail.")
  (message (s-trim (shell-command-to-string "mbsync -a")))
  (jens/notmuch-refresh))

(defun notmuch-mojn-delete-mail ()
  "Delete the actual files on disk, for mail tagged with `deleted'."
  (interactive)
  (let* ((files (notmuch/get-mail-files-with-tag "deleted")))
    (dolist (f files)
      (when (and (f-exists-p f) (f-file-p f))
        (message "deleting %s" f)
        (f-delete f))
      (notmuch-mojn-refresh))))

(defun notmuch-mojn--build-list-entry (entry)
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
  (setq tabulated-list-entries nil)
  (let* ((data (notmuch-mojn--get-saved-searches))
         (idx 0))
    (dolist (d data)
      (add-to-list 'tabulated-list-entries `(,idx ,(notmuch-mojn--build-list-entry d)) t)
      (cl-incf idx))))

(defun notmuch-mojn-visit-entry (entry)
  (when-let ((is-blank (not (map-elt entry :blank)))
             (query (map-elt entry :query))
             (sort-order (map-elt entry :sort-order 'newest-first)))
    (notmuch-search query (not (eq sort-order 'newest-first)))))

(defun notmuch-mojn-visit-entry-at-point ()
  (interactive)
  (when-let* ((data (notmuch-mojn--get-saved-searches))
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
  (when (eq major-mode 'notmuch-mojn-mode)
    (notmuch-mojn-update-entries)
    (revert-buffer))
  ;; (set-window-start (selected-window) (point-min)) ; because it moves down for some reason?
  (notmuch-refresh-this-buffer))

;;;; The Mode
;; https://gitlab.com/ambrevar/emacs-disk-usage/blob/master/disk-usage.el

(defvar notmuch-mojn-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    ;; keys to replicate `notmuch-hello' behaviour
    (define-key map (kbd "j") #'notmuch-jump-search)
    (define-key map (kbd "s") #'notmuch-search)
    (define-key map (kbd "m") #'notmuch-mua-new-mail)
    ;; mojn specific keys
    (define-key map (kbd "<return>") #'notmuch-mojn-visit-entry-at-point)
    (define-key map (kbd "g") #'notmuch-mojn-refresh)
    (define-key map (kbd "G") #'notmuch-mojn-fetch-mail)
    (define-key map (kbd "D") #'notmuch-mojn-delete-mail)
    map)
  "Local keymap for `notmuch-mojn-mode' buffers.")

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

  (notmuch-mojn-refresh)

  ;; (setq tabulated-list-entries '((1 ["i"  "inbox"    "2 / 12"]) (2 ["a"  "all mail" "3 / 50"])))

  (tabulated-list-print t)
  (goto-char (point-min)))

;;;; Keybindings


(provide 'notmuch-mojn)
