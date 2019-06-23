;;; notmuch-mojn-core.el. --- -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords:
;; Package-Version: 20190623
;; Version: 0.1

;;; Commentary:

;;; Code:

(require 'dash)
(require 's)

(defun notmuch/cmd (cmd)
  "Call the notmuch command CMD."
  (s-trim (shell-command-to-string (format "notmuch %s" cmd))))

(defun notmuch/cmd* (cmd)
  "Call `notmuch/cmd' with CMD, and split the output on newlines."
  (-remove #'s-blank-str-p (s-split "\n" (notmuch/cmd cmd))))

(defun notmuch-mojn-email-candidates ()
  "Return list of mail addresses from mails in the notmuch database."
  '())

(defun notmuch-mojn--count-unread (queries)
  "Like `notmuch-hello-query-counts', but add the count of
unread messages to the plist."
  (-map
   (lambda (e)
     (let* ((query (map-elt e :query))
            (unread-query (format "count %s and tag:unread" query)))
       (unless (s-contains-p "tag:unread" query)
         (map-put! e :unread-count
                   (string-to-number (notmuch/cmd unread-query))))))
   queries))

(defun notmuch-mojn--get-mail-data ()
  (let* ((data (notmuch-hello-query-counts
                notmuch-saved-searches
                :show-empty-searches notmuch-show-empty-saved-searches))
         (data (notmuch-mojn--count-unread data)))
    data))

(provide 'notmuch-mojn-core)
