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

;;;; Building blocks

(defun notmuch/cmd (cmd)
  "Call the notmuch command CMD."
  (s-trim (shell-command-to-string (format "notmuch %s" cmd))))

(defun notmuch/cmd* (cmd)
  "Call `notmuch/cmd' with CMD, and split the output on newlines."
  (-remove #'s-blank-str-p (s-split "\n" (notmuch/cmd cmd))))

(defun notmuch/count (query &optional output)
  "Count the number of messages which match QUERY."
  (let ((output (if output (format "--output=%s" output) "")))
    (notmuch/cmd (format "count %s %s" output query))))

(defun notmuch/search (query &optional output)
  "Search the notmuch database."
  (let* ((output (if output (format "--output=%s" output) ""))
         (cmd (s-join " " (list "search" output query))))
    (notmuch/cmd* cmd)))

(defun notmuch/get-files (query)
  "Return a list of filenames found from QUERY."
  (notmuch/search query 'files))

(defun notmuch/get-threads (query)
  "Return a list of thread-ids found from QUERY."
  (notmuch/search query 'threads))

(defun notmuch/get-messages (query)
  "Return a list of message-ids found from QUERY."
  (notmuch/search query 'messages))

(defun notmuch/get-files-by-tag (tag)
  "Return a list of filenames for files with TAG."
  (notmuch/get-files (format "tag:%s" tag)))

(defun notmuch/get-threads-by-tag (tag)
  "Return a list of thread-ids for threads with TAG."
  (notmuch/get-threads (format "tag:%s" tag)))

(defun notmuch/get-messages-by-tag (tag)
  "Return a list of message-ids for messages with TAG."
  (notmuch/get-messages (format "tag:%s" tag)))


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
