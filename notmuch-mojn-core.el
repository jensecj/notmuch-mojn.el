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

;;;; Settings

(defvar notmuch-mojn-completing-read-function #'completing-read
  "Function used for completing reads.")

(defvar notmuch-mojn-candidate-functions '()
  "List of function to call which supply additional candidates
  for address completion.

The function is called with no arguments, and should return a
list of strings.")

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
       (map-put! e :unread-count
                 (string-to-number (notmuch/cmd unread-query)))))
   queries))

(defun notmuch-mojn--clean-saved-searches ()
  "Return saved searches, replace `:blank' lines with entries
`notmuch' can understand."
  (-map-when
   (lambda (s) (map-elt s :blank))
   (lambda (s) (map-put! s :query "id:placeholder"))
   notmuch-saved-searches))

(defun notmuch-mojn--get-saved-searches ()
  "Return a list of saved searches (plists), augmented with the
number of unread and total number of mails."
  (let* ((searches (notmuch-mojn--clean-saved-searches))
         (data (notmuch-hello-query-counts searches :show-empty-searches t))
         (data (notmuch-mojn--count-unread data)))
    (-remove #'null data)))

;;;; Address completion

(defun notmuch-mojn--mail-candidates-notmuch ()
  "Return list of mail addresses from mails in the notmuch database."
  (unless notmuch-address-completions
    (notmuch-address-harvest nil t))

  (ht-keys notmuch-address-completions))

(defun notmuch-mojn-complete-address ()
  "Complete mail addresses using the senders/receivers harvested
by notmuch, and any additional mails collected from
`notmuch-mojn-candidate-functions'."
  (interactive)
  (let* ((notmuch-candidates (notmuch-mojn--mail-candidates-notmuch))
         (other-candidates (-mapcat #'funcall notmuch-mojn-candidate-functions))
         (candidates (remove-duplicates
                      (-concat notmuch-candidates other-candidates)
                      :test #'string=)))
    (insert (funcall notmuch-mojn-completing-read-function "Candidates: " candidates))))

(provide 'notmuch-mojn-core)
