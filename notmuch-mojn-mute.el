;;; notmuch-mojn-mute.el. --- -*- lexical-binding: t; -*-

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

(defun notmuch-mojn-mute--get-muted-threads ()
  "Return a list of ids for all muted threads."
  (notmuch/get-threads-by-tag "muted"))

(defun notmuch-mojn-mute--get-unmuted-messages (thread)
  "Return a list of id for all messages that belong to THREAD,
and do not have the +muted tag."
  (notmuch/search (format "%s and not tag:muted" thread) 'messages))

(defun notmuch-mojn-mute-retag-messages ()
  "If there exist messages which belong to a thread which has
the +muted tag, make sure the message is muted."
  (let ((muted-threads (notmuch-mojn-mute--get-muted-threads))
        (unmuted-messages))
    (dolist (th muted-threads unmuted-messages)
      (setq unmuted-messages
            (-concat unmuted-messages
                     (notmuch-mojn-mute--get-unmuted-messages th))))

    (dolist (id unmuted-messages)
      (notmuch/cmd (format "tag +muted -unread -- %s" id))
      (message "notmuch-mojn: muting %s" id))

    (when unmuted-messages
      (message "notmuch-mojn: muted %s new message(s)." (length unmuted-messages)))))

(provide 'notmuch-mojn-mute)
