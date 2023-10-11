;;; scrobble.el -- interfacing with last.fm

;; Copyright (C) 2011 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: home automation

;; This file is not part of GNU Emacs.

;; scrobble.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; scrobble.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides an interface to the (old-style) last.fm
;; scrobbling interface.  It tries to do this in a pretty reliable
;; way, and retries the scrobbling if last.fm is down, which it pretty
;; often is.

;; Usage: Set `scrobble-user' and `scrobble-password', and then just call
;; (scrobble artist album track duration).

;;; Code:

(require 'mm-url)

(defvar scrobble-login-urls
  '((:last "http://post.audioscrobbler.com/?hs=true&p=1.1&c=tst&v=10&u=%s")
    ;;(:libre "http://turtle.libre.fm/?hs=true&p=1.1&c=tst&v=10&u=%s")
    )
  "An alist of URLs to send scrobbles to.")

(defvar scrobble-user ""
  "last.fm user name.")

(defvar scrobble-password ""
  "Password to use.")

;; Internal variables.

(defvar scrobble-states nil)
(defvar scrobble-last nil)

(defun scrobble-login (service)
  (interactive)
  (with-temp-buffer
    (call-process
     "curl" nil (current-buffer) nil
     "-s" (format (cadr (assq service scrobble-login-urls)) scrobble-user))
    (goto-char (point-min))
    (when (looking-at "UPTODATE")
      (forward-line 1)
      (scrobble-set service :challenge
		    (buffer-substring (point) (line-end-position)))
      (forward-line 1)
      (scrobble-set service :url
		    (buffer-substring (point) (line-end-position))))))

(defun scrobble-set (service key value)
  (let ((state (assq service scrobble-states)))
    (unless state
      (setq state (list service))
      (push state scrobble-states))
    (setcdr state (plist-put (cdr state) key value))))

(defun scrobble-get (service key)
  (plist-get (cdr (assq service scrobble-states)) key))

(defun scrobble-encode (string)
  (mm-url-form-encode-xwfu (encode-coding-string string 'utf-8)))

(defun scrobble-do-not-scrobble (artist album song)
  "Say that we don't want to scrobble the song right now."
  (setq scrobble-last (list artist album song)))

(defun scrobble (artist album song &optional track-length cddb-id time)
  (let* ((spec (list artist album song))
	 (data (append spec (list (if time
				      (list (/ (truncate time) (expt 2 16))
					    (mod (truncate time) (expt 2 16)))
				    (current-time))
				  track-length cddb-id))))
    ;; If we're being called repeatedly with the same song, then
    ;; ignore subsequent calls.
    (when (not (equal spec scrobble-last))
      (setq scrobble-last spec)
      ;; Calls to last.fm may fail, so just put everything on the
      ;; queue, and flush the FIFO queue.
      (dolist (elem scrobble-login-urls)
	(when t
	  (unless (scrobble-get (car elem) :challenge)
	    (message "Logging in on %s" (car elem))
	    (scrobble-login (car elem))))
	(scrobble-set (car elem) :queue
		      (cons data (scrobble-get (car elem) :queue))))
      (scrobble-queue)
      )))

(defun scrobble-queue ()
  (dolist (elem scrobble-login-urls)
    (scrobble-queue-1 (car elem))))

(defun scrobble-queue-1 (service)
  (let ((queue (scrobble-get service :queue)))
    (when queue
      (scrobble-send service (car queue)))))

(defun scrobble-send (service spec)
  (cl-destructuring-bind (artist album song time track-length cddb-id) spec
    (let ((coding-system-for-write 'binary)
	  (url-request-data
	   (format "u=%s&s=%s&a[0]=%s&t[0]=%s&b[0]=%s&m[0]=%s&l[0]=%s&i[0]=%s"
		   scrobble-user
		   (md5 (concat (md5 scrobble-password)
				(scrobble-get service :challenge)))
		   (scrobble-encode artist)
		   (scrobble-encode song)
		   (scrobble-encode album)
		   ;; last.fm now ignores scrobbles that have the CDDB
		   ;; ID set.
		   ""			; (or cddb-id "")
		   (or track-length "")
		   (scrobble-encode
		    (format-time-string
		     "%Y-%m-%d %H:%M:%S"
		     (time-subtract time
				    (list 0 (car (current-time-zone))))))))
	  (url-request-extra-headers
	   '(("Content-Type" . "application/x-www-form-urlencoded")))
	  (url-request-method "POST"))
      (url-retrieve (scrobble-get service :url)
		    'scrobble-check-and-run-queue (list service spec)
		    t))))

(defun scrobble-check-and-run-queue (status service spec)
  (goto-char (point-min))
  (let ((buffer (current-buffer)))
    ;;(message "%s" (buffer-string))
    (when (search-forward "\n\n" nil t)
      (cond
       ((or (looking-at "BADAUTH")
	    (looking-at "BADSESSION"))
	(scrobble-login service)
	(when (scrobble-get service :queue)
	  (scrobble-queue-1 service)))
       ((looking-at "OK")
	(scrobble-set service :queue
		      (delete spec (scrobble-get service :queue)))
	(when (scrobble-get service :queue)
	  (scrobble-queue-1 service)))))
    (kill-buffer buffer)))

(defun scrobble-send-rockbox-log (file)
  (interactive "fScrobble log file: ")
  (let ((data nil))
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
	(unless (looking-at "#")
	  (push (split-string (buffer-substring (point) (line-end-position))
			      "\t")
		data))
	(forward-line 1))
      (setq data (nreverse data)))
    (pop-to-buffer "*log*")
    (erase-buffer)
    (cl-loop for (artist album track track-number
			 duration rating time musicbrainz)
	     in data
	     do (setq time (string-to-number time))
	     (when (< (string-to-number (format-time-string "%Y" time)) 1980)
	       (setq time (+ time 1383000000)))
	     (insert (format-time-string "%FT%T" time)
		     " " artist " " album " " track "\n")
	     (scrobble artist album track duration nil time))))

(provide 'scrobble)

;;; scrobble.el ends here
