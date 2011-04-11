(defvar jukebox-lastfm-password "")
(defvar jukebox-lastfm-user "")
(defvar jukebox-lastfm-challenge "")
(defvar jukebox-lastfm-url "")

(defun jukebox-login-lastfm ()
  (interactive)
  (with-temp-buffer
    (call-process "curl" nil (current-buffer) nil
		  "-s"
		  (format
		  "http://post.audioscrobbler.com/?hs=true&p=1.1&c=tst&v=10&u=%s"
		  jukebox-lastfm-user))
    (goto-char (point-min))
    (when (looking-at "UPTODATE")
      (forward-line 1)
      (setq jukebox-lastfm-challenge
	    (buffer-substring (point) (point-at-eol)))
      (message "Logged in to Last.fm with challenge %s"
	       jukebox-lastfm-challenge)
      (forward-line 1)
      (setq jukebox-lastfm-url
	    (buffer-substring (point) (point-at-eol))))))

(defun jukebox-scrobble-encode (string)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file "/tmp/scrobble.string"
      (insert string)))
  (with-temp-buffer
    (insert-file-contents "/tmp/scrobble.string")
    (mm-url-form-encode-xwfu (buffer-string))))

(defvar jukebox-last-scrobbled-file nil)
(defvar jukebox-disable-scrobble nil)

(defun jukebox-disable-scrobble (&optional file)
  (when (not (zerop jukebox-current-play-number))
    (unless file
      (setq file jukebox-current-track))
    (setq file (replace-regexp-in-string ".flac$" ".mp3" file))
    (setq jukebox-last-scrobbled-file file)))

(defvar jukebox-scrobble-queue nil)

(defun jukebox-scrobble (file)
  (setq file (replace-regexp-in-string ".flac$" ".mp3" file))
  (when (and (not (assoc file jukebox-scrobble-queue))
	     (string-match "^/music" file)
	     (not (equal file jukebox-last-scrobbled-file)))
    (message "Adding %s" file)
    (push (list file (current-time)) jukebox-scrobble-queue)
    (jukebox-scrobble-queue))
  (jukebox-disable-scrobble file))

(defun jukebox-scrobble-queue ()
  (dolist (spec (reverse jukebox-scrobble-queue))
    (let ((file (car spec))
	  (time (cadr spec)))
    (let ((response (jukebox-scrobble-2 file time)))
      (when (zerop (length response))
	(message "No scrobble response")
	(return nil))
      (message "Response '%s'" response)
      (when (string-match "OK" response)
	(message "Ok %s" file)
	(setq jukebox-scrobble-queue
	      (delete (assoc file jukebox-scrobble-queue)
		      jukebox-scrobble-queue)))))))
  
(defun jukebox-scrobble-2 (file time)
  (setq file (replace-regexp-in-string ".flac$" ".mp3" file))
  (message "Scrobbling %s" (file-name-nondirectory file))
  (let ((response (jukebox-scrobble-1 file time)))
    (when (string-match "BADAUTH" response)
      (jukebox-login-lastfm)
      (setq response (jukebox-scrobble-1 file time)))
    response))

(defun jukebox-scrobble-1 (file time)
  (let ((cddb-id "")
	(artist (jukebox-path-element 2 file))
	(album (jukebox-path-element 1 file))
	(song (jukebox-path-element 0 file))
	(track-number 0)
	track-length)
    ;; When using id3, which we don't, except for the Stuff tracks.
    (when (string-match "/repository/Stuff/" file)
      (if (jukebox-find-file-from-stuff file)
	  (setq file (jukebox-find-file-from-stuff file)
		artist (jukebox-path-element 2 file)
		album (jukebox-path-element 1 file)
		song (jukebox-path-element 0 file)))
      (with-temp-buffer
	(call-process "id3tool" nil (current-buffer) nil file)
	(goto-char (point-min))
	(when (re-search-forward "title:[ \t]*\\(.*\\)" nil t)
	  (setq song (replace-regexp-in-string " +$" "" (match-string 1))))
	(goto-char (point-min))
	(when (re-search-forward "artist:[ \t]*\\(.*\\)" nil t)
	  (setq artist (replace-regexp-in-string " +$" "" (match-string 1))))
	(goto-char (point-min))
	(when (re-search-forward "album:[ \t]*\\(.*\\)" nil t)
	  (setq album (replace-regexp-in-string " +$" "" (match-string 1))))))
    (when (string-match "^\\([0-9][0-9]\\)" song)
      (setq track-number (string-to-number (match-string 1 song))))
    (setq song (replace-regexp-in-string "^[0-9][0-9]-" "" song))
    (setq song (replace-regexp-in-string ".mp3$" "" song))
    (when (and (or t
		   (or (equal artist "Various")
		       (equal artist "Split")))
	       (string-match "\\(.*\\) - \\(.*\\)" song))
      (setq artist (match-string 1 song)
	    song (match-string 2 song)))
    (when (or (equal song "(untitled)")
	      (equal song "track"))
      (setq song (format "%s %s" album track-number)))
    (let ((id-file (expand-file-name "stats" (file-name-directory file))))
      (when (file-exists-p id-file)
	(with-temp-buffer
	  (insert-file-contents id-file)
	  (when (re-search-forward "CDDB: \\(.*\\)" nil t)
	    (setq cddb-id (match-string 1)))
	  (dolist (elem (jukebox-read-stats id-file))
	    (when (equal (car elem) (file-name-nondirectory file))
	      (setq track-length (jukebox-raw-to-seconds
				  (nth 2 elem))))))))
    (let ((coding-system-for-write 'binary))
      (with-temp-file "/tmp/scrobble"
	(insert
	 (format "u=%s&s=%s&a[0]=%s&t[0]=%s&b[0]=%s&m[0]=%s&l[0]=%s&i[0]=%s"
		 jukebox-lastfm-user
		 (md5 (concat (md5 jukebox-lastfm-password)
			      jukebox-lastfm-challenge))
		 (jukebox-scrobble-encode artist)
		 (jukebox-scrobble-encode song)
		 (jukebox-scrobble-encode album)
		 cddb-id track-length
		 (jukebox-scrobble-encode
		  (format-time-string
		   "%Y-%m-%d %H:%M:%S"
		   (time-subtract time
				  (list 0 (car (current-time-zone))))))))))
    (jukebox-ping-lastfm)))

(defun jukebox-ping-lastfm ()
  (with-temp-buffer
    (call-process "curl" nil (current-buffer) nil
		  "-d" "@/tmp/scrobble" "-s"
		  "-m" "1"
		  jukebox-lastfm-url)
    (replace-regexp-in-string "\n" " " (buffer-string))))

(defun jukebox-compute-mean-track-length ()
  (dolist (id-file stats)
    (setq id-file (expand-file-name "stats" (car id-file)))
    (dolist (elem (jukebox-read-stats id-file))
      (push (jukebox-raw-to-seconds (nth 2 elem))
	    lengths))))

(defun jukebox-ping-lastfm-async ()
  (message "Scrobbling async")
  (start-process
   "scrobble" (get-buffer-create "*scrobble*")
   "curl"
   "-d" "@/tmp/scrobble" "-s"
   "-m" "600"
   jukebox-lastfm-url))

(defconst mm-url-unreserved-chars
  '(
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

(defun mm-url-form-encode-xwfu (chunk)
  "Escape characters in a string for application/x-www-form-urlencoded.
Blasphemous crap because someone didn't think %20 was good enough for encoding
spaces.  Die Die Die."
  ;; This will get rid of the 'attributes' specified by the file type,
  ;; which are useless for an application/x-www-form-urlencoded form.
  (if (consp chunk)
      (setq chunk (cdr chunk)))

  (mapconcat
   (lambda (char)
     (cond
      ((= char ?  ) "+")
      ((memq char mm-url-unreserved-chars) (char-to-string char))
      (t (upcase (format "%%%02x" char)))))
   ;; Fixme: Should this actually be accepting multibyte?  Is there a
   ;; better way in XEmacs?
   (if (featurep 'mule)
       (encode-coding-string chunk
			     (if (fboundp 'find-coding-systems-string)
				 (car (find-coding-systems-string chunk))
				 buffer-file-coding-system))
     chunk)
   ""))

