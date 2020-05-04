;;; hypothesis.el --- hypothes.is API integration     -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/hypothesis
;; Keywords: hypermedia
;; Package-Requires: ((emacs "25.1") (org "9.2") (request "0.3.0"))
;; Version: 0.1

;;; Commentary:

;; Communicate between hypothes.is and Emacs.
;; Be sure to set `hypothesis-username' and `hypothesis-token'.
;; Currently only supports `org-mode' import with M-x hypothesis-to-org

;;; Code:

(require 'request)
(require 'json)
(require 'seq)
(require 'rx)
(require 'org)

(defvar hypothesis-username nil "Your username on hypothes.is.")
(defvar hypothesis-token nil "Your hypothes.is developer token.")
(defvar hypothesis-archive (expand-file-name "hypothesis.org" org-directory)
  "File which `hypothesis-to-archive' imports data into.")

(defvar hypothesis--site-level 1)
(defvar hypothesis--last-update nil)

(defun hypothesis-parse-iso-date (iso-date)
  "Run `date-to-time' on ISO-DATE (as given by hypothes.is)."
  (string-match (rx
                 (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)) ; Date
                 "T"
                 (group (= 2 digit) (= 2 ":" (= 2 digit))) ; Time
                 "."  (* digit)
                 (group (or "+" "-") (= 2 digit) ":" (= 2 digit)) ; Zone
                 )
                iso-date)
  (date-to-time
   (format "%s %s %s"
           (match-string 1 iso-date)
           (match-string 2 iso-date)
           (replace-regexp-in-string ":" ""  (match-string 3 iso-date)))))

(defun hypothesis-data (row)
  "Parse data from ROW into an alist."
  (let ((text (alist-get 'text row))
        (highlight (alist-get
                    'exact
                    (car (seq-filter
                          (lambda (selector)
                            (string-equal (alist-get 'type selector)
                                          "TextQuoteSelector"))
                          (alist-get 'selector (elt (alist-get 'target row) 0))))))
        (location-start (alist-get
                         'start
                         (car (seq-filter
                               (lambda (selector)
                                 (string-equal (alist-get 'type selector)
                                               "TextPositionSelector"))
                               (alist-get 'selector (elt (alist-get 'target row) 0)))))))
    `((uri . ,(alist-get 'uri row))
      (title . ,(elt (alist-get 'title (alist-get 'document row)) 0))
      (text . ,text)
      (highlight . ,highlight)
      (update-time . ,(hypothesis-parse-iso-date (alist-get 'updated row)))
      (location-start . ,location-start)
      (type . ,(cond
                ((string-empty-p text) 'highlight)
                (highlight 'annotation)
                (t 'page-note))))))

(defun hypothesis-insert-site-data (site)
  "Insert the data from SITE as `org-mode' text."
  ;;; i think i need to do the ordering here?
  (insert (format "%s [[%s][%s]]\n"
                  (make-string hypothesis--site-level ?*)
                  (car site)
                  (alist-get 'title (cadr site))))
  (dolist (x (sort (cdr site) (lambda (row1 row2) (< (alist-get 'location-start row1) (alist-get 'location-start row2)))))
    (org-insert-time-stamp (alist-get 'update-time x) t t nil "\n")
    (when-let ((highlight (alist-get 'highlight x)))
      (insert (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE" highlight)))
    (when (eq 'annotation (alist-get 'type x))
      (insert "\n\n- "))
    (insert (concat (alist-get 'text x) "\n\n\n"))))

(defun hypothesis-request (on-success &optional params exclude-user)
  "Make a request to hypothes.is/api/search and call function ON-SUCCESS.
ON-SUCCESS must take one argument: a list of `hypothesis-data' alists.

PARAMS is an alist of the uri parameters sent in the request.
However the `hypothesis-username' is also included unless EXCLUDE-USER is t."
  (if (and hypothesis-username hypothesis-token)
      (request
        "http://hypothes.is/api/search"
        :parser 'json-read
        :params (append params (unless exclude-user
                                 `(("user" . ,(format "acct:%s@hypothes.is"
                                                      hypothesis-username)))))
        :headers `(("Authorization" . ,(format "Bearer %s" hypothesis-token)))
        :type "GET"
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (let ((rows (alist-get 'rows data)))
                      (unless (seq-empty-p rows)
                        (setq hypothesis--last-update (alist-get 'updated (elt rows (1- (length rows))))))
                      (funcall on-success
                               (seq-group-by
                                (lambda (x) (alist-get 'uri x))
                                (mapcar #'hypothesis-data rows)))))))
    (user-error "`hypothesis-token' and `hypothesis-username' needs to be set")))

;;;###autoload
(defun hypothesis-to-org ()
  "Download data from hypothes.is and insert it into an `org-mode' buffer."
  (interactive)
  (hypothesis-request
   (lambda (sites)
     (with-current-buffer (get-buffer-create "*hypothesis*")
       (delete-region (point-min) (point-max))
       (org-mode)
       (mapc #'hypothesis-insert-site-data sites))
     (switch-to-buffer "*hypothesis*")
     (goto-char (point-min)))
   `(("limit" . 200))))

(defun hypothesis-last-archive-update ()
  "The last time the archive was updated.
Returns a string or nil."
  (unless (file-exists-p hypothesis-archive)
    (with-temp-file hypothesis-archive (insert "#+LAST_UPDATE:\n\n")))
  (let ((last-update
         (with-temp-buffer
           (insert-file-contents hypothesis-archive)
           (re-search-forward "^#\\+LAST_UPDATE:\\(.*\\)")
           (match-string 1))))
    (unless (string-empty-p last-update)
      last-update)))

;;;###autoload
(defun hypothesis-to-archive ()
  "Import notations into the `org-mode' file `hypothesis-archive'.
Only get notations made after the last import (up to 200)."
  (interactive)
  (let ((last-update (hypothesis-last-archive-update)))
    (hypothesis-request
     (lambda (sites)
       (find-file hypothesis-archive)
       (goto-char (point-max))
       (if (seq-empty-p sites)
           (message "Nothing new since last import.")
         (save-excursion
           (org-insert-time-stamp (current-time) nil t "* Imported on " "\n")
           (let ((hypothesis--site-level 2))
             (mapc #'hypothesis-insert-site-data sites))
           (re-search-backward "^#\\+LAST_UPDATE:\\(.*\\)")
           (kill-whole-line)
           (insert "#+LAST_UPDATE:" hypothesis--last-update "\n"))
         (save-buffer)))
     (append
      (when last-update `(("search_after" . ,last-update)))
      `(("limit" . 200)
        ("order" . "asc"))))))

(provide 'hypothesis)
;;; hypothesis.el ends here
