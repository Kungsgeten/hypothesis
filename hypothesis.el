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

(defvar hypothesis-username nil)
(defvar hypothesis-token nil)

(defun hypothesis-data (row)
  "Parse data from ROW into an alist."
  (let ((text (alist-get 'text row))
        (highlight (alist-get
                    'exact
                    (car (seq-filter
                          (lambda (selector)
                            (string-equal (alist-get 'type selector)
                                          "TextQuoteSelector"))
                          (alist-get 'selector (elt (alist-get 'target row) 0)))))))
    `((uri . ,(alist-get 'uri row))
      (title . ,(elt (alist-get 'title (alist-get 'document row)) 0))
      (text . ,text)
      (highlight . ,highlight)
      (type . ,(cond
                ((string-empty-p text) 'highlight)
                (highlight 'annotation)
                (t 'page-note))))))

(defun hypothesis-insert-site-data (site)
  "Insert the data from SITE as `org-mode' text."
  (insert (format "* [[%s][%s]]\n"
                  (car site)
                  (alist-get 'title (cadr site))))
  (dolist (x (cdr site))
    (when-let ((highlight (alist-get 'highlight x)))
      (insert (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE" highlight)))
    (when (eq 'annotation (alist-get 'type x))
      (insert "\n\n- "))
    (insert (concat (alist-get 'text x) "\n\n\n"))))

;;;###autoload
(defun hypothesis-to-org ()
  "Download data from hypothes.is and insert it into an `org-mode' buffer."
  (interactive)
  (if (and hypothesis-username hypothesis-token)
      (request
        "http://hypothes.is/api/search"
        :parser 'json-read
        :params `(("user" . ,(format "acct:%s@hypothes.is" hypothesis-username))
                  ("limit" . 50))
        :headers `(("Authorization" . ,(format "Bearer %s" hypothesis-token)))
        :type "GET"
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (let ((sites (seq-group-by (lambda (x) (alist-get 'uri x))
                                               (mapcar #'hypothesis-data (alist-get 'rows data)))))
                      (with-current-buffer (get-buffer-create "*hypothesis*")
                        (delete-region (point-min) (point-max))
                        (org-mode)
                        (mapc #'hypothesis-insert-site-data sites)))
                    (switch-to-buffer "*hypothesis*")
                    (goto-char (point-min)))))
    (user-error "`hypothesis-token' and `hypothesis-username' needs to be set")))

(provide 'hypothesis)
;;; hypothesis.el ends here
