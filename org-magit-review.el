;;; org-magit-review.el --- Org/magit-based assist for email-communicated code review.

;; Copyright (C) 2014-2015 Kosyrev Serge

;; Author: Kosyrev Serge <_deepfire@feelingofgreen.ru>
;; Created: 6 Dec 2014
;; Keywords: git
;; Homepage: https://github.com/deepfire/org-magit-review
;; Version: 0.0.1
;; Package-Requires: ((magit) (org))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;;
;;; `org-magit-review' provides code review automation, combining log and commit buffers of
;;; magit with Org-mode's outline for review composition.
;;;
;;; The workflow looks roughly like this:
;;;
;;; - while not satisfied with branch
;;;   - for every commit that looks like it would benefit from comments
;;;     - for every hunk that needs attention
;;;       - send either the whole hunk, or its selected part into the review buffer for the
;;;         current branch, using `org-magit-review'
;;; - once satisfied, invoking `org-magit-review-to-mail-compose' will compose an email
;;;   message to the author of the first commit you reviewed, inserting the buffer stripped
;;;   of its meta-information.  You can edit the list of authors in the review buffer.
;;;
;;; NOTE: O-M-R groups all review messages for a given commit together, so one doesn't need to
;;; pay attention to that.  The order of commits, though, is dictated by the order they are
;;; first seen -- but since Org makes reordering them so easy it shouldn't be a problem.
;;;
;;; O-M-R supports one review per branch -- it persists incomplete reviews in files:
;;;
;;;   <repo-topdir>/.review-<branch>.org
;;;
;;; O-M-R caches the commit-branch correspondence, as well as it tracks one default branch
;;; per repository.  Sometimes you want to change the default branch, and so you might
;;; want to either call `org-magit-review-switch-branch' directly, or provide C-u argument
;;; to `org-magit-review'.
;;; 

;;; Code:
(require 'cl)

;;;
;;; Mode-specific
;;;
(defcustom org-magit-review-mode-hook nil
  "Hook run when entering the Org Magit review mode."
  :options '()
  :type 'hook)

(defvar org-magit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-r C-c") 'org-magit-review-done-for-now)
    (define-key map (kbd "C-c C-r C-s") 'org-magit-review-to-mail-compose)
    map)
  "Keymap for `org-magit-review-mode'.")

(define-derived-mode org-magit-review-mode org-mode ""
  "Org Magit Review.

\\{org-magit-review-mode-map}")

(define-key magit-mode-map (kbd "M-RET")       'org-magit-review)
(define-key magit-mode-map (kbd "C-c C-r C-b") 'org-magit-review-switch-branch)

;;;
;;; Utils
;;;
(defun chomp-head-empty-lines (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx bos (* (seq (* (any " \t")) "\n")))
			    ""
			    str))

(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
			    ""
			    str))

(defun chomp-first-line (str)
  (let ((nlposn (position ?\n str)))
    (if nlposn
	(cl-subseq str (1+ nlposn))
	"")))

(defun org-entry-last-p ()
  (= (org-entry-end-position) (point-max)))

(defun org-find-if-between (f begin end &optional jump)
  (catch 'return
    (org-map-region (lambda ()
		      (let ((x (org-element-at-point)))
			(when (funcall f x)
			  (throw 'return x))))
		    begin end)
    nil))

(defun org-find-if (f)
  (org-find-if-between f (point-min) (point-max)))

(defun org-find-with-property-value (p v)
  (org-find-if (lambda (x)
		 (equal v (org-element-property p x)))))

(defun org-entry-ensure-fresh-text-input-position ()
  (let ((lastp (org-entry-last-p)))
    (goto-char (org-entry-end-position))
    (insert-string "\n\n")
    (unless lastp
      (forward-line -2))
    (insert-string "  ")))

(defun magit-map-refs (regex fn)
  (let (ret)
    (dolist (upstream (magit-git-lines "show-ref"))
      (setq upstream (cadr (split-string upstream " ")))
      (when (and (not (string-match-p "HEAD$" upstream))
		 (string-match-p regex upstream))
	(push (funcall fn upstream) ret)))
    ret))

(defun magit-map-all-remote-refs (fn)
  (magit-map-refs "^refs/remotes/" fn))

(defun magit-map-remote-refs (remote-name fn)
  (magit-map-refs (format "^refs/remotes/%s/" remote-name) fn))

(defun magit-map-heads (fn)
  (magit-map-refs "^refs/heads/" fn))

(defun magit-item-text (item)
  (buffer-substring (magit-section-beginning hunk)
		    (magit-section-end hunk)))

;;;
;;; Pure
;;;
(defun org-magit-review-file-name (topdir branch-name)
  (expand-file-name (concat topdir "/.review-" branch-name ".org")))

;;;
;;; Generic tables
;;;
;; filename babel type map :: Filename -> BabelType
(defvar org-magit-review-filename-extension-to-babel-mode-map (make-hash-table :test 'equal))

(dolist (kv '(("c" .   "c")
	      ("org" . "org")))
  (setf (gethash (car kv) org-magit-review-filename-extension-to-babel-mode-map)
	(cdr kv)))

(defun org-magit-review-filename-to-babel-lang (filename)
  (gethash (file-name-extension filename)
	   org-magit-review-filename-extension-to-babel-mode-map))

;;;;
;;;; Global state
;;;;
;;;
;;; Review buffers
;;;
;;  review buffer map :: Topdir -> Branch -> Buffer
(defvar org-magit-review-buffers (make-hash-table :test 'equal))

(defun org-magit-review-make-buffer (topdir branch)
  (find-file (org-magit-review-file-name topdir branch)))

(defun org-magit-review-ensure-buffer (topdir branch)
  (let ((b (gethash (cons topdir branch) org-magit-review-buffers)))
    (if (and b (buffer-live-p b))
	b
	(setf (gethash (cons topdir branch) org-magit-review-buffers)
	      (org-magit-review-make-buffer topdir branch)))))

;;;
;;; Review branches
;;;
;; commit branch map :: Topdir -> Commit-Id -> Branch
(defvar org-magit-review-commit-branches (make-hash-table :test 'equal))

(defvar org-magit-default-branches (make-hash-table :test 'equal))

(defun org-magit-set-default-review-branch (topdir branch)
  (setf (gethash topdir org-magit-default-branches) branch))

(defun org-magit-review-topdir-determine-branch (topdir)
  (or (gethash topdir org-magit-default-branches)
      (org-magit-set-default-review-branch topdir (magit-read-rev "branch to review"))))

;; XXX: currently only a single branch per topdir is supported..
(defun org-magit-review-commit-determine-branch (topdir commit-id)
  (org-magit-review-topdir-determine-branch topdir))

(defun org-magit-review-commit-branch (topdir commit-id)
  (or (gethash (cons topdir commit-id) org-magit-review-commit-branches)
      (setf (gethash (cons topdir commit-id) org-magit-review-commit-branches)
	    (org-magit-review-commit-determine-branch topdir commit-id))))

(defun org-magit-clear-ephemeral-maps ()
  (cl-clrhash org-magit-review-commit-branches)
  (cl-clrhash org-magit-default-branches))

;;;
;;; Magit commit buffer parsing
;;;
(defun org-magit-review-commit-buffer-commit-id ()
  (let ((commit-id (buffer-substring-no-properties 1 41)))
    (unless (string-match-p "[0-9a-f]*" commit-id)
      (error "Buffer '%s' isn't a valid Magit commit buffer: doesn't start with a commit-id."
	     (buffer-name)))
    commit-id))

(defun org-magit-review-commit-buffer-parse ()
  "Return commit-id, author, description as multiple values."
  (save-excursion
    (list (org-magit-review-commit-buffer-commit-id)
	  (progn
	    (beginning-of-buffer)
	    (beginning-of-line 2)
	    (re-search-forward "Author: \\(.*\\)$")
	    (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	  (progn
	    (beginning-of-buffer)
	    (beginning-of-line 5)
	    (re-search-forward "    \\(.*\\)$")
	    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

;;;
;;; Intermediate layer
;;;
(defun org-magit-review-insert-commit-heading (commit-id author commit-desc)
  ;; (message "Adding new review entry for commit '%s'" commit-desc)
  (if (not (org-element-at-point))
      (progn
	(insert-string (format "#+AUTHOR: %s\n\n" author))
	(org-insert-heading))
      (progn ;; not the first commit in review
	(goto-char (point-max))
	(unless (bolp)
	  (newline))
	(org-insert-heading)
	(goto-char (org-entry-beginning-position))
	(backward-char)
	(newline)
	(forward-char)
	(end-of-line)))
  (insert-string (format "%s" commit-desc))
  (let ((entry (org-element-at-point)))
    (org-entry-put nil "commit-id" commit-id)
    entry))

(defun org-magit-review-ensure-commit-goto-heading (commit-id author commit-desc)
  (let* ((entry (or (org-find-with-property-value :COMMIT-ID commit-id)
		    (org-magit-review-insert-commit-heading commit-id author commit-desc)))
	 (heading (plist-get (second entry) :raw-value)))
    (when entry
      ;; XXX: inefficient -- this is the second time we scan the buffer
      (org-link-search heading))
    entry))

(defun org-magit-review-insert-review-entry-goto-desc-area (file line text)
  (insert-string "<s")
  (org-try-structure-completion)
  (when file
    (insert-string (format "%s  at %s:%s" (org-magit-review-filename-to-babel-lang file) file line)))
  (forward-line)
  (insert-string text)
  (forward-line 2))

(defun org-magit-review-add (topdir branch commit-desc commit-id author file line selection)
  (message "branch=%s commit-desc=%s commit-id=%s"
	   branch commit-desc commit-id)
  ;; (message "topdir=%s branch=%s commit-desc=%s commit-id=%s author='%s' file=%s line=%s sel=%s"
  ;; 	   topdir branch commit-desc commit-id author file line selection)
  (let ((buffer (org-magit-review-ensure-buffer topdir branch)))
    (magit-mode-display-buffer buffer 'org-magit-review-mode 'pop-to-buffer)
    (with-current-buffer buffer
      (org-magit-review-mode)
      (org-magit-review-ensure-commit-goto-heading commit-id author commit-desc)
      (org-entry-ensure-fresh-text-input-position)
      (org-magit-review-insert-review-entry-goto-desc-area file line selection)
      (when selection
	(org-entry-ensure-fresh-text-input-position))
      (show-all)
      (hide-sublevels 1)
      (org-show-entry))))

;;; Commands:
;;;
;;;###autoload
(defun org-magit-review (request-default-branch)
  "Initiate or continue review of the current branch, with regard
   to the currently selected commit or any of its subsections.

   This creates or resumes a branch-and-repository-bound Org mode
   buffer, and ensures the current commit has a heading for it,
   and that within the heading there's enough fresh space to
   enter the review entry.

   A review entry consists of 1) content, wrapped in a srcblock,
   and 2) user-provided comment text.  The srcblock is
   automatically pre-filled with data coming from the commit
   subsection upon which `org-magit-review' was invoked.  After
   that the point is positioned into a convenient place for the
   user to enter the comment text.

   Once the review is complete, 

   The current review branch is only asked once per repository,
   so either invoke `org-magit-review' with C-u, or explicitly
   invoke `org-magit-forget-branch-cache'."
  (interactive "P")
  (unless (string= "*magit-commit*" (buffer-name (current-buffer)))
    ;; ..if we're in the log buffer, MAGIT-VISIT-ITEM will switch us to the commit buffer.
    (magit-visit-item))
  (let ((topdir (magit-get-top-dir)))
    (unless topdir
      (user-error "Not inside a Git repository"))
    (when request-default-branch
      (org-magit-set-default-review-branch topdir (magit-read-rev "branch to review")))
    (destructuring-bind (commit-id author commit-desc) (org-magit-review-commit-buffer-parse)
      (let* ((branch (org-magit-review-commit-branch topdir commit-id)))
	(flet ((review-add (file line sel)
		 (org-magit-review-add topdir branch commit-desc commit-id author file line sel)))
	  (magit-section-action review (type info parent-info)
	    (hunk ;; the assumption we're in the commit buffer is validated
	     (let* ((hunk it)
		    (diff (magit-section-parent hunk))
		    (file (magit-section-info diff))
		    (line (magit-hunk-item-target-line hunk))
		    (sel (chomp-head-empty-lines
			  (chomp-end (if (use-region-p)
					 (buffer-substring (region-beginning) (region-end))
					 (chomp-first-line (magit-item-text hunk)))))))
	       ;; (message "topdir=%s branch=%s commit-desc=%s commit-id=%s author='%s' file=%s line=%s sel=%s"
	       ;; 	      topdir branch commit-desc commit-id author file line sel)
	       (review-add file line sel)))
	    (commit-buf
	     (review-add nil nil nil))
	    (diff
	     (review-add nil nil (format "  file:  %s" (magit-section-info it))))
	    ((diffstat diffstats headers message)
	     (review-add nil nil
			 (chomp-end
			  (buffer-substring (magit-section-beginning it)
					    (magit-section-end it)))))
	    (*
	     (message "can't review sections of type '%s'" type))))))))

(defun org-magit-review-done-for-now ()
  (interactive)
  (let ((require-final-newline nil))
    (save-buffer))
  (magit-mode-quit-window))

;;;###autoload
(defun org-magit-review-switch-branch ()
  (interactive)
  (let ((topdir (magit-get-top-dir)))
    (unless topdir
      (user-error "Not inside a Git repository"))
    (org-magit-set-default-review-branch topdir (magit-read-rev "branch to review"))))

(defun org-magit-forget-branch-cache ()
  (interactive)
  (org-magit-clear-ephemeral-maps))

(defun org-magit-review-to-mail-compose (request-default-branch)
  (interactive "P")
  (let ((topdir (magit-get-top-dir)))
    (unless topdir
      (user-error "Not inside a Git repository"))
    (when request-default-branch
      (org-magit-set-default-review-branch topdir (magit-read-rev "branch to review")))
    (let* ((branch   (org-magit-review-topdir-determine-branch topdir))
	   (buffer   (org-magit-review-ensure-buffer topdir branch))
	   (content  (with-current-buffer buffer
		       (buffer-substring (point-min) (point-max))))
	   to
	   (washed   (with-temp-buffer
		       (insert-string content)
		       (setf to (first (plist-get (org-export--get-inbuffer-options) :author)))
		       (goto-char (point-min))
		       (kill-line 2)
		       (org-delete-property-globally "COMMIT-ID")
		       (org-map-region
			(lambda ()
			  (forward-line)
			  (org-remove-empty-drawer-at "PROPERTIES" (point)))
			(point-min) (point-max))
		       (buffer-substring (point-min) (point-max)))))
      (compose-mail to (format "Review of branch \"%s\"" branch))
      (insert-string (format "Hi!\n\nHere's some comments on the \"%s\" branch:\n\n"
			     branch))
      (insert-string washed))))

;;; org-magit-review.el ends here
