;;; buddy-note.el --- A note-taking workflow for Emacs  -*- lexical-binding: t -*-
(require 'org)
(require 'org-element)
(require 'denote)
(require 'consult)
(require 'consult-notes)

;; (require 'org-zettelkasten)
;; (add-hook 'org-mode-hook #'org-zettelkasten-mode)
;; (require 'zettelkasten)
;; (zettelkasten-mode t)

(setq denote-org-front-matter
      ":PROPERTIES:
:ID:          %4$s
:END:
#+title:      %1$s
#+date:       %2$s
#+filetags:   %3$s

* %1$s")

(defvar buddy-note--all-sources nil
  "List of all sources for use with consult-notes.
This is an internal variable. The user will typically only interact with `consult-notes-sources'.")

(defvar buddy-note--thought-type-options nil
  "List of key word to use in note file name")

(defun buddy-note--outline-candidates ()
  "Return alist of outline headings."
  (consult--forbid-minibuffer)
  (let* ((heading-regexp
          (concat "^\\(?:"
                  ;; default definition from outline.el
                  (or (bound-and-true-p outline-regexp) "[*\^L]+")
                  "\\)"))
         (candidates))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (re-search-forward heading-regexp nil t))
        (goto-char (match-beginning 0))
        (push
         (consult--buffer-substring
          (line-beginning-position)
          (line-end-position)
          'fontify)
         candidates)
        (unless (eobp) (forward-char 1))))
    (unless candidates
      (user-error "No headings"))
    (nreverse candidates)))

(defun buddy-note--make-source (name char dir)
  "Return a notes source list suitable for `consult--multi'.
NAME is the source name, CHAR is the narrowing character,
and DIR is the directory to find notes."
  (let ((idir (propertize (file-name-as-directory dir) 'invisible t)))
    `(:name     ,(propertize name 'face 'consult-notes-sep)
                :narrow   ,char
                :category ,consult-notes-category
                :face     consult-file
                :annotate ,(apply-partially consult-notes-annotate-note-function name)
                :items    ,(lambda () (mapcar (lambda (f) (concat idir f))
				              ;; filter files that glob *.*
				              (directory-files dir nil "[^.].*[.].+"))))))

(defun buddy-note--make-all-sources ()
  "Add generated `consult--multi' sources to list of all sources."
  (let ((sources (mapcar #'(lambda (s) (apply 'buddy-note--make-source s))
		         consult-notes-sources)))
    (dolist (i sources)
      (add-to-list 'buddy-note--all-sources i))))

(defun denote-retrieve--read-file-prompt ()
  "Find a file in a notes directory with consult-multi, or from SOURCES."
  (interactive)
  (buddy-note--make-all-sources)
  (let ((selected (consult--multi buddy-note--all-sources
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt "Notes: "
                                  :history 'consult-notes-history)))
    (expand-file-name (car selected))))

(defun buddy-note--name-prompt ()
  "Find a file in a notes directory with consult-multi, or from SOURCES."
  (interactive)
  (buddy-note--make-all-sources)
  (let ((selected (consult--multi buddy-note--all-sources
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt "Viewing notes for name(Select of C-enter): "
                                  :history 'consult-notes-history)))
    (if selected
        (car (last (split-string
                    (car (split-string
                          (file-name-base (car selected)) "__")) "--"))))))

(defun buddy-note--file-path-prompt ()
  "Find a file in a notes directory with consult-multi, or from SOURCES."
  (interactive)
  (buddy-note--make-all-sources)
  (let* ((selected (consult--multi buddy-note--all-sources
                                   :require-match
                                   (confirm-nonexistent-file-or-buffer)
                                   :prompt "Select a presenting note: "
                                   :history 'consult-notes-history))
         (file-path (expand-file-name (car selected))))
    (if (file-exists-p file-path) file-path nil)))

(defun buddy-note--thought-type-prompt ()
  "Select a key word form `buddy-note--thought-type-options'"
  (let ((choice (completing-read "Thought: " buddy-note--thought-type-options)))
    (car (split-string choice " "))))

(defun buddy-note--select-with-heading ()
  ""
  (let ((path (buddy-note--file-path-prompt)))
    (if path
        (progn
          (with-current-buffer (find-file-noselect path)
            (setq buddy-note--title
                  (completing-read
                   "input title: "
                   (consult--with-increased-gc
                    (buddy-note--outline-candidates)))))))
    path))

(defun buddy-note--thought-current-buffer ()
  ""
  (with-current-buffer (current-buffer)
    (completing-read
     "input title: "
     (consult--with-increased-gc
      (buddy-note--outline-candidates)))))

(org-insert-heading-respect-content)

;;;###autoload
(defun buddy-note-insert-thought(thought)
  ""
  (interactive
   (list (buddy-note--thought-current-buffer)))
  (if (eq major-mode 'org-mode)
        (org-insert-heading-respect-content))
  (insert thought))

;;;###autoload
(defun buddy-note-subdirectory (directory thought-type title keywords)
  "Like `denote' but ask for DIRECTORY to put the note in.

The DIRECTORY is either the variable `denote-directory' or a
subdirectory of it.  The TITLE and KEYWORDS are the same as for
the `denote' command.

Denote does not create subdirectories."
  (interactive
   (list
    (denote--subdirs-prompt)
    (list (buddy-note--thought-type-prompt))
    (buddy-note--name-prompt)
    (denote--keywords-prompt)))
  (denote title
          (if (stringp keywords)
              (add-to-list 'thought-type keywords)
            (append thought-type keywords))
          nil
          directory))

;;; org-capture

(provide 'buddy-note)
