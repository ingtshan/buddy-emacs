;;; early-init.el ---                -*- lexical-binding: t, no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5) ;;defer gc futher back

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;;; load
(let ((default-directory (file-name-as-directory (expand-file-name "lib" user-emacs-directory))))
  (push default-directory load-path)
  (normal-top-level-add-subdirs-to-load-path))

;;; borg
;; use git submodule to maintain my package
(eval-and-compile
  (require 'borg)
  (setq borg-rewrite-urls-alist
      '(("git@github.com:" . "https://github.com/")
        ("git@gitlab.com:" . "https://gitlab.com/")))
  (borg-initialize))

;;; no-littering
;; keeping ~/.emacs.d clean
(setq make-backup-files nil
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      no-littering-etc-directory
      (expand-file-name "etc/local/config" user-emacs-directory)
      no-littering-var-directory
      (expand-file-name "etc/local/var" user-emacs-directory))

(require 'no-littering)

;;; auto-complie
;; (setq load-prefer-newer t)
;; (add-to-list 'load-path (expand-file-name "lib/compat" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "lib/packed" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "lib/auto-compile" user-emacs-directory))
;; (require 'auto-compile)
;; (auto-compile-on-load-mode)
;; (auto-compile-on-save-mode)

;; (setq load-prefer-newer t)
;; (setq package-enable-at-startup nil)

;; (with-eval-after-load 'package
;;   (add-to-list 'package-archives
;;                (cons "melpa" "https://melpa.org/packages/")
;;                t))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
;; (setq frame-inhibit-implied-resize t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
;; (setq tool-bar-mode nil)
;; (setq menu-bar-mode nil)
;; (set-scroll-bar-mode nil)

;;; Font setting
;;;
;; (defvar font-list
;;   (cond
;;    ((eq system-type 'darwin)
;;     '(("SF Mono" . 13) ("Monaco" . 13) ("Menlo" . 13)))
;;    ((eq system-type 'windows-nt)
;;     '(("Cascadia Mono" . 11) ("SF Mono" . 11) ("Consolas" . 12)))
;;    (t
;;     '(("SF Mono" . 11) ("Consolas" . 12) ("Cascadia Mono" . 11))))
;;   "List of fonts and sizes.  The first one available will be used.")

;; Set default font before frame creation to make sure the first frame have the correct size
;; (add-to-list 'default-frame-alist (cons 'font (format "%s-%d" (caar font-list) (cdar font-list))))

;; (defun font-installed-p (font)
;;   "Check if the FONT is available."
;;   (find-font (font-spec :name font)))

;; (defun change-font ()
;;   "Change the font of frame from an available `font-list'."
;;   (interactive)
;;   (let* (available-fonts font-name font-size font-set)
;;     (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
;;       (when (font-installed-p (car font))
;;         (push font available-fonts)))
;;     (if (not available-fonts)
;;         (message "No fonts from the chosen set are available")
;;       (if (called-interactively-p 'interactive)
;;           (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t)
;;                                        available-fonts)))
;;             (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
;;         (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
;;       (setq font-set (format "%s-%d" font-name font-size))
;;       (set-frame-font font-set nil t)
;;       (add-to-list 'default-frame-alist (cons 'font font-set)))))

;; (defun change-unicode-font ()
;;   "Setup the Unicode font."
;;   (when (display-graphic-p)
;;     (cl-loop for font in '("Microsoft Yahei" "PingFang SC" "Noto Sans Mono CJK SC")
;;              when (font-installed-p font)
;;              return (dolist (charset '(kana han hangul cjk-misc bopomofo))
;;                       (set-fontset-font t charset font)))
;;     (cl-loop for font in '("Segoe UI Emoji" "Apple Color Emoji" "Noto Color Emoji")
;;              when (font-installed-p font)
;;              return (set-fontset-font t 'unicode font nil 'append))
;;     (dolist (font '("HanaMinA" "HanaMinB"))
;;       (when (font-installed-p font)
;;         (set-fontset-font t 'unicode font nil 'append)))))

;; Run after startup
;; (dolist (fn '(change-font change-unicode-font))
;;   (add-hook 'after-init-hook fn))

(provide 'early-init)
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
