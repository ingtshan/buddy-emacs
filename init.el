;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Commentary:

;;; Code

;;; epkag
;; describe dependence of a given package
(require 'epkg)
(setq epkg-repository (expand-file-name "etc/epkgs" user-emacs-directory))
(setq epkg-database-connector 'sqlite-builtin)

;;; my better default
(require 'mac-command)  ; use some mac shortcut
(progn
  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  ;; macosx
  (when (eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser))
  ;; windows-nt
  (when (eq system-type 'windows-nt)
    (setq browse-url-browser-function 'browse-url-default-windows-browser))
  ;; (setq browse-url-browser-function 'browse-url-chromium)

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)

  (set-default-coding-systems 'utf-8)

  ;; (global-set-key (kbd "M-/") 'hippie-expand)
  ;; (global-set-key (kbd "C-x C-b") 'ibuffer)
  ;; (global-set-key (kbd "M-z") 'zap-up-to-char)
  ;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  ;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  ;; (global-set-key (kbd "C-M-s") 'isearch-forward)
  ;; (global-set-key (kbd "C-M-r") 'isearch-backward)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (savehist-mode 1)
  (setq use-short-answers t
        confirm-kill-emacs 'y-or-n-p
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        backup-by-copying t
        create-lockfiles nil
        make-backup-files nil
        ediff-window-setup-function 'ediff-setup-windows-plain
        custom-file (expand-file-name "custom.el" user-emacs-directory))

  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups"))))))

;;; ui
;; use some code from doom-emacs
(setq
 doom-font (font-spec :family "Fira Code" :size 17 :weight 'regular)
 doom-variable-pitch-font (font-spec :family "Sarasa Mono SC Nerd" :size 17))

(with-eval-after-load 'vertico
  (setq doom-serif-font doom-font)
  (require 'core-lib)
  (require 'core-ui)
  (doom-init-fonts-h)
  (load-theme 'doom-challenger-deep t))

(eval-after-load 'doom-themes
  (progn
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (require 'doom-themes-ext-visual-bell)
    (doom-themes-visual-bell-config)
    (require 'doom-themes-ext-org)
    (doom-themes-org-config)))

;;; tips about loading order
;; file: early-init.el
;; file: init.el
;; `after-init-hook'
;; `emacs-startup-hook'

;;; evil
;; bring vim to emacs
(add-hook 'after-init-hook
          #'(lambda ()
              (setq evil-want-keybinding nil)
              (require 'evil)
              (evil-mode 1)))

;;; which-key
;; make my key-binding clear
(with-eval-after-load 'evil
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;;; vertico, orderless
;; completion UI
(add-hook 'after-init-hook 'vertico-mode)
(with-eval-after-load 'vertico
  (require 'orderless)

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; consult
;; consult way of selecting
(global-set-key (kbd "C-s") 'consult-line)

(defun +consult/search-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;; magit
;; magic git
(with-eval-after-load 'magit
  (evil-collection-init))

;;; tips of evil map
;; `evil-motion-state-map' Evil-specific present states that not editing
;; `evil-normal-state-map'

;;; load private
(let ((config-file
       (expand-file-name
        "etc/private/config.el"
        user-emacs-directory)))

  (when (file-exists-p config-file)
    (load-file config-file)))

;;; buddy-key
;; my key-binding
(with-eval-after-load 'which-key
  (require 'buddy-key)

  ;;; leader key
  (buddy-key-def-preset :leader
                        :keymaps 'evil-motion-state-map
                        :prefix "SPC")
  (buddy-def-key :leader nil)

  ;;; f file
  (buddy-def-key
   :leader "f" '("file" . nil)
   :leader "ff" '("Find file" . find-file)
   :leader "fs" '("Save file" . save-buffer)
   )

  ;;; h help
  (buddy-def-key
   :leader "h" '("help" . nil)
   :leader "hp" '("package" . nil)
   :leader "hpp" '("Describe packge" . epkg-describe-package)
   :leader "hpi" '("Install package" . borg-assimilate)
   :leader "hpr" '("Remove package" . borg-remove)
   )

  ;;; s search
  (buddy-def-key
   :leader "s" '("search" . nil)
   :leader "ss" 'consult-line
   :leader "sS" '+consult/search-symbol-at-point
   :leader "si" 'consult-imenu)

  ;;; g git
  (buddy-def-key
   :leader "g" '("git" . nil)
   :leader "gg" 'magit-status)

  ;;; o open
  (buddy-def-key
   :leader "o" '("open" . nil)
   :leader "oc" '("configuration" . nil)
   :leader "occ" '("Configuration"
                   . (lambda ()
                                      (interactive)
                                      (find-file
                                      (expand-file-name "init.el" user-emacs-directory))))
   :leader "ocn" '("Note workflow"
                   . (lambda ()
                                      (interactive)
                                      (find-file
                                      (expand-file-name "lib/buddy-note/buddy-note.el" user-emacs-directory))))

   :leader "ocp" '("Private configuration"
                   . (lambda ()
                                      (interactive)
                                      (find-file
                                      (expand-file-name "etc/private/config.el" user-emacs-directory)))))

  ;; (all-the-icons-install-fonts 'yes)
  ;;; b buffer
  (buddy-def-key
   :leader "b" '("buffer" . nil)
   :leader "bB" 'consult-buffer)

  ;;; i insert
  (buddy-def-key
   :leader "i" '("insert" . nil)
   :leader "ig" '("git message" . nil)
   :leader "igb" 'borg-insert-update-message)

  ;;; n note
  (buddy-def-key
   :leader "n" '("notes" . nil)
   :leader "ng" '("Google tasks" . org-gtasks)
   :ledaer "nv" 'consult-notes)

  );; my key-biding ends here

(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
