;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Commentary:

;;; Code

;;; borg
;; use git submodule to maintain my package
(eval-and-compile 
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;;; epkag
;; describe dependence of a given package
(require 'epkg)
(setq epkg-repository (expand-file-name "etc/epkgs" user-emacs-directory))
(setq epkg-database-connector 'sqlite-builtin)

;;; my better default
;;
(require 'better-defaults)
(require 'mac-command)  ; use some mac shortcut

;;; tips about loading order
;; file: early-init.el
;; file: init.el
;; `after-init-hook'
;; `emacs-startup-hook'

;;; evil
;; bring vim to emacs
(add-hook 'after-init-hook #'(lambda () (require 'evil) (evil-mode 1)))

;;; which-key
;; make my key-binding clear
(with-eval-after-load 'evil
  (diminish 'which-key-mode)
  (which-key-setup-side-window-bottom))

;;; vertico, orderless
;; completion UI
(add-hook 'after-init-hook 'vertico-mode)
(with-eval-after-load 'vertico
  (require 'orderless)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
