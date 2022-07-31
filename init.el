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
        ediff-window-setup-function 'ediff-setup-windows-plain
        system-time-locale "C" ; formatting time values in English
        ))

;;; ui
;; use some code from doom-emacs
(setq
 doom-font (font-spec :family "Fira Code" :size 17 :weight 'regular)
 doom-variable-pitch-font (font-spec :family "Sarasa Mono SC Nerd" :size 17)
 vertico-posframe-font "Sarasa Mono SC Nerd 16")

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

;;; vertico-posframe
;; bring children frame to vertico
;; (defface vertico-posframe-border
;;   '((t (:inherit default :background "gray50")))
;;   "Face used by the vertico-posframe's border."
;;   :group 'vertico-posframe)

;; (defun buddy--vertico-stop-using-minibuffer ()
;;   "kill the minibuffer"
;;   (when (and (>= (recursion-depth) 1)
;;              (active-minibuffer-window))
;;     (abort-recursive-edit)))
;; ;; kill minibuffer while unfocus
;; (add-hook 'mouse-leave-buffer-hook 'buddy--vertico-stop-using-minibuffer)
;; enabel after vertico
;; (add-hook 'vertico-mode-hook
;;           #'(lambda ()
;;               (require 'vertico-posframe)
;;               (vertico-posframe-mode 1)))

;;; marginalia
;; more describtion of candidate
(with-eval-after-load 'vertico
  (require 'marginalia)
  (require 'all-the-icons-completion)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (add-hook 'vertico-mode-hook #'marginalia-mode))

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

;;; load private
(let ((config-file
       (expand-file-name
        "etc/private/config.el"
        user-emacs-directory)))

  (when (file-exists-p config-file)
    (load-file config-file)))

;;; tips of evil map
;; `evil-motion-state-map' Evil-specific present states that not editing
;; `evil-normal-state-map'

;;; buddy-key
;; my key-binding
(with-eval-after-load 'which-key
  (require 'buddy-key)

  ;;; extend evil
  (buddy-def-key
   :keymaps 'evil-normal-state-map
   "U" 'undo-tree-visualize)

  ;;; fix evil
  (buddy-def-key
   :keymaps 'evil-motion-state-map
   "C-u" 'evil-scroll-up)

  ;;; leader key
  (buddy-key-def-preset :leader
    :keymaps 'evil-motion-state-map
    :prefix "SPC")
  (buddy-def-key
   :leader nil)

  (buddy-def-key
   :leader "x" 'scratch)

  ;;; f file
  (buddy-def-key
   :leader "f" '("file" . nil)
   :leader "ff" '("Find file" . find-file)
   :leader "fs" '("Save file" . save-buffer)
   )

  ;;; m my
  (buddy-def-key
   :leader "m" '("my stuff" . nil)
   :leader "mc" '("configuration" . nil))

  ;;; mc my configuration
  (buddy-def-key
   :leader "mcc" '("Configuration"
                   . (lambda ()
                       (interactive)
                       (find-file
                        (expand-file-name
                         "init.el"
                         user-emacs-directory))))

   :leader "mcp" '("Private configuration"
                   . (lambda ()
                       (interactive)
                       (find-file
                        (expand-file-name
                         "etc/private/config.el"
                         user-emacs-directory))))

   :leader "mcn" '("Note workflow"
                   . (lambda ()
                       (interactive)
                       (find-file
                        (expand-file-name
                         "lib/buddy-emacs/feature/buddy-note.el"
                         user-emacs-directory)))))

  ;;; h help
  (buddy-def-key
   :leader "h" '("help" . nil)
   :leader "hp" '("package" . nil)
   )

  ;;; hp help package
  (buddy-def-key
   :leader "hpp" '("Describe packge" . epkg-describe-package)
   :leader "hpi" '("Install package" . borg-assimilate)
   :leader "hpr" '("Remove package" . borg-remove))

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

  ;; (all-the-icons-install-fonts 'yes)
  ;;; b buffer
  (buddy-def-key
   :leader "b" '("buffer" . nil)
   :leader "bB" 'consult-buffer)

  ;;; i insert
  (buddy-def-key
   :leader "i" '("insert" . nil)
   :leader "ig" '("git message" . nil)
   :leader "in" 'buddy-note-insert-thought)

  ;;; ig insert git message
  (buddy-def-key
   :leader "igb" 'borg-insert-update-message)

  ;;; n note
  (buddy-def-key
   :leader "n" '("notes" . nil)
   :leader "ng" '("Google tasks" . org-gtasks)
   :ledaer "nv" 'consult-notes
   :leader "nn" 'buddy-note-subdirectory
   :leader "nc" 'org-capture)

  ;;; k kill
  (buddy-def-key
   :leader "k" '("kill" . nil)
   :leader "kb" 'kill-buffer
   :leader "kB" 'kill-current-buffer)

  );; my key-biding ends here

;;; useful tool
(defvar default-proxy "127.0.0.1:1080")
(defvar socks-server)
(defvar socks-noproxy)
;; Network Proxy
(defun ult/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" default-proxy)
    (message "No HTTP proxy")))

(defun ult/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,default-proxy)
          ("https" . ,default-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (ult/proxy-http-show))

(defun ult/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (ult/proxy-http-show))

(defun ult/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (ult/proxy-http-disable)
    (ult/proxy-http-enable)))

(defun ult/proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)                ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun ult/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (let* ((proxy (split-string default-proxy "\\s-*:\\s-*"))
         (addr (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq url-gateway-method 'socks
          socks-noproxy '("localhost")
          socks-server `("Default server" ,addr ,port 5)))
  (ult/proxy-socks-show))

(defun ult/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (ult/proxy-socks-show))

(defun ult/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (ult/proxy-socks-disable)
    (ult/proxy-socks-enable)))

;;; tips about loading order
;; file: early-init.el
;; file: init.el
;; `after-init-hook'
;; `emacs-startup-hook'
;;
;; load sequence
;; load-path
;; borg->no-littering->epkg->my better default->useful tool
;; after-init->evil->which-key->buddy-key
;; after-init->vertico->orderless
;;                    ->ui(doom-themes)
;;
;; autoload: consult, magit

(add-hook 'after-init-hook
          #'(lambda ()
              ;;; evil
              ;; bring vim to emacs
              (setq evil-want-keybinding nil)
              (require 'evil)
              (evil-mode 1)

              ;;;
              ))

(add-hook 'emacs-startup-hook
          #'(lambda ()
              ;;; word-wrap
              ;; show long line better
              (setq word-wrap-by-category t)
              (global-visual-line-mode)

              ;;; sis
              ;; less input method manual switch
              (sis-ism-lazyman-config
               ;; English input source may be: "ABC", "US" or another one.
               "com.apple.keylayout.ABC"
               ;; "com.apple.keylayout.US"

               ;; Other language input source: "rime", "sogou" or another one.
               ;; "im.rime.inputmethod.Squirrel.Rime"
               "com.apple.inputmethod.SCIM.ITABC")
              (sis-global-respect-mode t)
              ;;; undo
              (global-undo-tree-mode)
              ;;; noting
              (require 'buddy-note)
              ;;;
              ))

(provide 'init)
;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
