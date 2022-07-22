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

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
