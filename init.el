;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:
;;; init.el ends here
