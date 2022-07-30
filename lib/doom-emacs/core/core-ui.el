;;; core-ui.el -*- lexical-binding: t; -*-

;;
;;; Variables
(defvar doom-font nil
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq doom-font (font-spec :family \"Fira Mono\" :size 12))
  (setq doom-font \"Terminus (TTF):pixelsize=12:antialias=off\")
  (setq doom-font \"Fira Code-14\")")

(defvar doom-variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

An omitted font size means to inherit `doom-font''s size.")

(defvar doom-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

An omitted font size means to inherit `doom-font''s size.")

(defvar doom-unicode-font nil
  "Fallback font for Unicode glyphs.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

The defaults on macOS and Linux are Apple Color Emoji and Symbola, respectively.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(defvar doom-emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.")

(defvar doom-symbol-fallback-font-families
  '("Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.")

;;
;;; Theme & font

(defun doom--make-font-specs (face font &optional base-specs)
  (let* ((base-specs (cadr (assq 'user (get face 'theme-face))))
         (base-specs (or base-specs '((t nil))))
         (attrs '(:family :foundry :slant :weight :height :width))
         (new-specs nil))
    (dolist (spec base-specs)
      ;; Each SPEC has the form (DISPLAY ATTRIBUTE-PLIST)
      (let ((display (car spec))
            (plist   (copy-tree (nth 1 spec))))
        ;; Alter only DISPLAY conditions matching this frame.
        (when (or (memq display '(t default))
                  (face-spec-set-match-display display this-frame))
          (dolist (attr attrs)
            (setq plist (plist-put plist attr (face-attribute face attr)))))
        (push (list display plist) new-specs)))
    (nreverse new-specs)))

;;;###autoload
(defun doom-init-fonts-h (&optional reload)
  "Loads `doom-font'."
  (dolist (map `((default . ,doom-font)
                 (fixed-pitch . ,doom-font)
                 (fixed-pitch-serif . ,doom-serif-font)
                 (variable-pitch . ,doom-variable-pitch-font)))
    (when-let* ((face (car map))
                (font (cdr map)))
      (dolist (frame (frame-list))
        (when (display-multi-font-p frame)
          (set-face-attribute face frame
                              :width 'normal :weight 'normal
                              :slant 'normal :font font)))
      (let ((new-specs (doom--make-font-specs face font)))
        ;; Don't save to `customized-face' so it's omitted from `custom-file'
        ;;(put face 'customized-face new-specs)
        (custom-push-theme 'theme-face face 'user 'set new-specs)
        (put face 'face-modified nil))))
  (when (fboundp 'set-fontset-font)
    (let ((fn (doom-rpartial #'member (font-family-list))))
      (when-let (font (cl-find-if fn doom-symbol-fallback-font-families))
        (set-fontset-font t 'symbol font))
      (when-let (font (cl-find-if fn doom-emoji-fallback-font-families))
        (set-fontset-font t 'unicode font))
      (when doom-unicode-font
        (set-fontset-font t 'unicode doom-unicode-font))))
  ;; Users should inject their own font logic in `after-setting-font-hook'
  (run-hooks 'after-setting-font-hook))

(provide 'core-ui)
;;; core-ui.el ends here
