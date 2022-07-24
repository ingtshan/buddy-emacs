;;; core-lib.el --- Description -*- lexical-binding: t; -*-
(require 'cl-lib)

;;; Helpers

;;;###autoload
(defun doom-rpartial (fn &rest args)
  "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(provide 'core-lib)
;;; core-lib.el ends here
