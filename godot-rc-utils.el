;;; godot-rc-utils.el --- Utility functions for godot-rc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 shadr
;;
;; Author: shadr <shadr@nixos>
;; Homepage: https://github.com/shadr/godot-rc-emacs
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'magit-section)

(defun godot-rc--get-section-from-id (value &optional root)
  (setq root (or root magit-root-section))
  (if (equal value (oref root value))
      root
    (cl-loop for child in (oref root children)
             thereis (godot-rc--get-section-from-id value child))))

(defun godot-rc--get-section-start-position (section)
  (let ((position (oref section start)))
    (while (setq section (oref section parent)) (setq position (+ position (oref section start))))
    position))

(defun godot-rc--magit-section-index (section)
  (cl-position section (oref (oref section parent) children)))

(defun godot-rc--magit-section-previous-sibling (section)
  (let ((position (godot-rc--magit-section-index section)))
    (when (> position 0)
      (nth (- position 1) (oref (oref section parent) children)))))

(cl-defun godot-rc--magit-section-depth (&optional section)
  (let ((depth 0))
    (while (setq section (oref section parent)) (cl-incf depth))
    depth))

(provide 'godot-rc-utils)
;;; godot-rc-utils.el ends here
