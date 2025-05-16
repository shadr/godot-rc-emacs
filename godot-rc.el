;;; godot-rc.el --- Control godot editor without leaving Emacs -*- lexical-binding: t; -*-
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

;; TODO: feat: add marking capabilities, mark multiple nodes and reparent them to selected one or delete them
;; TODO: fix: when holding move-node-up key due to latency between request, refreshing buffer and keeping cursor on a node, it can move wrong node
;; TODO: fix: deduplicate refresh-buffer calls in node move functions
;; TODO: add safe guards to all functions
;; TODO: feat: disable .tscn file hook with a variable
;; TODO: feat: save node as scene
;; TODO: feat: show editable nodes under scene instance

(require 'gdscript-mode)
(require 's)

(require 'godot-rc-core)
(require 'godot-rc-tscn-mode)

(defun godot-rc--on-scene-changed (path)
  (godot-rc-tscn-refresh-buffer path))

(godot-rc-add-notification-handler "scene-changed" #'godot-rc--on-scene-changed)

(defun godot-rc-reload-resource (path)
  (godot-rc-request "resource-reload" path))

;;;###autoload
(defun godot-rc-gdscript-insert-node-path-multiple ()
  (interactive)
  (let ((tscn-path (gdscript-project--current-buffer-scene)))
    (if (eq tscn-path nil)
        (message "Current script is not associated with any scene")
      (godot-rc-request-callback "get-nodes-for-onready" tscn-path #'godot-rc--read-node-name-and-insert))))

(defun godot-rc--gdscript-insert-onready-variable (node)
  (let* ((nodepath (car node))
         (node-properties (cdr node))
         (name (cdr (assoc 'name node-properties)))
         (type (cdr (assoc 'type node-properties)))
         (text (concat "@onready var " (s-snake-case name) (if (not (eq type nil)) (concat ": " type " = $") " := $")  nodepath)))
    (insert text)))

(defun godot-rc--read-node-name-and-insert (result)
  (let ((matches (godot-rc--onready-list-to-completion-list result)))
    (if (not (eq matches nil))
        (let ((node-array (condition-case nil (completing-read-multiple "Nodes: " matches) (quit nil))))
          (dolist (node node-array)
            (godot-rc--gdscript-insert-onready-variable (assoc node matches))
            (newline-and-indent))))))

(defun godot-rc--onready-list-to-completion-list (nodes)
  (mapcar (lambda (node) (cons (string-remove-prefix "./" (gethash "path" node)) `((name . ,(gethash "name" node)) (type . ,(gethash "type" node))))) nodes))

;;;###autoload
(defun godot-rc-create-new-scene ()
  (interactive)
  (let ((path (read-file-name "Scene path: ")))
    (if (not (string-suffix-p ".tscn" path)) (setq path (concat path ".tscn")))
    (if (file-exists-p path)
        (error "File already exists")
      (godot-rc-request-callback
       "get-node-classes"
       nil
       (lambda (result) (godot-rc--create-new-scene-with-list-and-path result path))))))

(defun godot-rc--create-new-scene-with-list-and-path (class-list path)
  (let* ((base_class (condition-case nil (completing-read "Base: " class-list) (quit nil)))
         (name (read-string "Name: " base_class)))
    (godot-rc-request "scene-new" `(:path ,path :base ,base_class :name ,name))))

;;;###autoload
(defun godot-rc-edit-current-scene ()
  (interactive)
  (let ((tscn-path (gdscript-project--current-buffer-scene)))
    (godot-rc-edit-scene tscn-path)))

(defun godot-rc--find-file-hook ()
  (when (string-suffix-p ".tscn" buffer-file-name)
    (godot-rc-edit-scene buffer-file-name)
    (kill-buffer (current-buffer))))

(add-hook 'find-file-hook #'godot-rc--find-file-hook)

(provide 'godot-rc)
;;; godot-rc.el ends here
