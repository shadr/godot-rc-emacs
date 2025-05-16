;;; godot-rc-tscn-mode.el --- Major mode for manipulating scene trees -*- lexical-binding: t; -*-
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
(require 'evil)
(require 'projectile)
(require 'f)

(require 'godot-rc-core)
(require 'godot-rc-utils)

(defvar godot-rc--tscn-scene-path nil)
(defconst godot-rc--base-dir (file-name-directory (or load-file-name buffer-file-name)))

;;;###autoload
(define-derived-mode tscn-mode magit-section-mode "TSCN"
  (define-key tscn-mode-map [remap save-buffer] #'godot-rc-tscn-save-scene)
  (evil-define-key 'normal tscn-mode-map (kbd "o") #'godot-rc-open-scene)
  (evil-define-key 'normal tscn-mode-map (kbd "r") #'godot-rc-tscn-rename-node)
  (evil-define-key 'normal tscn-mode-map (kbd "c") #'godot-rc-tscn-change-node-type)
  (evil-define-key 'normal tscn-mode-map (kbd "R") #'godot-rc-tscn-refresh-buffer)
  (evil-define-key 'normal tscn-mode-map (kbd "D") #'godot-rc-tscn-delete-node)
  (evil-define-key 'normal tscn-mode-map (kbd "d") #'godot-rc-tscn-duplicate-node)
  (evil-define-key 'normal tscn-mode-map (kbd "a") #'godot-rc-tscn-add-sibling-node)
  (evil-define-key 'normal tscn-mode-map (kbd "A") #'godot-rc-tscn-add-child-node)
  (evil-define-key 'normal tscn-mode-map (kbd "C-j") #'godot-rc-tscn-move-node-down)
  (evil-define-key 'normal tscn-mode-map (kbd "C-k") #'godot-rc-tscn-move-node-up)
  (evil-define-key 'normal tscn-mode-map (kbd "C-l") #'godot-rc-tscn-move-node-in)
  (evil-define-key 'normal tscn-mode-map (kbd "C-h") #'godot-rc-tscn-move-node-out)
  (evil-define-key 'normal tscn-mode-map (kbd "RET") #'godot-rc--something))

(defun godot-rc--something ()
  (interactive)
  (message (godot-rc--get-section-from-id (magit-section-ident-value (magit-current-section)))))

(defun godot-rc-tscn-delete-node ()
  (interactive)
  (let ((node-id (magit-section-ident-value (magit-current-section)))
        (confirm (y-or-n-p "Delete node and its children?")))
    (when confirm (godot-rc-request "node-remove" node-id))))

(defun godot-rc-tscn-add-node (parent-id index)
  (godot-rc-request-callback
   "get-node-classes"
   nil
   (lambda (result)
     (let ((type (completing-read "Node type: " result)))
       (godot-rc-request "node-add" `((parent_id . ,parent-id) (index . ,index) (type . ,type)))))))

(defun godot-rc-tscn-add-sibling-node ()
  (interactive)
  (let* ((section (magit-current-section))
         (parent-id (magit-section-ident-value (oref section parent)))
         (current-index (godot-rc--magit-section-index section)))
    (godot-rc-tscn-add-node parent-id (+ current-index 1))))

(defun godot-rc-tscn-add-child-node ()
  (interactive)
  (let* ((section (magit-current-section))
         (parent-id (magit-section-ident-value section))
         (index (length (oref section children))))
    (godot-rc-tscn-add-node parent-id (+ index 1))))

(defun godot-rc-tscn-rename-node ()
  (interactive)
  (let ((current-section (magit-section-ident-value (magit-current-section)))
        (new-name (read-string "New name: ")))
    (godot-rc-request "node-rename" `((id . ,current-section) (name . ,new-name)))))

(defun godot-rc-tscn-change-node-type ()
  (interactive)
  (let ((current-section (magit-section-ident-value (magit-current-section))))
    (godot-rc-request-callback
     "get-node-classes"
     nil
     (lambda (result)
       (let ((new-type (completing-read "New type: " result)))
         (godot-rc-request "node-change-type" `((node_id . ,current-section) (new_type . ,new-type))))))))

(defun godot-rc-tscn-move-node (offset)
  (let* ((section (magit-current-section))
         (max-index (- (length (oref (oref section parent) children)) 1))
         (current-index (godot-rc--magit-section-index section))
         (node-id (magit-section-ident-value section))
         (parent-id (magit-section-ident-value (oref section parent)))
         (index (max (min (+ current-index offset) max-index) 0)))
    (unless (eq index current-index)
      (godot-rc-request-callback
       "node-move"
       `((node . ,node-id) (parent . ,parent-id) (index . ,(+ current-index offset)) (silent . t))
       #'godot-rc--tscn-restore-point-after-node-move))))

(defun godot-rc-tscn-move-node-up ()
  (interactive)
  (godot-rc-tscn-move-node (- (or current-prefix-arg 1))))

(defun godot-rc-tscn-move-node-down ()
  (interactive)
  (godot-rc-tscn-move-node (or current-prefix-arg 1)))

(defun godot-rc-tscn-move-node-in ()
  (interactive)
  (let* ((section (magit-current-section))
         (prev-sibling (godot-rc--magit-section-previous-sibling section)))
    (when prev-sibling
      (let* ((node-id (magit-section-ident-value section))
             (parent-id (magit-section-ident-value prev-sibling))
             (index (length (oref prev-sibling children))))
        (godot-rc-request-callback
         "node-move"
         `((node . ,node-id) (parent . ,parent-id) (index . ,index) (silent . t))
         #'godot-rc--tscn-restore-point-after-node-move)))))

(defun godot-rc-tscn-move-node-out ()
  (interactive)
  (let* ((section (magit-current-section))
         (depth (godot-rc--magit-section-depth section)))
    (when (> depth  1)
      (let* ((node-id (magit-section-ident-value section))
             (parent (oref section parent))
             (parent-id (magit-section-ident-value (oref parent parent)))
             (current-parent-index (godot-rc--magit-section-index parent)))
        (godot-rc-request-callback
         "node-move"
         `((node . ,node-id) (parent . ,parent-id) (index . ,(+ current-parent-index 1)) (silent . t))
         #'godot-rc--tscn-restore-point-after-node-move)))))

(defun godot-rc-tscn-duplicate-node ()
  (interactive)
  (let ((node-id (magit-section-ident-value (magit-current-section))))
    (godot-rc-request "node-duplicate" node-id)))


(defun godot-rc--tscn-restore-point-after-node-move (node-id)
  (godot-rc-tscn-refresh-buffer
   nil
   (lambda () (goto-char (oref (godot-rc--get-section-from-id node-id) start)))))

(defun godot-rc-tscn-save-scene ()
  (interactive)
  (godot-rc-request "scene-save"))

(defun godot-rc--tscn-make-section (node)
  (let ((name (gethash "name" node))
        (type (gethash "type" node))
        (id (gethash "id" node))
        (children (gethash "children" node '()))
        (scene-file-path (gethash "scene_absolute_path" node))
        (scene-res-file-path (gethash "scene_res_path" node))
        (script-path (gethash "script_path" node)))
    (magit-insert-section (magit-section id)
      (magit-insert-heading
        (make-string (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) ?\s)
        (let ((icon-path (f-join godot-rc--base-dir "godot-icons" (concat type ".svg"))))
          (if (file-exists-p icon-path)
              (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))
        " "
        name
        " "
        (propertize type 'face 'magit-dimmed)

        (when (and scene-file-path (not (string-equal scene-file-path godot-rc--tscn-scene-path))) " ")
        (when (and scene-file-path (not (string-equal scene-file-path godot-rc--tscn-scene-path)))
          (let ((icon-path (f-join godot-rc--base-dir "godot-icons" "InstanceOptions.svg")))
            (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))

        (when scene-file-path " ")
        (when scene-file-path
          (propertize scene-res-file-path 'face 'magit-dimmed))

        (when script-path " ")
        (when script-path
          (let ((icon-path (f-join godot-rc--base-dir "godot-icons" "Script.svg")))
            (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))
        (when script-path " ")
        (when script-path
          (propertize script-path 'face 'magit-dimmed)))
      (unless (and scene-file-path (not (string-equal scene-file-path godot-rc--tscn-scene-path)))
        (dolist (child children)
          (godot-rc--tscn-make-section child))))))

(defun godot-rc--tscn-insert-sections (scene-tree)
  (erase-buffer)
  (godot-rc--tscn-make-section scene-tree))


(defun godot-rc-tscn-open-scene (path scene-tree)
  (with-current-buffer (get-buffer-create "*tscn-edit*")
    (tscn-mode)
    (setq-local godot-rc--tscn-scene-path path)

    (let ((inhibit-read-only t))
      (godot-rc--tscn-insert-sections scene-tree))

    (pop-to-buffer (current-buffer))))

(defun godot-rc-tscn-refresh-buffer (&optional path do-after-refresh)
  (interactive)
  (let ((tscn-buffer (get-buffer "*tscn-edit*")))
    (when tscn-buffer
      (with-current-buffer tscn-buffer
        (when (or (null path) (string-equal path godot-rc--tscn-scene-path))
          (godot-rc-request-callback
           "scene-tree"
           godot-rc--tscn-scene-path
           (lambda (scene-tree)
             (with-current-buffer tscn-buffer
               (let ((inhibit-read-only t) (cursor-position (point)))
                 (godot-rc--tscn-insert-sections scene-tree)
                 (goto-char cursor-position))
               (when do-after-refresh (funcall do-after-refresh))))))))))

(defun godot-rc-edit-scene (tscn-path)
  (godot-rc-request-callback
   "scene-tree"
   tscn-path
   (lambda (tree)
     (godot-rc-tscn-open-scene tscn-path tree))))

;;;###autoload
(defun godot-rc-open-scene ()
  (interactive)
  (let ((file
         (projectile-completing-read
          "Scene: "
          (seq-filter
           (lambda (f) (string-suffix-p ".tscn" f))
           (projectile-current-project-files)))))
    (godot-rc-edit-scene (projectile-expand-root file))))

(defun godot-rc--tscn-wip ()
  (interactive)
  (with-current-buffer (get-buffer "*tscn-edit*")
    (message (length (magit-region-sections)))))

(provide 'godot-rc-tscn-mode)
;;; godot-rc-tscn-mode.el ends here
