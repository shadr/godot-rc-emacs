;;; tscn-mode.el --- Edit godot scenes without leaving emacs! -*- lexical-binding: t; -*-

;;; Code:

(require 'magit-section)
(require 'evil)
(require 'godot-rc-core)
(require 'projectile)
(require 'f)

(defvar tscn-mode--scene-path nil)
(defconst tscn--base-dir (file-name-directory (or load-file-name buffer-file-name)))

(define-derived-mode tscn-mode magit-section-mode "TSCN"
  (define-key tscn-mode-map [remap save-buffer] #'tscn-mode--save)
  (evil-define-key 'normal tscn-mode-map (kbd "o") 'godot-rc-open-scene)
  (evil-define-key 'normal tscn-mode-map (kbd "r") 'tscn-rename-node)
  (evil-define-key 'normal tscn-mode-map (kbd "c") 'tscn-change-node-type)
  (evil-define-key 'normal tscn-mode-map (kbd "R") 'tscn-refresh-buffer)
  (evil-define-key 'normal tscn-mode-map (kbd "D") 'tscn-delete-node)
  (evil-define-key 'normal tscn-mode-map (kbd "d") 'tscn-duplicate-node)
  (evil-define-key 'normal tscn-mode-map (kbd "a") 'tscn-add-sibling-node)
  (evil-define-key 'normal tscn-mode-map (kbd "A") 'tscn-add-child-node)
  (evil-define-key 'normal tscn-mode-map (kbd "C-j") 'tscn-mode-move-node-down)
  (evil-define-key 'normal tscn-mode-map (kbd "C-k") 'tscn-mode-move-node-up)
  (evil-define-key 'normal tscn-mode-map (kbd "C-l") 'tscn-mode-move-node-in)
  (evil-define-key 'normal tscn-mode-map (kbd "C-h") 'tscn-mode-move-node-out)
  (evil-define-key 'normal tscn-mode-map (kbd "RET") 'something))


(defun something ()
  (interactive)
  (message (tscn-mode--get-section-from-id (magit-section-ident-value (magit-current-section)))))

(defun tscn-delete-node ()
  (interactive)
  (let ((node-id (magit-section-ident-value (magit-current-section)))
        (confirm (y-or-n-p "Delete node and its children?")))
    (when confirm (godot-rc-request "remove-node" node-id))))

(defun tscn-add-node (parent-id index)
  (godot-rc-request-callback
   "get-node-classes"
   nil
   (lambda (result)
     (let ((type (completing-read "Node type: " result)))
       (godot-rc-request "node-add" `((parent_id . ,parent-id) (index . ,index) (type . ,type)))))))

(defun tscn-add-sibling-node ()
  (interactive)
  (let* ((section (magit-current-section))
         (parent-id (magit-section-ident-value (oref section parent)))
         (current-index (tscn-mode--magit-section-index section)))
    (tscn-add-node parent-id (+ current-index 1))))

(defun tscn-add-child-node ()
  (interactive)
  (let* ((section (magit-current-section))
         (parent-id (magit-section-ident-value section))
         (index (length (oref section children))))
    (tscn-add-node parent-id (+ index 1))))

(defun tscn-rename-node ()
  (interactive)
  (let ((current-section (magit-section-ident-value (magit-current-section)))
        (new-name (read-string "New name: ")))
    (godot-rc-request "rename-node" `((id . ,current-section) (name . ,new-name)))))

(defun tscn-change-node-type ()
  ;; https://github.com/godotengine/godot/blob/428a762e9866afc41e9ba7fa334751142a81b432/editor/scene_tree_dock.cpp#L3146
  (interactive)
  (let ((current-section (magit-section-ident-value (magit-current-section))))
    (godot-rc-request-callback
     "get-node-classes"
     nil
     (lambda (result)
       (let ((new-type (completing-read "New type: " result)))
         (godot-rc-request "node-change-type" `((node_id . ,current-section) (new_type . ,new-type))))))))

(defun tscn-mode--get-section-from-id (value &optional root)
  (setq root (or root magit-root-section))
  (if (equal value (oref root value))
      root
    (cl-loop for child in (oref root children)
             thereis (tscn-mode--get-section-from-id value child))))


(defun tscn-mode--get-section-start-position (section)
  (let ((position (oref section start)))
    (while (setq section (oref section parent)) (setq position (+ position (oref section start))))
    position))

(defun tscn-mode-move-node (offset)
  (let* ((section (magit-current-section))
         (max-index (- (length (oref (oref section parent) children)) 1))
         (current-index (tscn-mode--magit-section-index section))
         (node-id (magit-section-ident-value section))
         (parent-id (magit-section-ident-value (oref section parent)))
         (index (max (min (+ current-index offset) max-index) 0)))
    (unless (eq index current-index)
      (godot-rc-request-callback
       "move-node"
       `((node . ,node-id) (parent . ,parent-id) (index . ,(+ current-index offset)) (silent . t))
       'tscn-mode--restore-point-after-node-move))))

(defun tscn-mode-move-node-up ()
  (interactive)
  (tscn-mode-move-node (- (or current-prefix-arg 1))))

(defun tscn-mode-move-node-down ()
  (interactive)
  (tscn-mode-move-node (or current-prefix-arg 1)))

(defun tscn-mode-move-node-in ()
  (interactive)
  (let* ((section (magit-current-section))
         (prev-sibling (tscn-mode--magit-section-previous-sibling section)))
    (when prev-sibling
      (let* ((node-id (magit-section-ident-value section))
             (parent-id (magit-section-ident-value prev-sibling))
             (index (length (oref prev-sibling children))))
        (godot-rc-request-callback
         "move-node"
         `((node . ,node-id) (parent . ,parent-id) (index . ,index) (silent . t))
         'tscn-mode--restore-point-after-node-move)))))

(defun tscn-mode-move-node-out ()
  (interactive)
  (let* ((section (magit-current-section))
         (depth (tscn--magit-section-depth section)))
    (when (> depth  1)
      (let* ((node-id (magit-section-ident-value section))
             (parent (oref section parent))
             (parent-id (magit-section-ident-value (oref parent parent)))
             (current-parent-index (tscn-mode--magit-section-index parent)))
        (godot-rc-request-callback
         "move-node"
         `((node . ,node-id) (parent . ,parent-id) (index . ,(+ current-parent-index 1)) (silent . t))
         'tscn-mode--restore-point-after-node-move)))))

(defun tscn-duplicate-node ()
  (interactive)
  (let ((node-id (magit-section-ident-value (magit-current-section))))
    (godot-rc-request "node-duplicate" node-id)))


(defun tscn-mode--restore-point-after-node-move (node-id)
  (tscn-refresh-buffer
   nil
   (lambda () (goto-char (oref (tscn-mode--get-section-from-id node-id) start)))))

(defun tscn-mode--save ()
  (interactive)
  (godot-rc-request "save-scene"))

(defun tscn-mode--magit-section-index (section)
  (cl-position section (oref (oref section parent) children)))

(defun tscn-mode--magit-section-previous-sibling (section)
  (let ((position (tscn-mode--magit-section-index section)))
    (when (> position 0)
      (nth (- position 1) (oref (oref section parent) children)))))

(defun tscn-mode--make-section (node)
  (let ((name (gethash "name" node))
        (type (gethash "type" node))
        (id (gethash "id" node))
        (children (gethash "children" node '()))
        (scene-file-path (gethash "scene_absolute_path" node))
        (scene-res-file-path (gethash "scene_res_path" node))
        (script-path (gethash "script_path" node)))
    (magit-insert-section (magit-section id)
      (magit-insert-heading
        (make-string (* 2 (tscn--magit-section-depth)) ?\s)
        (let ((icon-path (f-join tscn--base-dir "godot-icons" (concat type ".svg"))))
          (if (file-exists-p icon-path)
              (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))
        " "
        name
        " "
        (propertize type 'face 'magit-dimmed)

        (when (and scene-file-path (not (string-equal scene-file-path tscn-mode--scene-path))) " ")
        (when (and scene-file-path (not (string-equal scene-file-path tscn-mode--scene-path)))
          (let ((icon-path (f-join tscn--base-dir "godot-icons" "InstanceOptions.svg")))
            (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))

        (when scene-file-path " ")
        (when scene-file-path
          (propertize scene-res-file-path 'face 'magit-dimmed))

        (when script-path " ")
        (when script-path
          (let ((icon-path (f-join tscn--base-dir "godot-icons" "Script.svg")))
            (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))
        (when script-path " ")
        (when script-path
          (propertize script-path 'face 'magit-dimmed)))
      (unless (and scene-file-path (not (string-equal scene-file-path tscn-mode--scene-path)))
        (dolist (child children)
          (tscn-mode--make-section child))))))


(cl-defun tscn--magit-section-depth (&optional section)
  (unless section (setq section magit-insert-section--current))
  (let ((depth 0))
    (while (setq section (oref section parent)) (cl-incf depth))
    depth))

(defun tscn--insert-sections (scene-tree)
  (erase-buffer)
  (tscn-mode--make-section scene-tree))


(defun tscn-open-scene (path scene-tree)
  (with-current-buffer (get-buffer-create "*tscn-edit*")
    (tscn-mode)
    (setq-local tscn-mode--scene-path path)

    (let ((inhibit-read-only t))
      (tscn--insert-sections scene-tree))

    (pop-to-buffer (current-buffer))))

(defun tscn-refresh-buffer (&optional path do-after-refresh)
  (interactive)
  (let ((tscn-buffer (get-buffer "*tscn-edit*")))
    (when tscn-buffer
      (with-current-buffer tscn-buffer
        (when (or (null path) (string-equal path tscn-mode--scene-path))
          (godot-rc-request-callback
           "get-scene-tree"
           tscn-mode--scene-path
           (lambda (scene-tree)
             (with-current-buffer tscn-buffer
               (let ((inhibit-read-only t) (cursor-position (point)))
                 (tscn--insert-sections scene-tree)
                 (goto-char cursor-position))
               (when do-after-refresh (funcall do-after-refresh))))))))))

(defun godot-rc-edit-scene (tscn-path)
  (godot-rc-request-callback
   "get-scene-tree"
   tscn-path
   (lambda (tree)
     (tscn-open-scene tscn-path tree))))

(defun godot-rc-open-scene ()
  (interactive)
  (let ((file
         (projectile-completing-read
          "Scene: "
          (seq-filter
           (lambda (f) (string-suffix-p ".tscn" f))
           (projectile-current-project-files)))))
    (godot-rc-edit-scene (projectile-expand-root file))))

(defun tscn--wip ()
  (interactive)
  (with-current-buffer (get-buffer "*tscn-edit*")
    (message (length (magit-region-sections)))))

(provide 'tscn-mode)

;;; tscn-mode.el ends here
