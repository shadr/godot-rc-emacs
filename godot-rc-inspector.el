;;; godot-rc-inspector.el --- Control godot editor without leaving Emacs -*- lexical-binding: t; -*-
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

;; TODO: perf: refresh single property on notification instead of whole buffer
;; TODO: smarter point restoration after refreshing buffer, currently point jumps if number of properties changes (changing Transform -> Rotation Edit Mode)
;; TODO: save path to inspected object as filepath/node/child-node..., currently refreshing inspector not working when scene is closed in godot
;; TODO: add keymap for the whole property text row instead of only value
;; TODO: feat: support math in number properties
;; TODO: fix: support explicit enum values, they can be stored in hint_string as "Zero,One,Three:3,Four,Six:6"
;; TODO: maybe? create single function for changing property value, store specific change-property functions in text properties
;; TODO: add checks for supported type and hint combinations
;; TODO: show number of changed properties in heading when it is collapsed
;; TODO: feat: function for reseting property to its default value
;; TODO: store scene and node path, use them instead of object id for referencing properties
;; TODO: collapse groups by default, remember which groups were opened before refresh, before closing inspector
;; TODO: render script property outside of all categories
;; TODO: receive layers names and show them in minibuffer when hovering over a layer index
;; TODO: merge category and grouping functions, accept optional 'insert-icon' argument
;; TODO: pass object-id always as strings to avoid float precision errors

(require 'magit-section)
(require 'f)

(require 'godot-rc-core)
(require 'godot-rc-utils)

(defvar godot-rc--inspector-object-id nil)
(defvar godot-rc--inspector-open-groups (make-hash-table))

(defconst godot-rc--variant-type-nil 0)
(defconst godot-rc--variant-type-bool 1) ; done
(defconst godot-rc--variant-type-int 2) ; done
(defconst godot-rc--variant-type-float 3) ; done
(defconst godot-rc--variant-type-string 4) ; done
(defconst godot-rc--variant-type-vector2 5) ; done
(defconst godot-rc--variant-type-vector2i 6) ; done
(defconst godot-rc--variant-type-rect2 7)
(defconst godot-rc--variant-type-rect2i 8)
(defconst godot-rc--variant-type-vector3 9) ; done
(defconst godot-rc--variant-type-vector3i 10) ; done
(defconst godot-rc--variant-type-transform2d 11)
(defconst godot-rc--variant-type-vector4 12) ; done
(defconst godot-rc--variant-type-vector4i 13) ; done
(defconst godot-rc--variant-type-plane 14)
(defconst godot-rc--variant-type-quaternion 15)
(defconst godot-rc--variant-type-aabb 16) ; done
(defconst godot-rc--variant-type-basis 17)
(defconst godot-rc--variant-type-transform3d 18)
(defconst godot-rc--variant-type-projection 19)
(defconst godot-rc--variant-type-color 20) ; done
(defconst godot-rc--variant-type-string_name 21)
(defconst godot-rc--variant-type-node_path 22)
(defconst godot-rc--variant-type-rid 23)
(defconst godot-rc--variant-type-object 24) ; done
(defconst godot-rc--variant-type-callable 25)
(defconst godot-rc--variant-type-signal 26)
(defconst godot-rc--variant-type-dictionary 27)
(defconst godot-rc--variant-type-array 28)
(defconst godot-rc--variant-type-packed_byte_array 29)
(defconst godot-rc--variant-type-packed_int32_array 30)
(defconst godot-rc--variant-type-packed_int64_array 31)
(defconst godot-rc--variant-type-packed_float32_array 32)
(defconst godot-rc--variant-type-packed_float64_array 33)
(defconst godot-rc--variant-type-packed_string_array 34)
(defconst godot-rc--variant-type-packed_vector2_array 35)
(defconst godot-rc--variant-type-packed_vector3_array 36)
(defconst godot-rc--variant-type-packed_color_array 37)
(defconst godot-rc--variant-type-packed_vector4_array 38)
(defconst godot-rc--variant-type-variant_max 39)

(defconst godot-rc--property_hint_none 0)
(defconst godot-rc--property_hint_range 1)
(defconst godot-rc--property_hint_enum 2)
(defconst godot-rc--property_hint_enum_suggestion 3)
(defconst godot-rc--property_hint_exp_easing 4)
(defconst godot-rc--property_hint_link 5)
(defconst godot-rc--property_hint_flags 6)
(defconst godot-rc--property_hint_layers_2d_render 7)
(defconst godot-rc--property_hint_layers_2d_physics 8)
(defconst godot-rc--property_hint_layers_2d_navigation 9)
(defconst godot-rc--property_hint_layers_3d_render 10)
(defconst godot-rc--property_hint_layers_3d_physics 11)
(defconst godot-rc--property_hint_layers_3d_navigation 12)
(defconst godot-rc--property_hint_file 13)
(defconst godot-rc--property_hint_dir 14)
(defconst godot-rc--property_hint_global_file 15)
(defconst godot-rc--property_hint_global_dir 16)
(defconst godot-rc--property_hint_resource_type 17)
(defconst godot-rc--property_hint_multiline_text 18)
(defconst godot-rc--property_hint_expression 19)
(defconst godot-rc--property_hint_placeholder_text 20)
(defconst godot-rc--property_hint_color_no_alpha 21)
(defconst godot-rc--property_hint_object_id 22)
(defconst godot-rc--property_hint_type_string 23)
(defconst godot-rc--property_hint_node_path_to_edited_node 24)
(defconst godot-rc--property_hint_object_too_big 25)
(defconst godot-rc--property_hint_node_path_valid_types 26)
(defconst godot-rc--property_hint_save_file 27)
(defconst godot-rc--property_hint_global_save_file 28)
(defconst godot-rc--property_hint_int_is_objectid 29)
(defconst godot-rc--property_hint_int_is_pointer 30)
(defconst godot-rc--property_hint_array_type 31)
(defconst godot-rc--property_hint_locale_id 32)
(defconst godot-rc--property_hint_localizable_string 33)
(defconst godot-rc--property_hint_node_type 34)
(defconst godot-rc--property_hint_hide_quaternion_edit 35)
(defconst godot-rc--property_hint_password 36)
(defconst godot-rc--property_hint_layers_avoidance 37)
(defconst godot-rc--property_hint_dictionary_type 38)
(defconst godot-rc--property_hint_tool_button 39)
(defconst godot-rc--property_hint_oneshot 40)
(defconst godot-rc--property_hint_no_nodepath 41)
(defconst godot-rc--property_hint_group_enable 42)
(defconst godot-rc--property_hint_input_name 43)
(defconst godot-rc--property_hint_max 44)

(defconst godot-rc--property_usage_none 0)
(defconst godot-rc--property_usage_storage (ash 1 1))
(defconst godot-rc--property_usage_editor (ash 1 2))
(defconst godot-rc--property_usage_internal (ash 1 3))
(defconst godot-rc--property_usage_checkable (ash 1 4))
(defconst godot-rc--property_usage_checked (ash 1 5))
(defconst godot-rc--property_usage_group (ash 1 6))
(defconst godot-rc--property_usage_category (ash 1 7))
(defconst godot-rc--property_usage_subgroup (ash 1 8))
(defconst godot-rc--property_usage_class_is_bitfield (ash 1 9))
(defconst godot-rc--property_usage_no_instance_state (ash 1 10))
(defconst godot-rc--property_usage_restart_if_changed (ash 1 11))
(defconst godot-rc--property_usage_script_variable (ash 1 12))
(defconst godot-rc--property_usage_store_if_null (ash 1 13))
(defconst godot-rc--property_usage_update_all_if_modified (ash 1 14))
(defconst godot-rc--property_usage_script_default_value (ash 1 15))
(defconst godot-rc--property_usage_class_is_enum (ash 1 16))
(defconst godot-rc--property_usage_nil_is_variant (ash 1 17))
(defconst godot-rc--property_usage_array (ash 1 18))
(defconst godot-rc--property_usage_always_duplicate (ash 1 19))
(defconst godot-rc--property_usage_never_duplicate (ash 1 20))
(defconst godot-rc--property_usage_high_end_gfx (ash 1 21))
(defconst godot-rc--property_usage_node_path_from_scene_root (ash 1 22))
(defconst godot-rc--property_usage_resource_not_persistent (ash 1 23))
(defconst godot-rc--property_usage_keying_increments (ash 1 24))
(defconst godot-rc--property_usage_deferred_set_resource (ash 1 25))
(defconst godot-rc--property_usage_editor_instantiate_object (ash 1 26))
(defconst godot-rc--property_usage_editor_basic_setting (ash 1 27))
(defconst godot-rc--property_usage_read_only (ash 1 28))
(defconst godot-rc--property_usage_secret (ash 1 29))

(defvar inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-section-toggle] #'godot-rc--inspector-section-toggle)
    map))

(declare-function evil-define-key "evil")
(with-eval-after-load 'evil
  (evil-define-key 'normal inspector-mode-map (kbd "R") #'godot-rc-inspector-refresh-buffer))

(define-derived-mode inspector-mode magit-section-mode "INSPECTOR")

(defun godot-rc--inspector-section-toggle (section)
  (interactive (list (magit-current-section)))
  (when (oref section hidden)
    (when (string-equal "object" (get-text-property (oref section start) 'type))
      (let ((opened-list (gethash godot-rc--inspector-object-id godot-rc--inspector-open-groups))
            (value (get-text-property (oref section start) 'value)))
        (when (when (and (numberp value) (not (member value opened-list)))
                (cl-pushnew value opened-list)
                (puthash godot-rc--inspector-object-id opened-list godot-rc--inspector-open-groups)
                t)
          (godot-rc-inspector-refresh-buffer)))))
  (magit-section-toggle section))

(defun godot-rc--get-object-properties (object-id callback)
  (godot-rc-request-callback
   "object-properties"
   `((object_id . ,object-id)
     ;; (scene_path . "scene-path")
     ;; (node_path . "node-path")
     ;; (opened_objects . "objects")
     (opened_props . ,(mapcar #'number-to-string (gethash object-id godot-rc--inspector-open-groups))))
   callback))

(defun godot-rc-node-open-inspector ()
  (interactive)
  (let ((node-id (magit-section-ident-value (magit-current-section))))
    (godot-rc--open-inspector node-id)))

(defun godot-rc-inspector-refresh-buffer ()
  (interactive)
  (with-current-buffer (get-buffer "*inspector*")
    (godot-rc--get-object-properties
     godot-rc--inspector-object-id
     (lambda (data)
       (with-current-buffer (get-buffer "*inspector*")

         (let ((p (point)) (wstart (window-start)))
           (godot-rc--inspector-insert-sections data godot-rc--inspector-object-id)
           (goto-char p)
           (set-window-start (selected-window) wstart)))))))

(defun godot-rc--open-inspector (object-id)
  (godot-rc--get-object-properties
   object-id
   (lambda (data)
     (with-current-buffer (get-buffer-create "*inspector*")
       (inspector-mode)
       (setq-local godot-rc--inspector-object-id object-id)
       (add-hook 'magit-section-set-visibility-hook #'godot-rc--inspector-visibility-hook 0 t)
       (godot-rc--inspector-insert-sections data object-id)
       (pop-to-buffer-same-window (current-buffer))))))

(defun godot-rc--inspector-visibility-hook (section)
  'show)

(defun godot-rc--inspector-insert-sections-nested (data object-id)
  (let ((start (point)))
    (magit-insert-section (magit-section "inspector-root")
      (dolist (d data) (godot-rc--inspector-insert-category-section d object-id)))
    (put-text-property start (point) 'object-id godot-rc--inspector-object-id)))

(defun godot-rc--inspector-insert-sections (data object-id)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays)
    (godot-rc--inspector-insert-sections-nested data object-id)))

(defun godot-rc--inspector-insert-section (data object-id)
  (let* ((visible-name (gethash "visible_name" data))
         (children (gethash "children" data)))
    (cond
     (visible-name (godot-rc--inspector-insert-property-section data object-id))
     (children (godot-rc--inspector-insert-grouping-section data object-id)))))

(defun godot-rc--inspector-insert-category-section (data object-id)
  (let* ((name (gethash "name" data))
         (children (gethash "children" data)))
    (magit-insert-section (magit-section `(:type 'category))
      (magit-insert-heading
        (make-string (* 2 (- (godot-rc--magit-section-depth magit-insert-section--current) 1)) ?\s)
        (let ((icon-path (f-join godot-rc--base-dir "godot-icons" (concat name ".svg"))))
          (if (file-exists-p icon-path)
              (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))
        name)

      (magit-insert-section-body
        (let ((start (point)))
          (when children
            (dolist (child children) (godot-rc--inspector-insert-section child object-id)))
          (put-text-property start (point) 'object-id object-id))))))

(defun godot-rc--inspector-insert-grouping-section (data object-id)
  (let* ((name (gethash "name" data))
         (children (gethash "children" data))
         (start (point)))
    (magit-insert-section (magit-section `(:type 'grouping))
      (magit-insert-heading
        (make-string (* 2 (- (godot-rc--magit-section-depth magit-insert-section--current) 1)) ?\s)
        name)
      (magit-insert-section-body
        (when children
          (dolist (child children) (godot-rc--inspector-insert-section child object-id)))))
    (put-text-property start (point) 'object-id object-id)))

(defun godot-rc--inspector-insert-property-section (data object-id)
  (let* ((type (gethash "type" data))
         (start (point)))
    (pcase type
      ((guard (eq type godot-rc--variant-type-bool)) (godot-rc--inspector-insert-bool-property data object-id))
      ((guard (eq type godot-rc--variant-type-int)) (godot-rc--inspector-insert-int-property data object-id))
      ((guard (eq type godot-rc--variant-type-float)) (godot-rc--inspector-insert-float-property data object-id))
      ((guard (eq type godot-rc--variant-type-vector2)) (godot-rc--inspector-insert-vector2-property data object-id))
      ((guard (eq type godot-rc--variant-type-vector3)) (godot-rc--inspector-insert-vector3-property data object-id))
      ((guard (eq type godot-rc--variant-type-vector4)) (godot-rc--inspector-insert-vector4-property data object-id))
      ((guard (eq type godot-rc--variant-type-vector2i)) (godot-rc--inspector-insert-vector2i-property data object-id))
      ((guard (eq type godot-rc--variant-type-vector3i)) (godot-rc--inspector-insert-vector3i-property data object-id))
      ((guard (eq type godot-rc--variant-type-vector4i)) (godot-rc--inspector-insert-vector4i-property data object-id))
      ((guard (eq type godot-rc--variant-type-string)) (godot-rc--inspector-insert-string-property data object-id))
      ((guard (eq type godot-rc--variant-type-color)) (godot-rc--inspector-insert-color-property data object-id))
      ((guard (eq type godot-rc--variant-type-object)) (godot-rc--inspector-insert-object-property data object-id))
      ((guard (eq type godot-rc--variant-type-aabb)) (godot-rc--inspector-insert-aabb-property data object-id))
      ((guard (eq type godot-rc--variant-type-node_path)) (godot-rc--inspector-insert-node-path-property data object-id))
      (_ (godot-rc--inspector-insert-unsupported-property data object-id)))
    (when (eq start (point))
      (message (concat "warning: property " (gethash "property" data) " didn't show up in inspector, hint: " (number-to-string (gethash "hint" data)))))
    (put-text-property start (point) 'property-name (gethash "property" data))
    (put-text-property start (point) 'hint-string (gethash "hint_string" data))
    (put-text-property start (point) 'value (gethash "value" data))))

(defun godot-rc--inspector-insert-visible-name (data)
  (let* ((indent (- (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) 1)))
    (insert (make-string indent ?\s)
            (if (not (eq (gethash "non_default" data) :false)) "*" " ")
            (gethash "visible_name" data)
            " ")))

(defun godot-rc--inspector-change-property-under-point (value &optional property)
  (unless property (setq property (get-text-property (point) 'property-name)))
  (godot-rc-request
   "inspector-change-property"
   `((object_id . ,(get-text-property (point) 'object-id))
     (property . ,property)
     (value . ,value))))

(defmacro godot-rc--magit-insert-section-body (object-id &rest body)
  `(let ((start (point)))
     (magit-insert-section-body ,@body)
     (put-text-property start (point) 'object-id ,object-id)))

(defmacro godot-rc--inspector-simple-body (data object-id &rest body)
  `(godot-rc--magit-insert-section-body
    ,object-id
    (godot-rc--inspector-insert-visible-name ,data)
    (progn ,@body)
    (insert "\n")))

(defmacro godot-rc--define-simple-property (name form keymap)
  `(defun ,(intern (format "godot-rc--inspector-insert-%s-property" name)) (data object-id)
     (godot-rc--inspector-simple-body
      data
      object-id
      (insert (propertize ,form
                          'face 'underline
                          'keymap ,keymap)))))

(defvar godot-rc--inspector-bool-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-bool-change)
    map))

(defun godot-rc--inspector-bool-change ()
  (interactive)
  (let* ((previous-value (get-text-property (point) 'value)))
    (godot-rc--inspector-change-property-under-point (eq previous-value :false))))

(godot-rc--define-simple-property bool (if (eq (gethash "value" data) :false) "FALSE" "TRUE") godot-rc--inspector-bool-keymap)

(defvar godot-rc--inspector-int-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-int-change)
    map))

(defun godot-rc--inspector-int-change ()
  (interactive)
  (let* ((new-value (read-number (concat "New value for " (get-text-property (point)'property-name) ": "))))
    (godot-rc--inspector-change-property-under-point new-value)))

(defun godot-rc--inspector-insert-int-property (data object-id)
  (let ((hint (gethash "hint" data)))
    (pcase hint
      ((guard (eq hint godot-rc--property_hint_none)) (godot-rc--inspector-insert-int-number-property data object-id))
      ((guard (eq hint godot-rc--property_hint_range)) (godot-rc--inspector-insert-int-range-property data object-id))
      ((guard (eq hint godot-rc--property_hint_enum)) (godot-rc--inspector-insert-int-enum-property data object-id))
      ((guard (eq hint godot-rc--property_hint_layers_2d_render)) (godot-rc--inspector-insert-int-layers-property data object-id))
      ((guard (eq hint godot-rc--property_hint_layers_2d_physics)) (godot-rc--inspector-insert-int-layers-property data object-id))
      ((guard (eq hint godot-rc--property_hint_layers_2d_navigation)) (godot-rc--inspector-insert-int-layers-property data object-id))
      ((guard (eq hint godot-rc--property_hint_layers_3d_render)) (godot-rc--inspector-insert-int-layers-property data object-id))
      ((guard (eq hint godot-rc--property_hint_layers_3d_physics)) (godot-rc--inspector-insert-int-layers-property data object-id))
      ((guard (eq hint godot-rc--property_hint_layers_3d_navigation)) (godot-rc--inspector-insert-int-layers-property data object-id)))))

(godot-rc--define-simple-property int-number (number-to-string (gethash "value" data)) godot-rc--inspector-int-keymap)

(defvar godot-rc--inspector-int-enum-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-int-enum-change)
    map))

(defun godot-rc--inspector-int-enum-change ()
  (interactive)
  (let* ((options (get-text-property (point) 'options))
         (new-option (completing-read (concat "New value for " (get-text-property (point) 'property-name) ": ") options))
         (new-value (cl-position new-option options :test 'equal)))
    (godot-rc--inspector-change-property-under-point new-value)))

(defun godot-rc--inspector-insert-int-enum-property (data object-id)
  (let ((enum-options (split-string (gethash "hint_string" data) ",")))
    (godot-rc--inspector-simple-body
     data
     object-id
     (insert (propertize (nth (gethash "value" data) enum-options)
                         'face 'underline
                         'keymap godot-rc--inspector-int-enum-keymap
                         'options enum-options)))))

(defvar godot-rc--inspector-int-range-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-int-range-change)
    map))

(defun godot-rc--inspector-int-range-change ()
  (interactive)
  (let* ((split (string-split (get-text-property (point) 'hint-string) ","))
         (min-value (nth 0 split))
         (max-value (nth 1 split))
         (new-value (read-number (format "New value [%s..%s]: " min-value max-value))))
    (godot-rc--inspector-change-property-under-point new-value)))

(defun godot-rc--inspector-insert-int-range-property (data object-id)
  (godot-rc--magit-insert-section-body
   object-id
   (godot-rc--inspector-insert-visible-name data)
   (insert (propertize (number-to-string (gethash "value" data))
                       'face 'underline
                       'keymap godot-rc--inspector-int-range-keymap))
   (insert "\n")))

(defvar godot-rc--inspector-layers-component-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-layers-component-change)
    map))

(defun godot-rc--inspector-layers-component-change ()
  (interactive)
  (let* ((property-name (get-text-property (point) 'property-name))
         (bit (get-text-property (point) 'bit))
         (old-value (get-text-property (point) 'value))
         (new-value (logxor old-value (ash 1 bit))))
    (godot-rc--inspector-change-property-under-point new-value property-name)))

(defun godot-rc--inspector-insert-layers-component (layers index)
  (propertize (format "%2s" (number-to-string (+ index 1)))
              'face `(:underline t :foreground ,(if (not (zerop (logand layers (ash 1 index)))) "white" "dim gray"))
              'keymap godot-rc--inspector-layers-component-keymap
              'bit index))

(defun godot-rc--inspector-insert-int-layers-property (data object-id)
  (let ((layers (gethash "value" data)))
    (godot-rc--magit-insert-section-body
     object-id
     (godot-rc--inspector-insert-visible-name data)
     (insert "\n")
     (insert (make-string (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) ?\s))
     (cl-loop for i from 0 to 9
              do (insert (godot-rc--inspector-insert-layers-component layers i) " "))
     (insert "\n" (make-string (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) ?\s))
     (cl-loop for i from 10 to 19
              do (insert (godot-rc--inspector-insert-layers-component layers i) " "))
     (insert "\n"))))

;; (defvar godot-rc--inspector-vector3-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "c") #'godot-rc--inspector-vector3-change)
;;     map))

;; (defun godot-rc--inspector-vector3-change ())

(defvar godot-rc--inspector-vector-component-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-vector-component-change)
    map))

(defun godot-rc--inspector-vector-component-change ()
  (interactive)
  (let* ((property-name (get-text-property (point) 'property-name))
         (field (get-text-property (point) 'field))
         (new-value (read-number "New value: ")))
    (godot-rc--inspector-change-property-under-point new-value (list property-name field))))

(defvar godot-rc--inspector-vectori-component-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-vectori-component-change)
    map))

(defun godot-rc--inspector-vectori-component-change ()
  (interactive)
  (let* ((property-name (get-text-property (point) 'property-name))
         (field (get-text-property (point) 'field))
         (new-value (read-number "New value: ")))
    (godot-rc--inspector-change-property-under-point new-value (list property-name field))))

(defmacro godot-rc--inspector-vector-definitions-macro (name component &rest fields)
  (let ((fun-name (intern (format "godot-rc--inspector-insert-%s-property" name)))
        (keymap-name (intern (format "godot-rc--inspector-%s-component-keymap" component))))
    `(defun ,fun-name (data object-id)
       (let* ((split (string-split (string-trim (gethash "value" data) "[\(\)]" "[\(\)]") ", "))
              ,@(cl-loop for field in fields
                         for i from 0
                         collect `(,field (nth ,i split))))

         (godot-rc--magit-insert-section-body
          object-id
          (godot-rc--inspector-insert-visible-name data)
          ,@(cl-loop for field in fields
                     collect `(insert (propertize ,field
                                                  'face 'underline
                                                  'keymap ,keymap-name
                                                  'field ',field))
                     collect `(insert " "))
          (insert "\n"))))))

(godot-rc--inspector-vector-definitions-macro vector2 vector x y)
(godot-rc--inspector-vector-definitions-macro vector3 vector x y z)
(godot-rc--inspector-vector-definitions-macro vector4 vector x y z w)
(godot-rc--inspector-vector-definitions-macro vector2i vectori x y)
(godot-rc--inspector-vector-definitions-macro vector3i vectori x y z)
(godot-rc--inspector-vector-definitions-macro vector4i vectori x y z w)

(defvar godot-rc--inspector-string-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-string-change)
    map))

(defun godot-rc--inspector-string-change ()
  (interactive)
  (let* ((old-string (get-text-property (point) 'value))
         (new-string (read-string "New string: " old-string)))
    (godot-rc--inspector-change-property-under-point new-string)))

(godot-rc--define-simple-property string
                                  (if (string-equal (gethash "value" data) "") "EMPTY" (gethash "value" data))
                                  godot-rc--inspector-string-keymap)

(defvar godot-rc--inspector-float-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-float-change)
    map))

(defun godot-rc--inspector-float-change ()
  (interactive)
  (let* ((new-value (read-number (concat "New value for " (get-text-property (point) 'property-name) ": "))))
    (godot-rc--inspector-change-property-under-point new-value)))

(godot-rc--define-simple-property float (number-to-string (gethash "value" data)) godot-rc--inspector-float-keymap)

;; (defvar godot-rc--inspector-color-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "c") #'godot-rc--inspector-color-change)
;;     map))

;; (defun godot-rc--inspector-color-change ()
;;   (interactive))


(defun godot-rc--inspector-insert-color-property (data object-id)
  (let* ((split
          (string-split
           (string-trim (gethash "value" data) "[()]" "[()]") ", "))
         (r (nth 0 split))
         (g (nth 1 split))
         (b (nth 2 split))
         (a (nth 3 split))
         (color-code (format "#%02x%02x%02x"
                             (round (* (string-to-number r) 255))
                             (round (* (string-to-number g) 255))
                             (round (* (string-to-number b) 255)))))
    (godot-rc--inspector-simple-body
     data
     object-id

     (insert (propertize "■ " 'face `(:foreground ,color-code)))
     (insert (propertize r 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'r))
     (insert " ")
     (insert (propertize g 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'g))
     (insert " ")
     (insert (propertize b 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'b))
     (insert " ")
     (insert (propertize a 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'a)))))

(defun godot-rc--inspector-insert-object-property (data object-id)
  (let ((start (point)) (children (gethash "children" data)))
    (magit-insert-section (magit-section)
      (magit-insert-heading
        (make-string (* 2 (- (godot-rc--magit-section-depth magit-insert-section--current) 1)) ?\s)
        (gethash "visible_name" data)
        (when (and (symbolp (gethash "value" data)) (eq :null (gethash "value" data))) " <empty>"))
      (godot-rc--magit-insert-section-body
       (gethash "value" data)
       (when children
         (dolist (child children) (godot-rc--inspector-insert-section child object-id)))))
    (put-text-property start (point) 'type 'object)))

(defun godot-rc--inspector-insert-aabb-property (data object-id)
  (let* ((value (gethash "value" data))
         (x (nth 0 value))
         (y (nth 1 value))
         (z (nth 2 value))
         (w (nth 3 value))
         (h (nth 4 value))
         (d (nth 5 value)))
    (godot-rc--inspector-simple-body
     data
     object-id
     (insert "\n")
     (insert (make-string (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) ?\s))
     (insert (propertize (number-to-string x) 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'position:x))
     (insert " ")
     (insert (propertize (number-to-string y) 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'position:y))
     (insert " ")
     (insert (propertize (number-to-string z) 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'position:z))
     (insert "\n")
     (insert (make-string (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) ?\s))
     (insert (propertize (number-to-string w) 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'size:x))
     (insert " ")
     (insert (propertize (number-to-string h) 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'size:y))
     (insert " ")
     (insert (propertize (number-to-string d) 'face 'underline 'keymap
                         godot-rc--inspector-vector-component-keymap 'field 'size:z)))))

(defvar godot-rc--inspector-node-path-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'godot-rc--inspector-node-path-change)
    map))

(defun godot-rc--inspector-node-path-change ()
  (interactive)
  (godot-rc-request-callback
   "inspector-query-node-paths"
   `((object_id . ,(get-text-property (point) 'object-id))
     (classes . ,(string-split (get-text-property (point) 'hint-string) ",")))
   (lambda (result)
     (if result
         (let ((new-value (completing-read "New NodePath: " result)))
           (godot-rc--inspector-change-property-under-point new-value))
       (message "No suitable nodes found in a scene")))))

(defun godot-rc--inspector-insert-node-path-property (data object-id)
  (let* ((target-node (gethash "additional_info" data)))
    (godot-rc--inspector-simple-body
     data object-id
     (if target-node
         (progn
           (let ((icon-path (f-join godot-rc--base-dir "godot-icons" (concat (gethash "type" target-node) ".svg"))))
             (when (file-exists-p icon-path)
               (insert (propertize "@" 'display (create-image icon-path nil nil :ascent 'center)))))
           (insert (propertize (gethash "name" target-node) 'face 'underline 'keymap godot-rc--inspector-node-path-keymap)))
       (insert (propertize "<empty>" 'face 'underline 'keymap godot-rc--inspector-node-path-keymap))))))

(godot-rc--define-simple-property unsupported
                                  (concat "UNSUPPORTED"
                                          " type: " (number-to-string (gethash "type" data))
                                          " hint:" (number-to-string (gethash "hint" data))
                                          " hint_string: " (gethash "hint_string" data))
                                  nil)

(godot-rc-add-notification-handler "property-changed" #'godot-rc--inspector-on-property-changed)

(defun godot-rc--inspector-on-property-changed (_params)
  (godot-rc-inspector-refresh-buffer))

(provide 'godot-rc-inspector)
;;; godot-rc-inspector.el ends here
