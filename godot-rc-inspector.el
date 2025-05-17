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

(require 'magit-section)
(require 'f)

(require 'godot-rc-core)
(require 'godot-rc-utils)

(defconst godot-rc--variant-type-nil 0)
(defconst godot-rc--variant-type-bool 1)
(defconst godot-rc--variant-type-int 2)
(defconst godot-rc--variant-type-float 3)
(defconst godot-rc--variant-type-string 4)
(defconst godot-rc--variant-type-vector2 5)
(defconst godot-rc--variant-type-vector2i 6)
(defconst godot-rc--variant-type-rect2 7)
(defconst godot-rc--variant-type-rect2i 8)
(defconst godot-rc--variant-type-vector3 9)
(defconst godot-rc--variant-type-vector3i 10)
(defconst godot-rc--variant-type-transform2d 11)
(defconst godot-rc--variant-type-vector4 12)
(defconst godot-rc--variant-type-vector4i 13)
(defconst godot-rc--variant-type-plane 14)
(defconst godot-rc--variant-type-quaternion 15)
(defconst godot-rc--variant-type-aabb 16)
(defconst godot-rc--variant-type-basis 17)
(defconst godot-rc--variant-type-transform3d 18)
(defconst godot-rc--variant-type-projection 19)
(defconst godot-rc--variant-type-color 20)
(defconst godot-rc--variant-type-string_name 21)
(defconst godot-rc--variant-type-node_path 22)
(defconst godot-rc--variant-type-rid 23)
(defconst godot-rc--variant-type-object 24)
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

(define-derived-mode inspector-mode magit-section-mode "INSPECTOR")

(defun godot-rc--get-node-properties (object-id callback)
  (godot-rc-request-callback "node-properties" `((node_id . ,object-id) (opened_props . ())) callback))

(defun godot-rc-node-open-inspector ()
  (interactive)
  (let ((node-id (magit-section-ident-value (magit-current-section))))
    (godot-rc--open-inspector node-id)))


(defun godot-rc--open-inspector (object-id)
  (godot-rc--get-node-properties
   object-id
   (lambda (data)
     (with-current-buffer (get-buffer-create "*inspector*")
       (inspector-mode)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (godot-rc--inspector-insert-sections data))
       (pop-to-buffer (current-buffer))))))

(defun godot-rc--inspector-insert-sections (data)
  (magit-insert-section (magit-section "inspector-root")
    (dolist (d data) (godot-rc--inspector-insert-category-section d))))


(defun godot-rc--inspector-insert-section (data)
  (let* ((visible-name (gethash "visible_name" data))
         (children (gethash "children" data)))
    (when children (godot-rc--inspector-insert-grouping-section data))
    (when visible-name (godot-rc--inspector-insert-property-section data))))

(defun godot-rc--inspector-insert-category-section (data)
  (let* ((name (gethash "name" data))
         (children (gethash "children" data)))
    (magit-insert-section (magit-section `(:type 'category))
      (magit-insert-heading
        (make-string (* 2 (- (godot-rc--magit-section-depth magit-insert-section--current) 1)) ?\s)
        (let ((icon-path (f-join godot-rc--base-dir "godot-icons" (concat name ".svg"))))
          (if (file-exists-p icon-path)
              (propertize "@" 'display (create-image icon-path nil nil :ascent 'center))))
        name)

      (when children
        (dolist (child children) (godot-rc--inspector-insert-section child))))))


(defun godot-rc--inspector-insert-grouping-section (data)
  (let* ((name (gethash "name" data))
         (children (gethash "children" data)))
    (magit-insert-section (magit-section `(:type 'grouping))
      :hide t
      (magit-insert-heading
        (make-string (* 2 (- (godot-rc--magit-section-depth magit-insert-section--current) 1)) ?\s)
        name)

      (when children
        (dolist (child children) (godot-rc--inspector-insert-section child))))))

(defun godot-rc--inspector-insert-property-section (data)
  (let* ((_property-name (gethash "property" data))
         (type (gethash "type" data)))
    (pcase type
      ((guard (eq type godot-rc--variant-type-bool)) (godot-rc--inspector-insert-bool-property data))
      (_ (godot-rc--inspector-insert-unsupported-property data)))))

(defun godot-rc--inspector-insert-bool-property (data)
  (let ((visible-name (gethash "visible_name" data)))
    (magit-insert-section-body
      (widget-insert (make-string (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) ?\s))
      (widget-insert visible-name)
      (widget-insert " ")
      (widget-create 'checkbox)
      (widget-insert "\n"))))

(defun godot-rc--inspector-insert-unsupported-property (data)
  (let ((visible-name (gethash "visible_name" data)))
    (magit-insert-section-body
      (widget-insert (make-string (* 2 (godot-rc--magit-section-depth magit-insert-section--current)) ?\s))
      (widget-insert visible-name)
      (widget-insert " UNSUPPORTED\n"))))

(provide 'godot-rc-inspector)
;;; godot-rc-inspector.el ends here
