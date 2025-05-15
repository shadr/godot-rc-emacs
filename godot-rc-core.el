;;; godot-rc-core.el -*- lexical-binding: t; -*-

;;; Code:

(require 'websocket)

(defvar godot-rc--socket nil)
(defvar godot-rc--response-id 0)
(defvar godot-rc--response-callbacks (make-hash-table))
(defvar godot-rc--notification-handlers (make-hash-table :test 'equal))

(defun godot-rc-is-socket-alive ()
  (and
   (not (eq godot-rc--socket nil))
   (eq (websocket-ready-state godot-rc--socket) 'open)))

(defun godot-rc-start ()
  (setq godot-rc--socket (websocket-open "ws://127.0.0.1:6500" :on-message (lambda (_ws frame) (godot-rc--on-message (websocket-frame-text frame))))))

(defun godot-rc-stop ()
  (interactive)
  (websocket-close godot-rc--socket))

(defun godot-rc-check-server ()
  (if (not (godot-rc-is-socket-alive)) (godot-rc-start)))

(defun godot-rc-send-text (text)
  (godot-rc-check-server)
  (websocket-send-text godot-rc--socket text))

(defun godot-rc-request (method &optional params)
  (godot-rc-send-text (json-encode `(:method ,method :params ,params))))

(defun godot-rc-request-callback (method params callback)
  (godot-rc-send-text (json-encode `(:method ,method :params ,params :id ,godot-rc--response-id)))
  (puthash godot-rc--response-id callback godot-rc--response-callbacks)
  (cl-incf godot-rc--response-id))

(defun godot-rc--on-message (text)
  (let* ((parsed (json-parse-string text :array-type 'list))
         (id (gethash "id" parsed))
         (method (gethash "method" parsed)))
    (when method (godot-rc--on-notification parsed))
    (when id (godot-rc--on-response parsed))))

(defun godot-rc--on-notification (parsed)
  (let* ((name (gethash "method" parsed))
         (handler (gethash name godot-rc--notification-handlers))
         (params (gethash "params" parsed nil)))
    (when handler (funcall handler params))))

(defun godot-rc--on-response (parsed)
  (let* ((result (gethash "result" parsed))
         (id (gethash "id" parsed)))
    (when (and result id)
      (if-let ((callback (gethash id godot-rc--response-callbacks)))
          (funcall callback result)))
    (remhash id godot-rc--response-callbacks)))

(defun godot-rc-add-notification-handler (name handler)
  (puthash name handler godot-rc--notification-handlers))

(provide 'godot-rc-core)

;;; godot-rc-core.el ends here
