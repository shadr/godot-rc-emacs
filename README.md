# Overview
`godot-rc-emacs` connects to a [godot-rc](https://github.com/shadr/godot-rc) and allows you to modify scene tree and nodes properties without leaving Emacs.

# DISCLAIMER
Package is in early stages of development, use with caution.

Also this is my first Emacs package, bugs are expected, QoL is non-existent, code goes against every guideline and convention.

# Installation
## Requirements
Ensure that you have following packages:

- [websocket](https://github.com/ahyatt/emacs-websocket) 
- [magit-section](https://elpa.nongnu.org/nongnu/magit-section.html)
- [gdscript-mode](https://github.com/godotengine/emacs-gdscript-mode)
- [f](https://github.com/rejeep/f.el)

Optional packages:

- [evil](https://github.com/emacs-evil/evil) - if you have evil installed, then godot-rc will add bindings for evil normal state

## Installation

1) clone package somewhere

`git clone https://github.com/shadr/godot-rc-emacs.git ~/godot-rc-emacs`

2) add godot-rc directory to the load-path of Emacs and load package

``` emacs-lisp
(add-to-list 'load-path "~/godot-rc-emacs/")
(require 'godot-rc)
```

3) currently godot-rc does not provide default bindings for emacs (only for evil), so you will need to bind necessary functions yourself

``` emacs-lisp
(define-key tscn-mode-map (kbd "C-m o") #'godot-rc-open-scene)
(define-key tscn-mode-map (kbd "C-m r") #'godot-rc-tscn-rename-node)
(define-key tscn-mode-map (kbd "C-m c") #'godot-rc-tscn-change-node-type)
;; etc
```

