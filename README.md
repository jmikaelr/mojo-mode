# Mojo Mode for Emacs

🚧 **WIP — Work in Progress** 🚧

Syntax highlighting and Python-style indentation for the [Mojo](https://www.modular.com/mojo) programming language by Modular.

## Features

- Syntax highlighting for keywords, types, and constants
- Python-like indentation rules
- Imenu support for functions and structs
- Automatic `.mojo` file association

## LSP integration — TODO

Planned:
- Discover `mojo-lsp-server` inside a project’s `.pixi/envs/*/bin`
- Fall back to `PATH` if not found
- Autostart via `lsp-mode` when opening `.mojo` files

## Install

### Spacemacs

1. Place `mojo-mode.el` at:  
   ```
   ~/.emacs.d/private/local/mojo-mode.el
   ```
2. In your `~/.spacemacs`, inside `dotspacemacs/user-config` add:
   ```elisp
   (load "~/.emacs.d/private/local/mojo-mode.el")
   ```
3. Reload config with `SPC f e R`.

### Vanilla Emacs

Add this to your init file:
```elisp
(add-to-list 'load-path "~/.emacs.d/private/local")
(require 'mojo-mode)
```

## Usage

- Open any `.mojo` file to activate the mode
- Use Imenu (`SPC j i` in Spacemacs, or `M-x imenu`) to jump between functions/structs

## License

MIT
