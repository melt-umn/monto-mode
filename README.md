# monto-mode

A [Monto](https://github.com/monto-editor) major mode for emacs.

## Dependencies

Requires the [request.el](https://github.com/skeeto/elisp-ffi) library

## Installation

### Install request.el

```
git clone https://github.com/tkf/emacs-request.git ~/.emacs.d/request.el
echo "(add-to-list 'load-path \"~/.emacs.d/request.el\")" >> ~/.emacs
```

### Install monto-mode

```
git clone https://github.com/melt-umn/monto-mode.git ~/.emacs.d/monto-mode
echo "(add-to-list 'load-path \"~/.emacs.d/monto-mode\")" >> ~/.emacs
echo "(require 'monto-mode)" >> ~/.emacs
```

### Configuration

By default, monto-mode doesn't do much.
You need to set up styles and language associations in your `.emacs`.
See the `example-dot-emacs-file.el` for more information.

## Usage

`monto-mode` should automatically activate for any files with appropriate extensions.

## Caveats

 - This is only tested on x86_64 Linux. macOS users, you've been warned.
 - This was written in Vim by a Vim user. Bug reports welcomed.
