# monto-mode

A [Monto](https://github.com/monto-editor) major mode for emacs.

## Dependencies

Requires the [elisp-ffi](https://github.com/skeeto/elisp-ffi) library, which in
turn requires [libffi](https://sourceware.org/libffi/).

## Installation

### Install libffi

On Linux/BSD, libffi should be available from your package manager.
On macOS, libffi is available through [Homebrew](https://brew.sh).

### Install elisp-ffi

```
git clone https://github.com/skeeto/elisp-ffi.git ~/.emacs.d/elisp-ffi
make ffi-glue test -C ~/.emacs.d/elisp-ffi
echo "(add-to-list 'load-path \"~/.emacs.d/elisp-ffi\")" >> ~/.emacs
```

### Install monto-mode

```
git clone https://github.com/melt-umn/monto-mode.git ~/.emacs.d/monto-mode
echo "(add-to-list 'load-path \"~/.emacs.d/monto-mode\")" >> ~/.emacs
echo "(require 'monto-mode)" >> ~/.emacs
```

### Configuration

By default, monto-mode doesn't do much. You need to set up styles and
language associations in your `.emacs`. See the `example-dot-emacs-file.el`
for more information.

## Usage

`monto-mode` should automatically activate for any files with appropriate
extensions.

## Caveats

 - This is only tested on x86_64 Linux. macOS users, you've been warned.
 - This was written in Vim by a Vim user. Bug reports welcomed.
