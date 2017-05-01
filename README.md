# monto-editor-emacs

A [Monto](https://github.com/monto-editor) client for emacs.

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

### Install monto-emacs

```
git clone git@github.umn.edu:melt/monto-editor-emacs.git ~/.emacs.d/monto-editor-emacs
echo "(add-to-list 'load-path \"~/.emacs.d/monto-editor-emacs\")" >> ~/.emacs
```

## Usage

Just do `M-x monto-mode`.
