; Add monto-mode to the load path.
(add-to-list 'load-path "~/.emacs.d/monto-mode")

; Load Monto Mode.
(require 'monto-mode)

; Set the URL to connect to the broker at.
(setq monto-broker-url "http://localhost:28888")

; Set up Monto fonts.
(def-monto-highlighting-styles
  (comment
    :foreground "grey")
  (constant
    :foreground "red")
  (identifier
    :foreground "purple")
  (keyword
    :foreground "orange")
  (literal
    :foreground "blue")
  (operator
    :foreground "green"))

; Set up Monto languages.
(def-monto-languages
  ("dcv2"   "dcv2")
  ("silver" "sv"))
