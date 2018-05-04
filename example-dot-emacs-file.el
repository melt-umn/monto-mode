; Add monto-mode to the load path.
(add-to-list 'load-path "~/.emacs.d/monto-mode")

; Load Monto Mode.
(require 'monto-mode)

; Set the URL to connect to the broker at.
(setq monto-broker-url "http://localhost:28888")

; Set up Monto fonts.
(def-monto-highlighting-styles
  (comment     :foreground "grey")
  (function    :foreground "red")
  (identifier  :foreground "purple")
  (keyword     :foreground "orange")
  (literal     :foreground "blue")
  (operator    :foreground "green")
  (punctuation :foreground "white")
  (type        :foreground "yellow")
  ( 0 :foreground "black")
  ( 1 :foreground "red")
  ( 2 :foreground "green")
  ( 3 :foreground "yellow")
  ( 4 :foreground "blue")
  ( 5 :foreground "magenta")
  ( 6 :foreground "cyan")
  ( 7 :foreground "white")
  ( 8 :foreground "black")
  ( 9 :foreground "red")
  (10 :foreground "green")
  (11 :foreground "yellow")
  (12 :foreground "blue")
  (13 :foreground "magenta")
  (14 :foreground "cyan")
  (15 :foreground "white"))

; Set up Monto languages.
(def-monto-languages
  ("dcv2"   "dcv2")
  ("silver" "sv"))
