(setq-default
 mode-line-format
 `(
   ""
   ;w32-ime-mode-line-state-indicator
   " "
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (line-number-mode
    (:eval
     (format "L%%l/L%d " (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   global-mode-string
   (-3 . "%p")
   (which-func-mode ("" which-func-format " "))
   )
 )
(setq mode-line-frame-identification " ")
