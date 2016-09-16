(defun xah-lookup-weblio (&optional φword)
  "Lookup definition of current word or text selection in URL `http://www.weblio.jp/'"
  (interactive)
  (xah-lookup-word-on-internet
   φword
   "http://www.weblio.jp/content/�"))
