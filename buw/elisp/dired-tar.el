; https://gist.github.com/KeenS/6828197/raw/2647435eac8e4b466e19daa14b1bc6df74bf1238/dired-tar.el
(defun dired-tar (tarname files &optional arg)
  "A dired-mode extension to archive files marked. With prefix argument, the tarball is gziped."
  (interactive (let ((files (dired-get-marked-files)))
		 (list (read-string "Tarball name: " (concat (file-relative-name (car files)) ".tar.gz"))
		       files "P")))
  (let ((tar (if arg
		 (if dired-guess-shell-gnutar
		     (concat dired-guess-shell-gnutar " zcf %s %s")
		   "tar cf - %2s | gzip > %1s")
	       "tar cf %s %s")))
    (shell-command (format tar tarname (mapconcat 'file-relative-name files " ")))))
;(add-hook 'dired-load-hook (lambda () (define-key dired-mode-map "T" 'dired-tar)))
