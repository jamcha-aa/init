; for configuration of mew
; http://blog.livedoor.jp/dreamcraft/archives/47026603.html
; edit username and IamUser

;Load stunnel
(setq mew-prog-ssl "/usr/bin/stunnel")
; User IMAP with Gmail for receiving a mail
(setq mew-proto "%")
(setq mew-imap-server "imap.gmail.com")
(setq mew-imap-ssl-port "993")
(setq mew-imap-user "username@gmail.com")
(setq mew-imap-auth  t)
(setq mew-imap-ssl t)
; User SMTP with Gmail for sending a mail
(setq mew-smtp-server "smtp.gmail.com")
(setq mew-smtp-ssl-port "465")
(setq mew-smtp-user "username@gmail.com")
(setq mew-smtp-auth t)
(setq mew-smtp-ssl t)

(setq mew-fcc "%Sent")
(setq mew-imap-trash-folder "%[Gmail]/All Mail")
(setq mew-use-cached-passwd t)

(setq mew-name "IamUser")
(setq mew-user "username")
(setq mew-mail-domain "gmail.com")
