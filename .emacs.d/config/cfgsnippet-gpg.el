;;
;; gpg.el from Oort Gnus
;;
(require 'gpg)
;; Your passphrase timeout. Set it to zero if Emacs
;; shouldn't store your passphrase
(setq gpg-passphrase-timeout 120)
(setq gpg-temp-directory "~/.gnupg/tmp/")
;; Your default key for signing emails.
(setq gpg-default-key-id "01FD5B62")
