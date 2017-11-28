;;; mobile-suit.el --- startup configuration
;;
;;; Commentary:
;;
;;; Configures the mobile-suit environment.
;;
;;; Code:
(require 'tramp)

(defvar mobile-suit/images '()
  "Images spun up with Emacs.")

(defvar mobile-suit/master-key "/tmp/private/key.asc"
  "Secret key location. Will be deleted after it's imported.")

(defun mobile-suit/-import-master-key ()
  "Imports and deletes GnuPG private key located: `mobile-suit/master-key'."
  (let ((key `,mobile-suit/master-key))
    (when (file-exists-p key)
      (epa-import-keys key)
      (delete-file key))))

(defun mobile-suit/-copy-pub-key-to-images ()
  "Copy SSH pub key needed for `tramp' to `mobile-suite/images'."
  (message "[mobile-suit] copying SSH pub key to images...")
  (let ((passfile "/home/emacs/.password"))
    (with-temp-file passfile
      (insert "password"))
    (dolist (image `,mobile-suit/images)
      (shell-command (format "sshpass -f %s ssh-copy-id %s"
                             passfile image)))
    (delete-file passfile))
  (message "[mobile-suit] ...done"))

(defun mobile-suit/-set-tramp-properties ()
  "Set remote-shell `tramp-connection-properties' for `mobile-suit/images'."
  (message "[mobile-suit] setting TRAMP properties...")
  (dolist (image `,mobile-suit/images)
    (dolist (keypair `(("remote-shell" . "/bin/bash")))
      (add-to-list 'tramp-connection-properties
                   (list (regexp-quote (format "/ssh:%s:" image))
                         (car keypair)
                         (cdr keypair)))))
  (message "[mobile-suit] ...done"))

(defun mobile-suit/-decrypt-ssh-keys ()
  "Decrypt *.gpg encrypted files located in /home/emacs/.ssh directory.

Decrypted file name truncates .gpg extension."
  (message "[mobile-suit] decrypting SSH keys...")
  (dolist (key (directory-files "/home/emacs/.ssh" t "\.gpg"))
    (epa-decrypt-file key (replace-regexp-in-string "\.gpg" "" key)))
  (message "[mobile-suit] ...done"))

(defun mobile-suit/-copy-ssh-keys-to-images ()
  "Copy SSH keys to `mobile-suit/images'."
  (message "[mobile-suit] copying SSH keys to images...")
  (dolist (image `,mobile-suit/images)
    (dolist (key (directory-files "/home/emacs/.ssh" t "[^id]_rsa$"))
      (copy-file key (format "/ssh:%s:/home/emacs/.ssh" image) t)))
  (message "[mobile-suit] ...done"))

(defun mobile-suit/-chown-image-code-volumes ()
  "Change ownership of /home/emacs/code on `mobile-suit/images' to the spacemacser user."
  (message "[mobile-suit] chowning /home/emacs/code on images...")
  (dolist (image `,mobile-suit/images)
    (let ((default-directory (format "/ssh:%s:/home/emacs" image)))
      (shell-command "sudo chown spacemacser:emacs code")))
  (message "[mobile-suit] ...done"))

(defun mobile-suit/-load-custom-lisp-scripts ()
  "Load custom script files."
  (message "[mobile-suit] loading custom /lisp scripts...")
  (let ((custom-scripts (remove "/lisp/mobile-suit.el"
                                (directory-files "/lisp" t "\.el$"))))
    (dolist (script custom-scripts)
      (load-file script)))
  (message "[mobile-suit] ...done"))

(defun mobile-suit/init ()
  "Configure the mobile-suit environment."
  (progn
    (message "[mobile-suit] initializing...")
    (mobile-suit/-import-master-key)
    (mobile-suit/-copy-pub-key-to-images)
    (mobile-suit/-decrypt-ssh-keys)
    (mobile-suit/-copy-ssh-keys-to-images)
    (mobile-suit/-set-tramp-properties)
    (mobile-suit/-chown-image-code-volumes)
    (mobile-suit/-load-custom-lisp-scripts)))

(eval-after-load 'mobile-suit
  (mobile-suit/init))

(provide 'mobile-suit)
;;; mobile-suit.el ends here
