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

(defmacro mobile-suit/with-message (msg &rest body)
  "`message' MSG before executing BODY."
  (declare (indent defun))
  (let ((fmsg (format "[mobile-suit] %s..." msg)))
    `(progn
       (message ,fmsg)
       ,@body
       (message "[mobile-suit] ...done"))))

(defun mobile-suit/-import-master-key ()
  "Imports and deletes GnuPG private key located: `mobile-suit/master-key'."
  (let ((key `,mobile-suit/master-key))
    (when (file-exists-p key)
      (epa-import-keys key)
      (delete-file key))))

(defun mobile-suit/-disable-image-hostkey-checking ()
  "Disable StrictHostKeyChecking for `mobile-suit/images'."
  (mobile-suit/with-message "disabling StrictHostKeyChecking for images"
    (with-temp-buffer
      (dolist (image `,mobile-suit/images)
        (insert (mapconcat (lambda (s) (format s image))
                           '("\n" "Host %s" "  StrictHostKeyChecking no")
                           "\n")))
      (with-current-buffer (current-buffer)
        (append-to-file nil nil "/home/emacs/.ssh/config")))))

(defun mobile-suit/-copy-pub-key-to-images ()
  "Copy SSH pub key needed for `tramp' to `mobile-suite/images'."
  (mobile-suit/with-message "copying SSH pub key to images"
    (let ((passfile "/home/emacs/.password"))
      (with-temp-file passfile
        (insert "password"))
      (dolist (image `,mobile-suit/images)
        (shell-command (format "sshpass -f %s ssh-copy-id %s"
                               passfile image)))
      (delete-file passfile))))

(defun mobile-suit/-set-tramp-properties ()
  "Set remote-shell `tramp-connection-properties' for `mobile-suit/images'."
 (mobile-suit/with-message "setting TRAMP properties"
   (dolist (image `,mobile-suit/images)
     (dolist (keypair `(("remote-shell" . "/bin/bash")))
       (add-to-list 'tramp-connection-properties
                    (list (regexp-quote (format "/ssh:%s:" image))
                          (car keypair)
                          (cdr keypair)))))))

(defun mobile-suit/-decrypt-ssh-keys ()
  "Decrypt *.gpg encrypted files located in /home/emacs/.ssh directory.

Decrypted file name truncates .gpg extension."
  (mobile-suit/with-message "decrypting SSH keys"
    (dolist (key (directory-files "/home/emacs/.ssh" t "\.gpg"))
      (epa-decrypt-file key (replace-regexp-in-string "\.gpg" "" key)))))

(defun mobile-suit/-copy-ssh-keys-to-images ()
  "Copy SSH keys to `mobile-suit/images'."
  (mobile-suit/with-message "copying SSH keys to images"
    (dolist (image `,mobile-suit/images)
      (dolist (key (directory-files "/home/emacs/.ssh" t "[^id]_rsa$"))
        (copy-file key (format "/ssh:%s:/home/emacs/.ssh" image) t)))))

(defun mobile-suit/-chown-image-code-volumes ()
  "Change ownership of /home/emacs/code on `mobile-suit/images' to the spacemacser user."
  (mobile-suit/with-message "chowning /home/emacs/code on images"
    (dolist (image `,mobile-suit/images)
      (let ((default-directory (format "/ssh:%s:/home/emacs" image)))
        (shell-command "sudo chown spacemacser:emacs code")))))

(defun mobile-suit/-pre-init ()
  "Load custom script before `mobile-suit/-init' execution."
  (mobile-suit/with-message "loading pre-init script"
    (load-file "/lisp/mobile-suit-pre-init.el")))

(defun mobile-suit/-post-init ()
  "Load custom script after `mobile-suit/-init' execution."
  (mobile-suit/with-message "loading post-init script"
    (load-file "/lisp/mobile-suit-post-init.el")))

(defun mobile-suit/-init ()
  "Configure the mobile-suit environment."
  (mobile-suit/-import-master-key)
  (mobile-suit/-disable-image-hostkey-checking)
  (mobile-suit/-copy-pub-key-to-images)
  (mobile-suit/-decrypt-ssh-keys)
  (mobile-suit/-copy-ssh-keys-to-images)
  (mobile-suit/-set-tramp-properties)
  (mobile-suit/-chown-image-code-volumes))

(eval-after-load 'mobile-suit
  (progn
    (mobile-suit/-pre-init)
    (mobile-suit/-init)
    (mobile-suit/-post-init)))

(provide 'mobile-suit)
;;; mobile-suit.el ends here
