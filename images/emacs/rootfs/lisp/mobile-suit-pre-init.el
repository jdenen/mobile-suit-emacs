;;; mobile-suit-pre-init.el --- executed before `mobile-suit/-init'
(dolist  (file (directory-files "/tmp/private/.ssh" nil "^[^\.]"))
  (let ((source (format "/tmp/private/.ssh/%s" file))
        (destination (format "/home/emacs/.ssh/%s" file)))
    (copy-file source destination t)))
;;; mobile-suit-pre-init.el ends here
