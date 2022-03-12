(let ((generated-autoload-file
       (expand-file-name "hare-autoloads.el")))
  (update-file-autoloads (expand-file-name "hare.el"))
  (set-buffer (get-file-buffer generated-autoload-file))
  (save-buffer 0))
