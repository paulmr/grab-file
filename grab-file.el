(defvar grab-file-list (make-hash-table :test 'equal)
    "The hash table that matches file-names to paths")

(defvar grab-file-history (list)
    "History for grab-file input")

(defun grab-file-add (path &optional fname)
    (unless fname (setq fname (file-name-nondirectory path)))
    (puthash fname path grab-file-list)
    (message "Grab file cache added: %s" fname))

(defun grab-clear ()
    (interactive)
    (clrhash grab-file-list)
    (message "Grab file list cleared"))

(defun grab-file-remove (fname)
    (interactive (list (grab-read-completing)))
    (remhash fname grab-file-list))

(defun grab-add-buffer-file (buffname)
    (interactive "bBuffer: ")
    (let ((path (buffer-file-name (get-buffer buffname))))
        (grab-file-add path)))

(defun grab-file-lookup (fname)
    (gethash fname grab-file-list 'nil))

(defun grab-file-get-keys ()
    (let (keys)
        (maphash (lambda (key value) (setq keys (cons key keys)))
            grab-file-list)
        keys))

(defun grab-read-completing ()
    (ido-completing-read
        "File name: " (grab-file-get-keys) nil t nil grab-file-history nil))

(defun grab-file (fname)
    (interactive (list (grab-read-completing)))
    (let ((path (grab-file-lookup fname)))
        (if path (find-file path)
            (error "Not found: %s" fname))))

(defun grab-file-add-file-list-from-buffer (buff)
    (with-current-buffer buff
        (mapc (lambda (line) (grab-file-add line))
            (split-string (buffer-string) "\n"))))

(defun grab-file-find-directory (dontremove)
  (interactive "P")
  (unless dontremove (grab-clear))
  (let* ((dirname
           (expand-file-name (read-directory-name "Dir to search: " nil nil t)))
          (findcmd 
            (read-string "Run find like this: " (format "find %s -type f" dirname))))
    (with-temp-buffer
      (shell-command findcmd t)
      (grab-file-add-file-list-from-buffer (current-buffer)))))

(provide 'grab-file)
