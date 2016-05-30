
(defun epub-mode ()
  (let ((archive buffer-file-name))
    (kill-buffer)
    (epub--show-toc archive)))

(defvar archive-file nil)

(defun epub--show-toc (archive)
  (let* ((ncx-file (epub--locate-ncx archive))
         (ncx (epub--archive-get-xml archive ncx-file))
         (buf (get-buffer-create "*TOC*")))
    (with-current-buffer buf
      (erase-buffer)
      (set (make-local-variable 'archive-file) archive)
      (archive-zip-extract archive ncx-file)
      (epub--pretty-print-xml)
      (goto-char (point-min)))
    (pop-to-buffer-same-window buf)))

(defun epub--pretty-print-xml (&optional begin end)
  (interactive (and (use-region-p) (list (region-beginning) (region-end))))
  (save-excursion
      (nxml-mode)
      (goto-char (or begin (point-min)))
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region (or begin (point-min)) (or end (point-max)))))

(defun epub--locate-rootfile (archive)
  (let* ((container (epub--archive-get-xml archive "META-INF/container.xml"))
         (rootfiles (cdr-safe (assq 'rootfiles container)))
         (rootfile (car-safe (cdr-safe (assq 'rootfile rootfiles))))
         (full-path (cdr-safe (assq 'full-path rootfile)))
         (media-type (cdr-safe (assq 'media-type rootfile))))
    (unless (stringp full-path)
      (error "Unable to locate epub document root file"))
    full-path))

(defun epub--locate-ncx (archive)
  (let* ((rootfile (epub--locate-rootfile archive))
         (dom (epub--archive-get-xml archive rootfile))
         (manifest (cdr (cdr-safe (assq 'manifest dom))))
         (ncx
          (cl-loop for item in manifest
                   when (string= "ncx" (epub--xml-prop 'id item))
                   return item))
         (href (epub--xml-prop 'href ncx)))
    (unless (stringp href)
      (error "Error locating ncx in epub document manifest"))
    (epub--href-relative href rootfile)))

(defun epub--xml-prop (key item)
  (cdr-safe (assq key (car-safe (cdr-safe item)))))

(defun epub--href-relative (name &optional relative-to)
  (concat (or (file-name-directory (or relative-to "")) "") name))

(defun epub--archive-get-xml (archive name &optional relative-to)
  (with-temp-buffer
    (archive-zip-extract archive (epub--href-relative name relative-to))
    (libxml-parse-xml-region (point-min) (point-max))))


(add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-mode))
(add-to-list 'auto-coding-alist '("\\.epub\\'" . no-conversion-multibyte))

(provide 'epub)
