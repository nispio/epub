
(require 'arc-mode)

(defun epub-mode ()
  (let* ((archive buffer-file-name)
         (name (file-name-sans-extension (file-name-nondirectory archive)))
         (toc (format "Table of Contents (%s)" name)))
    (kill-buffer)
    (epub--show-toc archive toc)))

(defvar epub--debug nil)

(defvar-local archive-file nil)

(defsubst cadr-safe (x)
  "Return the safe car of the cdr of X."
  (car-safe (cdr-safe x)))

(defsubst cddr-safe (x)
  "Return the safe cdr of the cdr of X."
  (cdr-safe (cdr-safe x)))

(defsubst caddr-safe (x)
  "Return the safe car of the cdr of the cdr of X."
  (car-safe (cdr-safe (cdr-safe x))))

(defun epub--show-toc (archive &optional buffer)
  (let* ((ncx-file (epub--locate-ncx archive))
         (ncx (epub--archive-get-xml archive ncx-file))
         (title (caddr-safe (assq 'text (assq 'docTitle ncx))))
         (navmap (cddr-safe (assq 'navMap ncx)))
         (buf (get-buffer-create (or buffer "TOC"))))
    (pop-to-buffer-same-window buf)
    (indented-text-mode)
    (erase-buffer)
    (insert title "\n\n")
    (epub--insert-navmap navmap)
    (insert "\n\n")
    (when epub--debug
        (archive-zip-extract archive ncx-file)
        (epub--pretty-print-xml))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq archive-file archive)))a

(defun epub--insert-navmap (navmap)
  (cl-loop for navpoint in navmap
           when (caddr-safe (assq 'text (assq 'navLabel navpoint)))
           do (insert it "\n")))

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
