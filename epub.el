
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
         (title (caddr-safe (epub--xml-node ncx 'docTitle 'text)))
         (navmap (epub--xml-node ncx 'navMap))
         (buf (get-buffer-create (or buffer "TOC")))
         start)
    (pop-to-buffer-same-window buf)
    (indented-text-mode)
    (erase-buffer)
    (insert title "\n\n")
    (epub--insert-navmap navmap)
    (insert "\n\n")
    (if epub--debug (epub--insert-xml archive ncx-file))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq archive-file archive)))

(defun epub--insert-xml (archive name &optional no-pretty-print)
  (let ((start (point-marker)))
    (archive-zip-extract archive name)
    (decode-coding-inserted-region start (point) name)
    (unless no-pretty-print
      (epub--pretty-print-xml start (point)))))

(defun epub--insert-navpoint (navpoint text)
  ;; TODO: Turn these into buttons that link to chapters
  (insert text "\n"))

(defun epub--insert-navmap (navmap)
  (cl-loop for navpoint in navmap
           when (epub--xml-node navpoint 'navLabel 'text)
           do (epub--insert-navpoint navpoint (caddr-safe it))))

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
         (rootfile (epub--xml-node container 'rootfiles 'rootfile))
         (full-path (epub--xml-prop rootfile 'full-path)))
    (unless (stringp full-path)
      (error "Unable to locate epub document root file"))
    full-path))

(defun epub--locate-ncx (archive)
  (let* ((rootfile (epub--locate-rootfile archive))
         (dom (epub--archive-get-xml archive rootfile))
         (manifest (epub--xml-node dom 'manifest))
         (ncx
          (cl-loop for item in (cddr-safe manifest)
                   when (string= "ncx" (epub--xml-prop item 'id))
                   return item))
         (href (epub--xml-prop ncx 'href)))
    (unless (stringp href)
      (error "Error locating ncx in epub document manifest"))
    (epub--href-relative href rootfile)))

(defun epub--xml-node (item &rest keys)
  (unless (> (length keys) 0)
    (error "Insufficient number of keys"))
  (let ((node item))
    (cl-loop for key in keys
             if (assq key (cddr-safe node))
             do (setq node it)
             else return nil
             finally return node)))

(defun epub--xml-prop (item key)
  (cdr-safe (assq key (cadr-safe item))))

(defun epub--href-relative (name &optional relative-to)
  (concat (or (file-name-directory (or relative-to "")) "") name))

(defun epub--archive-get-xml (archive name &optional relative-to)
  (with-temp-buffer
    (epub--insert-xml archive name t)
    (libxml-parse-xml-region (point-min) (point-max))))


(add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-mode))
(add-to-list 'auto-coding-alist '("\\.epub\\'" . no-conversion-multibyte))

(provide 'epub)
