;;; -*- lexical-binding: t -*-
(require 'shr)
(require 'arc-mode)

(defun epub-mode ()
  (interactive)
  (let* ((archive buffer-file-name)
         (name (file-name-sans-extension (file-name-nondirectory archive)))
         (toc (format "Table of Contents (%s)" name)))
    (unless epub--debug
      (kill-buffer))
    (epub--show-toc archive toc)))

(defvar epub--debug nil)

(defvar epub--archive-cache nil
  "Contains xml and page content cache as alist in format
((ARCHIVE-PATH . ((CONTENT-PATH . VALUE)
                  ...))
 ...)")

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
  (let* ((rootfile (epub--locate-rootfile archive))
         (ncx-file (epub--locate-ncx archive rootfile))
         (ncx-path (file-name-directory (or ncx-file "")))
         (ncx (epub--archive-get-xml archive ncx-file))
         (title (caddr-safe (epub--xml-node ncx 'docTitle 'text)))
         (navmap (epub--xml-node ncx 'navMap))
         (buf (get-buffer-create (or buffer "TOC")))
         start)
    (pop-to-buffer-same-window buf)
    (indented-text-mode)
    (erase-buffer)
    (insert title "\n\n")
    (epub--insert-navmap navmap archive ncx-path)
    (insert "\n\n")
    (if epub--debug (epub--insert-xml archive ncx-file))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq archive-file archive)))

(defun epub--insert-xml (archive name &optional no-pretty-print)
  (let ((start (point-marker)))
    (when epub--debug
      (message "Extracting %S, looking for %S" archive name))
    (archive-zip-extract archive name)
    (decode-coding-inserted-region start (point) name)
    (unless no-pretty-print
      (epub--pretty-print-xml start (point)))))

(defun epub--create-navpoint-handler (archive node-path buffer)
  (let* ((split-node (split-string node-path "#"))
         (arc-path (car split-node))
         (html-path (cadr split-node)))
    (lambda (unused-param)
      (switch-to-buffer (get-buffer-create buffer))
      (let ((dom (epub--archive-get-dom archive arc-path)))
        (erase-buffer)
        (shr-insert-document dom)
        (goto-char (point-min)))
      ;;  (let ((rendered (epub--archive-get-rendered-page archive arc-path)))
      ;;    (erase-buffer)
      ;;    (insert rendered)
      ;;    (goto-char (point-min)))
      )))


;;; BIG UGLY HACK - REDEFINING shr-image-fetched
(defvar shr-image-fetched-original
  (symbol-function 'shr-image-fetched))

(defun shr-image-fetched (status buffer start end &optional flags)
  ;; For some reason shr-image-fetched expects to be in the beginnig of
  ;; the buffer containing image, and isn't. So let's move it manually
  (goto-char (point-min))
  (funcall shr-image-fetched-original
           status buffer start end flags))

(defun epub--insert-navpoint (navpoint ncx-path archive &optional ident-str)
  (let ((navpoint-content (epub--xml-prop (epub--xml-node navpoint 'content) 'src))
        (text (caddr-safe (epub--xml-node navpoint 'navLabel 'text)))
        (point-start (point)))
    (when ident-str (insert ident-str)) 
    (insert text)
    (make-text-button
     point-start (point)
     'action (epub--create-navpoint-handler
              archive (concat ncx-path navpoint-content) text)
     'follow-link t)                                               
    (insert "\n")))

(defun epub--insert-navmap (navmap archive ncx-path &optional ident-str)
  (cl-loop for navpoint in navmap
           when (epub--xml-node navpoint 'navLabel 'text)
           do
           (epub--insert-navpoint navpoint ncx-path archive ident-str)
           (epub--insert-navmap navpoint archive ncx-path (concat ident-str "  "))))

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

(defun epub--locate-ncx (archive rootfile)
  (let* ((dom (epub--archive-get-xml archive rootfile))
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

(defun epub--fill-cache (archive name value)
  (push (cons name value)
        (cdr (assoc archive epub--archive-cache)))
  value)

(defun epub--get-cached(archive name &optional cache-fill-func)
  (let ((arc-cache (cdr (assoc archive epub--archive-cache))))
    (unless arc-cache
      (push (cons archive nil)
            epub--archive-cache))
    (let ((cache (cdr (assoc name arc-cache))))
      (when (and (not cache)
                 cache-fill-func)
        (setq cache (funcall cache-fill-func archive name)))
      cache)))

(defun epub--fill-dom-cache (archive name)
  (let ((dom-cache (with-temp-buffer
                     (archive-zip-extract archive name)
                     (epub--convert-links archive
                                          (libxml-parse-html-region (point-min)
                                                                    (point-max))))))
    (epub--fill-cache archive name dom-cache)))

(defun epub--archive-get-dom (archive name)
  (epub--get-cached archive
                    name
                    #'epub--fill-dom-cache))

(defun epub--map-alist (alist mapper)
  "Maps ALIST entries with MAPPER function. 
Mapper must accept a cons cell and return updated cons cell"
  (cond ((not (listp alist)) alist) ;; atom
        ((not (listp (cdr alist))) (funcall mapper alist)) ;; flat alist entry
        (t (mapcar (lambda (x) (epub--map-alist x mapper)) ;; nested alists
                   alist))))

(defun epub--extract-link (archive link)
  "From ARCHIVE, extract file at relative path LINK and return url for extracted"
  (let ((tmp-file (make-temp-file "epub-src"
                                  nil
                                  (file-name-extension link t))))
    (with-temp-file tmp-file
      (archive-zip-extract archive link)
      (goto-char (point-min))
      (concat "file://" tmp-file))))

(defun epub--convert-links (archive dom)
  (epub--map-alist dom
                   (lambda (x)
                     (if (eq (car x) 'src)
                         (let ((result-link
                                (epub--extract-link archive (cdr x))))
                           (cons 'src result-link))
                       x))))

(defun epub--fill-render-cache (archive name)
  (let ((rendered
         (with-temp-buffer
           (when epub--debug
             (message "Extracting %S, looking for %S" archive name))
           (archive-zip-extract archive name)
           (let ((dom
                  (epub--convert-links archive
                                       (libxml-parse-html-region (point-min)
                                                                 (point-max)))))
             (erase-buffer)
             (shr-insert-document dom)
             (buffer-substring (point-min) (point-max))))))
    (epub--fill-cache archive name rendered)))

(defun epub--archive-get-rendered-page (archive name)
  (epub--get-cached archive
                    name
                    #'epub--fill-render-cache))

(defun epub--fill-xml-cache (archive name)
  (let ((xml-cache (with-temp-buffer
                     (epub--insert-xml archive name t)
                     (libxml-parse-xml-region (point-min) (point-max)))))
    (epub--fill-cache archive name xml-cache)))

(defun epub--archive-get-xml (archive name)
  (epub--get-cached archive
                    name
                    #'epub--fill-xml-cache))

(unless (fboundp 'libxml-parse-html-region)
  (error "epub.el requires Emacs to be compiled with libxml2"))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-mode))
(add-to-list 'auto-coding-alist '("\\.epub\\'" . no-conversion-multibyte))

(provide 'epub)
