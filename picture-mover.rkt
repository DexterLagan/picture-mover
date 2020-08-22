#lang racket/gui

;;; purpose

; 1) make a list of all folders in d:\pictures
; 2) for each folder,
;    a) if no Originals folder, create it
;    b) move all files in ./Originals

;;; consts

(define *version* "1.4")
(define *app-name* (string-append "Picture Mover v" *version*))
(define *config-filename* "picture-mover.conf")

;;; versions

; v1.4 - added support for reading search path from config file;
; v1.3 - added support for Windows Camera Roll and folder selection;
; v1.2 - added support for automatically renaming duplicate folders;
; v1.1 - improved error handling;
; v1.0 - initial release, locked to d:\pictures.

;;; defs

;; generic message box
(define (msg-box msg)
  (void (message-box *app-name* (string-append msg " "))))

;; generic confirmation dialog
(define (confirm? msg)
  (message-box *app-name* (string-append msg " ") #f (list 'yes-no)))

;; creates an 'Originals' folder inside a path if it doesn't exist
(define (create-originals-folder path)
  (let ((originals-folder (build-path path "Originals")))
    (when (not (directory-exists? originals-folder))
      (make-directory originals-folder))))

;; moves all files in a folder to its 'Originals' sub-folder
(define (move-to-originals path)
  (let* ((originals-folder (build-path path "Originals"))
         (root-files (directory-list path))
         (move-to-orig (λ (f) (when (not (string-contains? (path->string f) "Originals"))
                                (rename-file-or-directory (build-path path f) (build-path originals-folder f))))))
    (when (directory-exists? originals-folder)
      (for-each move-to-orig root-files))))

;; process a picture folder :
;; - make sure no Originals folder exists prior;
;; - create an Originals folder;
;; - move all files to Originals folder"
(define (move-pictures-to-originals path)
  (when (directory-exists? path)
    (let ((originals-folder (build-path path "Originals")))
      (when (not (directory-exists? originals-folder))
        (begin (create-originals-folder path)
               (move-to-originals path))))))

;; rename a folder so that the prefix date follows the DDMMYYYY format
(define (rename-folder path)
  (when (directory-exists? path)
    (let* ((remove-dashes-from-first (λ args (string-replace (car args) "-" "")))
           (dashed-date-pattern #px"\\d{4}\\-(0?[1-9]|1[012])\\-(0?[1-9]|[12][0-9]|3[01])*")
           (path-string (path->string path))
           (new-path (regexp-replace dashed-date-pattern
                                     path-string
                                     remove-dashes-from-first)))
      ; quick and dirty way to automatically rename folder if date exists
      (when (not (string=? (path->string path) new-path))
        (when (directory-exists? new-path)
          (set! new-path (string-append new-path "a")))
        (when (directory-exists? new-path)
          (set! new-path (string-append new-path "b")))
        (when (directory-exists? new-path)
          (set! new-path (string-append new-path "c")))
        (when (directory-exists? new-path)
          (set! new-path (string-append new-path "d")))
        (rename-file-or-directory path new-path)))))

;;; main

; read search path from config file
(define search-path (file->string *config-filename*))
(unless search-path
  (msg-box "Config file not found.")
  (exit 0))

(unless (directory-exists? search-path)
  (msg-box "Search path folder does not exist.")
  (exit 0))

(define pic-folders (map (λ (path) (build-path search-path path))
                         (directory-list search-path)))

; move files to originals folders
(for-each move-pictures-to-originals pic-folders)

; apply folder date naming convention
(for-each rename-folder pic-folders)

(msg-box "All done!  ")


; EOF
