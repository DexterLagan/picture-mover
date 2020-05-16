#lang racket/gui       ; use minimal racket
(require racket/string)
(define appname "Picture Importer")

;;; purpose
; 1) make a list of all folders in d:\pictures
; 2) for each folder,
;    a) if no Originals folder, create it
;    b) move all files in ./Originals

;;; funcs

(define (create-originals-folder path)
  ; creates an 'Originals' folder inside a path if it doesn't exist
  (let ((originals-folder (build-path path "Originals")))
    (when (not (directory-exists? originals-folder))
      (make-directory originals-folder))))

(define (move-to-originals path)
  ; moves all files in a folder to its 'Originals' sub-folder
  (let* ((originals-folder (build-path path "Originals"))
         (root-files (directory-list path))
         (move-to-orig (λ (f) (when (not (string-contains? (path->string f) "Originals"))
                                (rename-file-or-directory (build-path path f) (build-path originals-folder f))))))
    (when (directory-exists? originals-folder)
      (for-each move-to-orig root-files))))

(define (move-pictures-to-originals path)
  ; process a picture folder :
  ; - make sure no Originals folder exists prior;
  ; - create an Originals folder;
  ; - move all files to Originals folder"
  (when (directory-exists? path)
    (let ((originals-folder (build-path path "Originals")))
      (when (not (directory-exists? originals-folder))
        (begin (create-originals-folder path)
               (move-to-originals path))))))

(define (rename-folder path)
  ; rename a folder so that the prefix date follows the DDMMYYYY format
  (when (directory-exists? path)
    (let* ((remove-dashes-from-first (λ args (string-replace (car args) "-" "")))
           (dashed-date-pattern #px"\\d{4}\\-(0?[1-9]|1[012])\\-(0?[1-9]|[12][0-9]|3[01])*")
           (path-string (path->string path))
           (new-path (regexp-replace dashed-date-pattern
                                     path-string
                                     remove-dashes-from-first)))
      (rename-file-or-directory path new-path))))

;;; main

(define pictures-path-string "d:\\pictures")
(when (not (directory-exists? pictures-path-string)) exit)
(define pic-folders (map (λ (path) (build-path pictures-path-string path))
                         (directory-list pictures-path-string)))
(for-each move-pictures-to-originals pic-folders)
(for-each rename-folder pic-folders)
(void (message-box appname "  All done!         "))
