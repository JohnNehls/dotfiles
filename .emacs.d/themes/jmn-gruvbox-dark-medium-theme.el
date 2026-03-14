(deftheme jmn-gruvbox-dark-medium
  "Altering the gruvbox-dark-medium theme-- load after loading the gruvbox theme")

(let ((done-color "gray35"))

  ;; changes which will be permanent  
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          `(("NEXT" . ,(face-foreground font-lock-function-name-face))
            ("HOLD" . ,(face-foreground font-lock-builtin-face))
            ("DONE" . done-color)
            ("IGNORE" . done-color))))


  ;; changes which will be undone when disabling the theme
   (custom-theme-set-faces
    'jmn-gruvbox-dark-medium
    ;; Basics
    ;; `(default ((t (:foreground "moccasin" :extend t))))
    `(line-number ((t :background ,(face-attribute 'default :background))))
    `(font-lock-comment-face ((t (:foreground "#98be65" :extend t))))

    ;; org    
    `(org-agenda-done ((t (:foreground ,done-color))))
    `(org-headline-done ((t (:foreground ,done-color))))
    `(org-done ((t (:foreground ,done-color))))
    )
)

(provide-theme 'jmn-gruvbox-dark-medium)
