(deftheme jmn-gruvbox-general-org
  "Altering all the gruvbox themes-- load after loading the gruvbox theme")

;; changes which will be permanent
(with-eval-after-load 'org
  (setq org-todo-keyword-faces
        `(("NEXT" . ,(face-foreground font-lock-function-name-face))
          ("HOLD" . ,(face-foreground font-lock-builtin-face)))))

  ;; changes which will be undone when disabling the theme
  (custom-theme-set-faces
   'jmn-gruvbox-general-org

   `(org-priority ((t (:foreground ,(face-foreground 'font-lock-constant-face)))))
   `(org-block-begin-line ((t (:inherit font-lock-comment-face
                               :background ,(face-background 'default)))))
   `(org-block-end-line ((t (:inherit font-lock-comment-face
                             :background ,(face-background 'default))))))


(provide-theme 'jmn-gruvbox-general-org)
