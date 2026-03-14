(deftheme jmn-light
  "Altering the default theme")

;; variables which will be permanent
(with-eval-after-load 'org
  (setq org-todo-keyword-faces
            `(("TODO" . "Red1")
              ("NEXT" . "orange")
              ("HOLD" . "Black")
              ("IGNORE" . "gray60"))))


  (custom-theme-set-faces
   'jmn-light

   ;; Org-mode
   `(org-block ((t (:background "gray93" :extend t))))

   ;; Tab bar
   '(tab-bar ((t (:inherit org-default :extend t))))
   `(tab-bar-tab ((t (:background "darkseagreen2"))))
   '(tab-bar-tab-inactive ((t (:inherit org-default :extend t))))
   )


(provide-theme 'jmn-light)
