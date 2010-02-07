(defun slime-gosh-buffer-package ()
  (let ((case-fold-search t)
        (regexp "^[ \t]*(select-module[ \n\t]+\\_<\\(.+?\\)\\_>)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 1)))))

(defun slime-gosh-package ()
  (or (slime-gosh-buffer-package)
      (slime-lisp-package)))

(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'slime-complete-symbol-function)
            (setq slime-complete-symbol-function 'slime-complete-symbol*)
            (make-local-variable 'slime-find-buffer-package-function)
            (setq slime-find-buffer-package-function 'slime-gosh-package)))

(defun gosh ()
  (interactive)
  (slime-start :program "/usr/bin/gosh"
               :program-args (list (expand-file-name
                                    "contrib/swank-gauche.scm" slime-path)
                                   (slime-swank-port-file))
               :init (lambda (&rest args) "")))
