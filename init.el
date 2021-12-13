;; 100 meg garbage collection threshold - will decrease startup times
(setq gc-cons-threshold (* 100 1024 1024))
;; For Langauge Server Protocol
(setq read-process-output-max (* 1024 1024))

;; Main
(if (file-readable-p "~/.emacs.d/gnu-emacs.el")
  (load "~/.emacs.d/gnu-emacs.el" nil t))

;; Customisations go in this file
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p "~/.emacs.d/custom.el")
  (write-region "" nil custom-file))
(load "~/.emacs.d/custom.el" t t)
