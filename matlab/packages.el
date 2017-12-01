(defvar matlab-packages
  '(matlab-mode))

(defun matlab/init-matlab-mode ()
  (use-package matlab-mode
    ;; :defer t
    :mode (("\\.m\\'" . matlab-mode))
    :init
    (add-hook 'matlab-mode-hook 'spacemacs/run-prog-mode-hooks)
    (setq matlab-completion-technique 'increment)
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'matlab-mode
        "osi" 'matlab-shell
        "osc" 'matlab-shell-run-cell
        "osr" 'matlab-shell-run-region-or-line
        "ohc" 'matlab-shell-describe-command
        "ohv" 'matlab-shell-describe-variable
        "ohv" 'matlab-shell-topic-browser
        "ohv" 'matlab-shell-apropos
        "oti" 'tempo-template-matlab-if
        "otI" 'tempo-template-matlab-if-else
        "otf" 'tempo-template-matlab-for
        "ots" 'tempo-template-matlab-switch
        "ott" 'tempo-template-matlab-try
        "otw" 'tempo-template-matlab-while
        "otF" 'tempo-template-matlab-function)))
  )
