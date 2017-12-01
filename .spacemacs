;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     yaml
     html
     gutils
     ;; lua
     ;; windows-scripts
     vimscript
     python
     ;; (extra-langs :variables matlab-mode)
     (c-c++ :variables c-c++-enable-clang-support t)
     ;; ycmd
     spacemacs-cmake-ide
     helm
     bibtex
     pandoc
     semantic
     pdf-tools
     plantuml
     (ranger :variables ranger-override-dired t)
     slack
     deft
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-complete-with-key-sequence "jk"
                      :disabled-for: org)
     better-defaults
     google-calendar
     emacs-lisp
     git
     java
     markdown
     (org :variables org-enable-reveal-js-support t)
     trello
     matlab
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;; spell-checking
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     version-control
     latex
     ;; irony
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(glsl-mode
                                      opencl-mode
                                      cdlatex
                                      all-the-icons
                                      all-the-icons-dired
                                      calfw-org)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))


(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  (push "/usr/local/share/emacs/site-lisp/rtags" load-path)

  (load "~/.emacs.d/private/.spacemacs-secrets.el.gpg")

  (add-hook 'org-agenda-mode-hook (lambda() (org-gcal-sync)))
  (add-hook 'org-capture-after-finalize-hook 'google-calendar/sync-cal-after-capture)
  (setq org-agenda-files (list "~/Dropbox/Notes/calendar.org"))
  )

(defun dotspacemacs/user-config ()
  (setq-default helm-make-build-dir "build")
  (require 'helm-bookmark)
  (c-add-style "my"
               '((indent-tabs-mode . nil)
                 (c-basic-offset . 4)
                 (c-offsets-alist

                  (substatement-open . 0)
                  (inline-open . 0)
                  (statement-cont . c-lineup-assignments)
                  (inextern-lang . 0)
                  (innamespace . 0))))



  (setq exec-path (append exec-path '("/home/gnox/Development/tools/jdk1.8.0_131/bin/")))

  (setq org-plantuml-jar-path (expand-file-name "/home/gnox/Dropbox/plantuml.jar"))
  (setq plantuml-jar-path (expand-file-name "/home/gnox/Dropbox/plantuml.jar"))

  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'ranger-mode-hook 'all-the-icons-dired-mode)

  (setq company-idle-delay 0.1)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (emacs-lisp . t)
      (matlab . t)
      (plantuml . t)
      (ditaa . t
)
      (python . t)))
  (setq org-src-fontify-natively t)

  (setq company-idle-delay 0.1)
  (setq ycmd-server-command '("python" "/home/jozef2/Dropbox/development/programs/Personal/ycmd/ycmd"))
  (setq python-shell-interpreter "python3")
  (setq org-babel-python-command "python3")

  (setq ycmd-force-semantic-completion t)
  (push '(other . "my") c-default-style)
  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  (setq org-latex-create-formula-image-program 'dvipng)

  (setq-default dotspacemacs-configuration-layers
                '((c-c++ :variables
                         c-c++-default-mode-for-headers 'c++-mode)))

  (global-git-commit-mode t)
  (setq org-hide-macro-markers t)

  (setq deft-extensions '("org" "trello"))
  (setq deft-directory "~/Dropbox/Notes")

  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f"))

  ;; (setq matlab-completion-technique 'increment)
  (setq matlab-shell-command-switches '("-nodesktop -nosplash"))
  (setq neo-theme 'icons)

  (spacemacs/set-leader-keys
    "omms" 'matlab-shell
    "omsc" 'matlab-shell-run-cell
    "omsr" 'matlab-shell-run-region-or-line
    "omhc" 'matlab-shell-describe-command
    "omhv" 'matlab-shell-describe-variable
    "omht" 'matlab-shell-topic-browser
    "omha" 'matlab-shell-apropos
    "omti" 'tempo-template-matlab-if
    "omtI" 'tempo-template-matlab-if-else
    "omtf" 'tempo-template-matlab-for
    "omts" 'tempo-template-matlab-switch
    "omtt" 'tempo-template-matlab-try
    "omtw" 'tempo-template-matlab-while
    "omtF" 'tempo-template-matlab-function)

  (require 'slack)
  (slack-register-team
   :name "Kill-Bills"
   :default t
   :client-id my-slack-client-id
   :client-secret my-slack-client-secret
   :token my-slack-token
   :subscribed-channels '(general slackbot))

  (add-to-list 'org-latex-classes
               '("diploma"
                 "\\documentclass[a4paper,english,10.5pt,appendix,twoside,openright]{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))

  (setq cmake-ide-flags-c '("-fopenmp=libomp"))
  (setq cmake-ide-flags-c++
        (append '("-std=c++1y")
                ;; (mapcar (lambda(path)
                ;;           (concat "-I" path))
                ;;         (c++-include-paths))
                '("-fopenmp=libomp")
                '("-Iusr/lib/gcc/x86_64-linux-gnu/5/include/")))
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (calfw-org org-gcal calfw all-the-icons-dired all-the-icons memoize font-lock+ plantuml-mode slack emojify circe oauth2 websocket org-trello ox-reveal deft ranger cdlatex yaml-mode winum web-mode vimrc-mode unfill thrift tagedit stan-mode slim-mode scss-mode scad-mode sass-mode qml-mode pug-mode pandoc-mode ox-pandoc ht org-ref pdf-tools key-chord ivy tablist org-category-capture opencl-mode matlab-mode less-css-mode julia-mode dash-functional helm-css-scss helm-bibtex parsebib haml-mode fuzzy emmet-mode dactyl-mode company-web web-completion-data company-emacs-eclim eclim cmake-ide levenshtein biblio biblio-core arduino-mode auctex-latexmk company-auctex auctex flycheck-ycmd company-ycmd ycmd request-deferred deferred stickyfunc-enhance srefactor glsl-mode irony-eldoc flycheck-irony company-irony-c-headers company-irony irony yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help disaster company-c-headers cmake-mode clang-format smeargle orgit org-projectile pcache org-present org org-pomodoro alert log4e gntp org-download mwim mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor diff-hl company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "cd _build; and cmake ..; and cmake --build . --target raw3_raycast; and ./raw3_raycast")
     (org-export-in-background . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
