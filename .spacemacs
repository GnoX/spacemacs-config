;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     javascript
     shell-scripts
     yaml
     html
     gutils
     ;; lua
     ;; windows-scripts
     vimscript
     python
     ;; (extra-langs :variables matlab-mode)
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode)
     ;; ycmd
     (ruby :variables
           ruby-enable-enh-ruby-mode t
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec)
     ruby-on-rails
     dash
     spacemacs-cmake-ide
     helm
     bibtex
     pandoc
     semantic
     pdf-tools
     docker
     plantuml
     (ranger :variables ranger-override-dired t)
     slack
     deft
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-return-key-behavior nil
                      auto-completion-enable-sort-by-usage t
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-help-tooltip 'manual
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
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     version-control
     latex
     gtags
     shaders
     ;; irony
     journal
     )
   dotspacemacs-additional-packages '(opencl-mode
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
  (setq org-crypt-key "Jozef Mlaka")
  (setq org-gcal-file-alist '(("gc3b7qj0n8ii4p0muv19ju8hek@group.calendar.google.com"
                               . "~/Dropbox/Notes/timetable.org")
                              ("x.jozef.mlaka@gmail.com" . "~/Dropbox/Notes/calendar.org")))

  (setq org-agenda-files (list "~/Dropbox/Notes/todo.org"
                               "~/Dropbox/Notes/personal.trello"
                               "~/Dropbox/Notes/timetable.org")))

(defun dotspacemacs/user-config ()
  (setq-default helm-make-build-dir "build")
  (require 'helm-bookmark)
  (org-crypt-use-before-save-magic)
  (setq web-mode-extra-snippets
        '(("erb" . (("erb" . "<%= | %>")))))

  (define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  (add-hook 'text-mode-hook 'auto-fill-mode)

  (c-add-style "my"
               '((indent-tabs-mode . nil)
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (substatement-open . 0)
                  (inline-open . 0)
                  (statement-cont . c-lineup-assignments)
                  (inextern-lang . 0)
                  (innamespace . 0))))

  (setq cfw:org-capture-template '("t" "Create trello todo" entry (file "~/Dropbox/Notes/personal.trello")
                                   "* %^{Description}\nDEADLINE: %(cfw:org-capture-day)\n"))
  (setq org-capture-templates
        '(("a" "Appointment" entry (file  "~/Dropbox/Notes/calendar.org" )
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("e" "Event" entry (file "~/Dropbox/Notes/calendar.org")
           "* Event %^{Event}\n%^T\n%?")
          ("l" "Link" entry (file+headline "~/Dropbox/Notes/todo.org" "Links")
           "* %? %^L %^g \n%T" :prepend t)
          ("n" "Note" entry (file+headline "~/Dropbox/Notes/todo.org" "Note space")
           "* %?\n%u" :prepend t)
          ("j" "todo" entry (file+headline "~/Dropbox/Notes/todo.org" "Todos")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)))


  (spacemacs/set-leader-keys "agd" 'org-gcal-delete-at-point)

  (setq exec-path (append exec-path '("/home/gnox/Development/tools/jdk1.8.0_131/bin/")))

  (setq org-plantuml-jar-path (expand-file-name "/home/gnox/Dropbox/plantuml.jar"))
  (setq plantuml-jar-path (expand-file-name "/home/gnox/Dropbox/plantuml.jar"))

  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'ranger-mode-hook 'all-the-icons-dired-mode)
  (global-auto-revert-mode t)

  (setq org-journal-dir "~/Dropbox/Notes/journal/")

  (setq company-idle-delay 0.1)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (emacs-lisp . t)
      (matlab . t)
      (plantuml . t)
      (ditaa . t)
      (python . t)))

  (setq org-src-fontify-natively t)

  (setq company-idle-delay 0.1)
  (setq ycmd-server-command '("python" "/home/jozef2/Dropbox/development/programs/Personal/ycmd/ycmd"))
  (setq python-shell-interpreter "python3")
  (setq org-babel-python-command "python3")

  (setq ycmd-force-semantic-completion t)
  (push '(other . "my") c-default-style)
  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

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


  (defun get-all-trello-ids-from-org-file (file)
    (let ((parsetree
           (with-temp-buffer
             (insert-file-contents file)
             (org-mode)
             (org-element-parse-buffer 'headline))))
      (org-element-map parsetree 'headline (lambda (hl) (org-element-property :orgtrello-id hl)))))

  (defun get-org-file-headlines-for-calendar()
    (let ((parsetree
           (with-temp-buffer
             (insert-file-contents "~/Dropbox/Notes/personal.trello")
             (org-mode)
             (org-element-parse-buffer 'headline))))
      (let ((calendar-headlines (get-all-headlines-from-org-file "~/Dropbox/Notes/calendar.org")))
        (org-element-map parsetree 'headline
          (lambda (hl)
            (let ((deadline (org-element-property :deadline hl)))
              (when deadline
                (let ((todo-keyword (org-element-property :todo-keyword hl))
                      (title (org-element-property :title hl))
                      (link-id (org-element-property :ORGTRELLO-ID hl)))
                  (concat "* " todo-keyword " " title "\\n"
                          (plist-get (car (cdr deadline)) ':raw-value) "\\n"
                          "https://trello.com/c/" link-id)
                  ))))))))

  (setq org-link-abbrev-alist '(("trello" . "~/Dropbox/Notes/personal.trello::%s")))

  (defun trello-headline-completion (&optional arg)
    (format "trello:%s"
            (completing-read "Trello headlines: "
                             (let ((parsetree (with-temp-buffer
                                                (insert-file-contents "~/Dropbox/Notes/personal.trello")
                                                (org-mode)
                                                (org-element-parse-buffer 'headline))))
                               (org-element-map parsetree 'headline
                                 (lambda (hl) (org-element-property :title hl)))))))

  (org-link-set-parameters "trello" :complete 'trello-headline-completion)
  (defun org-link-describe (link desc)
    (when (string-prefix-p "trello:" link)
      (string-remove-prefix "trello:" link)))

  (setf org-make-link-description-function #'org-link-describe)
  (setq-default org-download-image-dir "./img/")
  (setq-default org-download-heading-lvl nil)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "orange" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

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

  ;; (custom-set-variables '(org2jekyll-blog-author "GnoX")
  ;;                       '(org2jekyll-source-directory
  ;;                         (expand-file-name "~/Dropbox/development/programs/Personal/org-gnox.github.io"))
  ;;                       '(org2jekyll-jekyll-directory
  ;;                         (expand-file-name "~/dropbox/development/programs/Personal/gnox.github.io"))
  ;;                       '(org2jekyll-jekyll-drafts-dir "")
  ;;                       '(org2jekyll-jekyll-posts-dir "_posts")
  ;;                       '(org-publish-project-alist
  ;;                         `(("default"
  ;;                            :base-directory ,(org2jekyll-input-directory)
  ;;                            :base-extension "org"
  ;;                            :publishing-directory ,(org2jekyll-output-directory)
  ;;                            :publishing-function org-html-publish-to-html
  ;;                            :headline-levels 4
  ;;                            :section-numbers nil
  ;;                            :with-toc nil
  ;;                            :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
  ;;                            :html-preamble t
  ;;                            :recursive t
  ;;                            :make-index t
  ;;                            :html-extension "html"
  ;;                            :body-only t)
  ;;                           ("post"
  ;;                            :base-directory ,(org2jekyll-input-directory)
  ;;                            :base-extension "org"
  ;;                            :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
  ;;                            :publishing-function org-html-publish-to-html
  ;;                            :headline-levels 4
  ;;                            :section-numbers nil
  ;;                            :with-toc nil
  ;;                            :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
  ;;                            :html-preamble t
  ;;                            :recursive t
  ;;                            :make-index t
  ;;                            :html-extension "html"
  ;;                            :body-only t)
  ;;                           ("images"
  ;;                            :base-directory ,(org2jekyll-input-directory "img")
  ;;                            :base-extension "jpg\\|gif\\|png"
  ;;                            :publishing-directory ,(org2jekyll-output-directory "img")
  ;;                            :publishing-function org-publish-attachment
  ;;                            :recursive t)
  ;;                           ("js"
  ;;                            :base-directory ,(org2jekyll-input-directory "js")
  ;;                            :base-extension "js"
  ;;                            :publishing-directory ,(org2jekyll-output-directory "js")
  ;;                            :publishing-function org-publish-attachment
  ;;                            :recursive t)
  ;;                           ("css"
  ;;                            :base-directory ,(org2jekyll-input-directory "css")
  ;;                            :base-extension "css\\|el"
  ;;                            :publishing-directory ,(org2jekyll-output-directory "css")
  ;;                            :publishing-function org-publish-attachment
  ;;                            :recursive t)
  ;;                           ("web" :components ("images" "js" "css"))))
  ;; )
)


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (web-beautify livid-mode skewer-mode simple-httpd js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode projectile-rails inflections feature-mode insert-shebang fish-mode company-shell enh-ruby-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby dockerfile-mode docker json-mode docker-tramp json-snatcher json-reformat helm-gtags ggtags zeal-at-point helm-dash org-journal hyde org2jekyll kv flyspell-correct-helm flyspell-correct company-quickhelp auto-dictionary calfw-org org-gcal calfw all-the-icons-dired all-the-icons memoize font-lock+ plantuml-mode slack emojify circe oauth2 websocket org-trello ox-reveal deft ranger cdlatex yaml-mode winum web-mode vimrc-mode unfill thrift tagedit stan-mode slim-mode scss-mode scad-mode sass-mode qml-mode pug-mode pandoc-mode ox-pandoc ht org-ref pdf-tools key-chord ivy tablist org-category-capture opencl-mode matlab-mode less-css-mode julia-mode dash-functional helm-css-scss helm-bibtex parsebib haml-mode fuzzy emmet-mode dactyl-mode company-web web-completion-data company-emacs-eclim eclim cmake-ide levenshtein biblio biblio-core arduino-mode auctex-latexmk company-auctex auctex flycheck-ycmd company-ycmd ycmd request-deferred deferred stickyfunc-enhance srefactor glsl-mode irony-eldoc flycheck-irony company-irony-c-headers company-irony irony yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help disaster company-c-headers cmake-mode clang-format smeargle orgit org-projectile pcache org-present org org-pomodoro alert log4e gntp org-download mwim mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor diff-hl company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "cd _build; and cmake -DCMAKE_BUILD_TYPE=Release ..; and cmake --build . --target raw3_raytrace; and ./raw3_raytrace")
     (projectile-project-compilation-cmd . "cd _build; and cmake ..; and cmake --build . --target raw3_raycast; and ./raw3_raycast")
     (org-export-in-background . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
