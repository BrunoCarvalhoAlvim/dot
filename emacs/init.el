;; Desabilita mensagem na inicialização
(setq inhibit-startup-message t)

;; Desabilita scrollbar
(scroll-bar-mode -1)

;; Desabilita barra de ferramentas
(tool-bar-mode -1)

;; Desabilita dicas
(tooltip-mode -1)


(set-fringe-mode 10)

;; Desabilita a barra de menu
(menu-bar-mode -1)

;; Habilita mensagens de erro visuais
(setq visible-bell t)

;; Configura fontes do editor
(set-face-attribute 'default nil :font "RobotoMono Nerd Font Mono" :height 140)

;; ESC sai do prompt
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Instalando pacotes ELPA MELPA
(require 'package)
(setq package-archives '(;;("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; inicializando pacotes em sistemas nao linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(require 'use-package)
(setq use-package-always-ensure t)

;; linhas numeradas
(column-number-mode)
(global-display-line-numbers-mode t)

;; desabilita linhas numeradas em alguns modos


(use-package swiper :ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; mudança de buffer mais amigável
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Quando carregar pela primeira vez rode o seguinte
;; comando
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; themas doom
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

;; colore os delimitadores
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; complemento ao ivy
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; melhora o buffer de ajuda
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel -load-theme :which-key "choose theme")))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :edit t))

(rune/leader-keys
 "ts" '(hydra-text-scale/body :which-key "scale text"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-collection evil which-key use-package rainbow-delimiters ivy-rich hydra helpful goto-chg general doom-themes doom-modeline counsel command-log-mode annalist)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
