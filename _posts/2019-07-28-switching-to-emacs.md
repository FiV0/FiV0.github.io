---
layout: post
title: Switching to EMACS
comments: true
redirect_from: "/2019/07/28/emacs-switch/"

permalink: emacs-switch 
---

After around 7 years on vim, I have decided to try emacs. My main reason is not dissatisfaction with vim, but rather the better support for REPL-based languages like Common Lisp and Clojure.
I am actually very happy with [slimv](https://github.com/kovisoft/slimv), a SLIME (*the* Common Lisp interaction mode) integration for vim. Nevertheless, it was always more hassle to tweak something 
to my liking and if you have a question the userbase is a lot smaller.
This [answer](https://stackoverflow.com/a/95493) on Stackoverflow gives a nice little insight why the tooling for Lisps is a lot better on the emacs side. Secondly, I recently wanted to try out
Clojure(script) and the option to go for seemed [CIDER](https://cider.readthedocs.io/en/stable/), an emacs integrated Clojure IDE. The post tries to summarize my setup, plugins and the experience of the *migration* so far.

The equivalent of the `.vimrc` file is the `.emacs` file (sometimes also `init.el`). Vanilla emacs has some packages available from the GNU package repository, but most emacs packages are available through the 
[MELPA](https://melpa.org/#/) package archive. Add the following lines to your configuration file to enable the later.
```el
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
```
To install packages you can type `M-x list-packages RET` and select the ones you are interested in. It follows some non plugin related setup:
```el
;; save auto-save and backup files somewhere else
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; global linenumbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; dired directory sorting
(setq dired-listing-switches "-alX  --group-directories-first")

;; tabs as spaces
(progn (setq-default indent-tabs-mode nil))
  
(set-cursor-color "#ef330e")
```

I started off by installing [evil](https://github.com/emacs-evil/evil), a plugin that tries to emulate vim keybindings in emacs. I was supprised how well everything worked out of the box. Even stuff like saving, searching and navigating directories worked instantly with 
the same keybindings as in vim. An option I had to enable explicitly was screen scrolling with `C-u` (be aware that this might cripple the keymap for other plugins, as `C-u` seems to be a commonly used mapping).
The second non-standard option I used was allowing tabs in `evil-mode`. The whole setup:
```el
;; Evil mode
(setq evil-want-C-u-scroll t)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
;;allow tabs in evil mode
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(evil-mode 1)

;; make normal mode the default
(setq evil-emacs-state-modes nil)
```
The next plugin I installed was [evil-nerd-commenter](https://github.com/redguardtoo/evil-nerd-commenter) together with [evil-leader](https://github.com/cofi/evil-leader). Nerdcommenter is originally a vim plugin for easy (un)commenting in all kinds of programming languages. `evil-nerd-commenter` is the port to emacs.
`evil-leader` is a plugin that lets you emulate the leader key from vim. As I wanted to have the same keybindings as in my vim setup, I needed the leader key customization option. 
```el
;; leader mode
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; nerd-commenter
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "."  'evilnc-copy-and-comment-operator
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
)
```
The next section describes plugins that are useful for working with Lisps. [Paredit](https://www.emacswiki.org/emacs/ParEdit) is a plugin that keeps your parentheses (or other brackets) balanced.
As `paredit` is not tailored to work together with `evil`, one also needs [evil-cleverparens](https://github.com/luxbock/evil-cleverparens), so that `evil` does not mess up `paredit`'s parentheses balancing with command like a `x` in `normal-mode`.
For better code readability one needs [rainbow-parentheses](https://github.com/Fanael/rainbow-delimiters) which colors matching parentheses differently. These plugins are not useful in every buffer. Emacs has the option to set so
called hooks for buffer customization. Hooks call a set of functions on well defined occasions. 
In this case we want to enable certain kinds of plugins when editing lisp files. An example of a hook is
```el
(add-hook 'lisp-mode-hook  #'enable-paredit-mode)
```
which enables `paredit-mode` whenever opening a lisp buffer. See my [.emacs](https://github.com/FiV0/dotfiles-tower/blob/master/.emacs#L113) for more hooks.

Another plugin I use and which was ported from vim is [evil-surround](https://github.com/emacs-evil/evil-surround). 
It lets you easily enclose previously written code/text in parentheses or quotes. One has to set some
custom options so that `evil-surround` and `paredit` work seamlessly together. 
```el
;; evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
;; compatibility with paredit
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-change . change))
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-delete . delete))
```

For code completion I use the [company](http://company-mode.github.io/) plugin together with [company-quickhelp](https://github.com/expez/company-quickhelp) which gives you documentation for the current selected completion.
```el
;; company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\t" 'company-complete-common)
(setq company-idle-delay 0) ; start completions immediately

;; company quickhelp
(company-quickhelp-mode)
```
As already mentioned, I am using (or rather trying to use) Cider for Clojure(script). I won't go into the details of the cider environment setup for REPL-driven development. Enough content for another post. The only option I enabled
via emacs was the direct documentation lookup.
```el
;; cider stuff
(setq cider-prompt-for-symbol nil)
```
[Slime](https://common-lisp.net/project/slime/) is the de facto standard for developing in Common Lisp. I also installed [slime-company](https://github.com/anwyn/slime-company), which gives completions for slime. One enables the later plugin by 
adding `slime-company` to `slime-contribs`. `slime-company` then becomes the primary completion engine once slime starts.
This can be a bit annoying because local buffer variables are no longer completed. The solution is to have grouped 
company backend, i.e. check multiple company backends for completion candidates.
```el
;; slime stuff
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-company))
;; the slime-company contrib pushs slime-company as single backend 
;; to company-backends
(defun slime-avoid-override ()
  (pop company-backends)
  (push '(company-slime company-dabbrev) company-backends))
(setq slime-connected-hook 'slime-avoid-override)
```

When working with vim, I mostly use tabs and a split windows. Standard emacs doesn't come with anything like tabs. I used `tabbar` and `powerline` 
to get to something very similar. [This](https://amitp.blogspot.com/2018/10/emacs-prettier-tabbar.html) blog post describes most of my setup. 

To finish off, here a list of the most relevant shortcuts that brought me up to speed in emacs. I omitted all the vimlike commands, as they weren't new to me.
For the REPL interaction commands of SLIME and CIDER chechout their respective documentations.
```
M-x revert-buffer - reload buffer (useful when changing .emacs)
M-x describe-bindings - show all bindings in current mode
M-x ielm - start elisp repl
C-x C-b - list buffers
C-w C-o - close window
C-g - quit partially typed command
C-z - switch from normal to emacs mode and vice versa
C-x C-f - open file/path
C-x C-c - exit emacs

;; shell
M-x shell - open a shell in Emacs
M-n (C-<DOWN>) - next shell command
M-p (C-<UP>) - previous shell command
```

I didn't touch upon all the details of my setup. If you are interested, just check out the full 
[.emacs](https://github.com/FiV0/dotfiles-tower/blob/master/.emacs).
