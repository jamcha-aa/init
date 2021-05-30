# init.el
author: jamcha (jamcha.aa@gmail.com)

last update: Aug 19, 2018.

### emacs init.el for Windows or GNU/Linux 25.1 or later ([Rust :heart: Emacs](https://github.com/wilfred/remacs) compatible)

My init.el ideal is ["Simple, slim and sleek, yet powerful"](http://scribes.sourceforge.net/) _specifically for writing (not coding)_.

This init.el targets Windows emacs or GNU/Linux emacs. Based on [gnupack](http://gnupack.osdn.jp/docs/latest/UsersGuide.html)'s init.el, I added following packages to work without gnupack, because gnupack is a large package to install on poor storage (eMMC 32gb) netbooks with latex system. Almost all codes are derived from many emacs sensei on the web.

- [ddskk](https://github.com/skk-dev/ddskk/) (and [SKK-JISYO.L](http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1#p7)), skk-azik compatible.
- [flatui-theme](https://github.com/john2x/flatui-theme.el)
- [helm](https://github.com/helm/helm)
- [magit](https://github.com/magit/magit)
- [migemo](https://github.com/emacs-jp/migemo) (and cmigemo)
- [smart-mode-line](https://github.com/Malabarba/smart-mode-line)
- [switch-window](https://github.com/dimitri/switch-window)
- [twittering-mode](https://github.com/hayamiz/twittering-mode)
- [wordcount-mode](https://github.com/bnbeckwith/wc-mode)
- [xah-lookup](https://github.com/xahlee/lookup-word-on-internet)
- [yasnippet](https://github.com/joaotavora/yasnippet)
- [yasnippet-snippets](https://github.com/AndreaCrotti/yasnippet-snippets)
- [yatex](https://www.yatex.org/)
- [mew](https://github.com/kazu-yamamoto/Mew) (for Linux)
- [navi2ch](https://github.com/naota/navi2ch) (comment out on Windows)
- [wanderlust](https://github.com/wanderlust/wanderlust) (for Windows)
- likemc enables dired to behave as a two panel file manager (e.g., [mc](https://github.com/MidnightCommander/mc))
- mewconf and org-feed are personal settings of e-mail and RSS.

This init works well on an old netbook (such as Thinkpad X121e, which passmark cpu score is 616). Emacs replaces heavy web applications to light-weight elisp packages (e.g., firefox to eww, gmail to mew, etc). 

### Customized keyboard shortcuts
##### dired-tar (only for Linux)
- On dired, press "shift-z" then it compress or extract file/directory.

##### org-mode
- "C-c l" org-store-link
- "C-c c" org-capture
- "C-c a" org-agenda
- "C-c b" org-iswitchb

##### likemc
- "c" dired-copy-to-other-window
- "r" dired-move-to-other-window
- "l" dired-make-symlinks-to-other-window
- "e" wdired-change-to-wdired-mode

##### yasnippet
- "C-x i i" yas-insert-snippet
- "C-x i n" yas-new-snippet
- "C-x i v" yas-visit-snippet-file

##### switching windows
- "C-t" works as "C-x o"
- "C-x o" launches [switch-window](https://github.com/dimitri/switch-window)

##### launch wordcount-mode
- "C-c w"

### Install
- Put init.el, environment, elisp, conf, and org into the .emacs.d.
- Install packages of the previous section.
  + Edit org-capture section as you want.
  + When you use Windows, put [SKK-JISYO.L](http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1#p7) and [dict folder (in cmigemo)](https://www.kaoriya.net/software/cmigemo/) to your home directory.
  + mew, twittering-mode and wanderlust requires additional individual settings (registering gmail imap, OAuth, etc).
  + mew requires additional certs files. Download it from https://mew.org/Release/, extract it and rename as ".certs" then put it your home directory.
- When you use linux, install "Source Han Code-JP" fonts.
- If you want to use skk-azik, put .skk to your home directory.
  + Edit .skk when you use US keyboards.
- Enjoy.

### TIPS for [wordcount-mode](https://github.com/bnbeckwith/wc-mode)
- If you mainly use two bytes chars (such as Chinese, Japanese, and Korean), open the `~/.emacs.d/elpa/wc-mode` folder, then remove **wc-mode.elc**. Open **wc-mode.el**, edit `WC[%W%w/%tw]` (word count mode) to `WC[%C%c/%tc]` (character count mode).

### init.el for Windows Subsystem for Linux (WSL, previously called "bash on ubuntu on windows")
last update: May 10, 2018.

I made an another version of init.el (in buw folder) for WSL. This version contains ddskk, helm, magit, mew, navi2ch, org-license, powerline, twittering-mode, yasnippet, yatex, and xah-lookup. This init.el allows dired to open binary files with registered windows applications (e.g., open a pdf with adobe reader). I strongly recommend to use [wsltty](https://github.com/mintty/wsltty) via bash accessing. This init.el is also compatible with [Rust :heart: Emacs](https://github.com/wilfred/remacs).

#### Usage
- Install [wslstart](https://www49.atwiki.jp/ntemacs/pages/62.html)
- Enter buw folder.
- Put init.el, environment, elisp, conf, and org into your .emacs.d.
- Install packages.
- Enjoy.

### init.el for antiX Linux
release date: May 30, 2021.

This init.el is made for a :heart: [antiX Magic](https://antixlinux.com/) :heart: environment. This version contains skk, magit, and wc-mode.

#### Usage
- Install fonts-noto-cjk-extra fonts.
- Download SKK-JISYO.L from the [official website](https://github.com/skk-dev/dict/blob/master/SKK-JISYO.L) and put it into .emacs.d directory.
- Install ddskk, magit, wc-mode.
- Enjoy.