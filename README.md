# init.el
author: jamcha (jamcha.aa@gmail.com)

last update: Oct 27, 2017.

### emacs init.el for Windows or GNU/Linux

My emacs init.el keeps this rule, ["Simple, slim and sleek, yet powerful"](http://scribes.sourceforge.net/) _specifically for writing (not coding)_.

This init.el targets Windows emacs or GNU/Linux emacs. Based on [gnupack](http://gnupack.osdn.jp/docs/latest/UsersGuide.html)'s init.el, I added following packages to work without gnupack, because gnupack is a large package to install on poor storage (eMMC 32gb) netbooks with latex system.

- ddskk (and SKK-JISYO.L), skk-azik compatible.
- flatui
- helm
- hiwin-mode
- magit
- migemo (and cmigemo)
- org-license
- twittering-mode
- yasnippet
- yatex
- xah-lookup
- emms (comment out on Windows)
- mew (for Linux)
- navi2ch (comment out on Windows)
- wanderlust (for Windows)
- likemc enables dired to behave as a two panel file manager (e.g., [mc](https://github.com/MidnightCommander/mc))
- mewconf and org-feed are my private settings of e-mail and RSS.

This init.el works well on an old netbook (such as Thinkpad X121e, which passmark cpu score is 616). Emacs replaces heavy web applications to light-weight elisp packages (e.g., firefox to eww, gmail to mew, etc).

I know emacs is a powerful programming environment. But so far I use emacs as a powerful word processor. Therefore I have excluded some popular programming support packages (e.g., auto-complete) from my settings.

### Customized keyboard shortcuts
##### org-mode
- "C-c l" org-store-link
- "C-c c" org-capture
- "C-c a" org-agenda
- "C-c b" org-iswitchb

org-capture and org-agenda files are stored to OneDrive. Modify paths as you want.

##### likemc
- "c" dired-copy-to-other-window
- "r" dired-move-to-other-window
- "l" dired-make-symlinks-to-other-window
- "e" wdired-change-to-wdired-mode

##### yasnippet
- "C-x i i" yas-insert-snippet
- "C-x i n" yas-new-snippet
- "C-x i v" yas-visit-snippet-file

##### window switching
- "C-t" works as "C-x o"

### Install
- Put init.el, environment, elisp into the .emacs.d.
- Install packages of the previous section.
  + Caution: Some private files (e.g., mewconf.el) are missing.
- Install "Source Han Sans Code-JP" fonts if you use linux.
- If you want to use skk-azik, put .skk to your home directory.
- Enjoy.

### init.el for bash on ubuntu on windows

Recently I made an another version of init.el (in buw folder) especially for bash on ubuntu on windows. This version contains ddskk, helm, magit, org-license, twittering-mode, yatex, and xah-lookup. This init.el allows dired to open binary files with registered windows applications (e.g., open a pdf with adobe reader). I strongly recommend to use [wsl-terminal and cbwin](https://github.com/goreliu/wsl-terminal/releases) via bash accessing.

#### Usage
- Make [wslstart](https://www49.atwiki.jp/ntemacs/pages/62.html)
- Put init.el, environment, elisp into the .emacs.d.
- Install packages.
- Enjoy.
