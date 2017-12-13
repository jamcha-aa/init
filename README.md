# init.el
author: jamcha (jamcha.aa@gmail.com)

last update: Nov 17, 2017.

### emacs init.el for Windows or GNU/Linux ([Rust :heart: Emacs](https://github.com/wilfred/remacs) compatible)

My emacs init.el keeps this rule, ["Simple, slim and sleek, yet powerful"](http://scribes.sourceforge.net/) _specifically for writing (not coding)_.

This init.el targets Windows emacs or GNU/Linux emacs. Based on [gnupack](http://gnupack.osdn.jp/docs/latest/UsersGuide.html)'s init.el, I added following packages to work without gnupack, because gnupack is a large package to install on poor storage (eMMC 32gb) netbooks with latex system. Almost all of codes were cited from many emacs sensei on the web.

- ddskk (and SKK-JISYO.L), skk-azik compatible.
- flatui
- helm
- hiwin-mode
- magit
- migemo (and cmigemo)
- org-license
- powerline
- twittering-mode
- yasnippet
- yasnippet-snippets
- yatex
- xah-lookup
- emms (comment out on Windows)
- mew (for Linux)
- navi2ch (comment out on Windows)
- smart-newline (for Windows)
- wanderlust (for Windows)
- likemc enables dired to behave as a two panel file manager (e.g., [mc](https://github.com/MidnightCommander/mc))
- mewconf and org-feed are my private settings of e-mail and RSS.

This init.el works well on an old netbook (such as Thinkpad X121e, which passmark cpu score is 616). Emacs replaces heavy web applications to light-weight elisp packages (e.g., firefox to eww, gmail to mew, etc).

I know emacs is a powerful programming environment. But so far I use emacs as a powerful word processor. Therefore I have excluded some popular programming support packages (e.g., auto-complete) from my settings.

### Customized keyboard shortcuts
##### dired-tar (only for Linux)
- On dired, press "shift-z" then it compress or extract file/directory.

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
  + Edit org-capture section as you want.
  + When you use Windows, put [SKK-JISYO.L](http://openlab.ring.gr.jp/skk/wiki/wiki.cgi?page=SKK%BC%AD%BD%F1#p7) and [dict folder (in cmigemo)](https://www.kaoriya.net/software/cmigemo/) to your home directory.
  + mew, twittering-mode and wanderlust requires additional individual settings (registering gmail imap, OAuth, etc).
- When you use linux, install "Source Han Sans Code-JP" fonts.
- If you want to use skk-azik, put .skk to your home directory.
  + Edit .skk when you use US keyboards.
- Enjoy.

### init.el for Windows Subsystem for Linux (WSL, previously called "bash on ubuntu on windows")
last update: Dec 7, 2017.

I made an another version of init.el (in buw folder) for WSL. This version contains ddskk, helm, magit, mew, navi2ch, org-license, powerline, twittering-mode, yasnippet, yatex, and xah-lookup. This init.el allows dired to open binary files with registered windows applications (e.g., open a pdf with adobe reader). I strongly recommend to use [wsltty](https://github.com/mintty/wsltty) via bash accessing. This init.el is also compatible with [Rust :heart: Emacs](https://github.com/wilfred/remacs).

#### Usage
- Install [wslstart](https://www49.atwiki.jp/ntemacs/pages/62.html)
- Enter buw folder.
- Put init.el, environment, elisp into your .emacs.d.
- Install packages.
- Enjoy.
