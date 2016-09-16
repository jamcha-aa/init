# init.el
author: jamcha (jamcha.aa@gmail.com)

last update: Sep 17, 2016.

My emacs init.el keeps this rule, ["Simple, slim and sleek, yet powerful"](http://scribes.sourceforge.net/) _specifically in writing texts (not coding)_.

This init.el targets Windows emacs or GNU/Linux emacs. Based on [gnupack](http://gnupack.osdn.jp/docs/latest/UsersGuide.html)'s init.el, I added following packages to work without gnupack, because gnupack is difficult to install on poor storage (eMMC 32gb) netbooks with latex system.

- ddskk (and SKK-JISYO.L)
- flatui
- helm
- hiwin-mode
- magit
- migemo (and cmigemo)
- org-license
- twittering-mode
- yatex
- xah-lookup
- emms (comment out on Windows)
- mew (comment out on Windows)
- navi2ch (comment out on Windows)
- mewconf and org-feed are my personal settings of e-mail and RSS.
 
This init.el works well on an old netbook (such as Thinkpad X121e, which passmark cpu score is 616). Emacs replaces heavy web applications to light-weight elisp packages (e.g., firefox to eww, gmail to mew, etc).

I know emacs is a powerful programming environment. But so far I use emacs as a powerful word processor. Therefore I have excluded programming support packages (e.g., yasnippet) from my settings.

#### Usage
- Put init.el, environment, elisp into the .emacs.d.
- Install packages of the previous section.
- Install "Source Han Sans Code-JP" fonts if you use linux.
- Enjoy.

#### bash on ubuntu on windows init.el

Recently I made an another version of init.el (in buw folder) especially for bash on ubuntu on windows. This version contains ddskk, helm, magit, org-license, twittering-mode, yatex, and xah-lookup. This init.el allows dired to open binary files with registered windows applications (e.g., open a pdf with adobe reader). I strongly recommend to use [wsl-terminal and cbwin](https://github.com/goreliu/wsl-terminal/releases) via bash accessing.
