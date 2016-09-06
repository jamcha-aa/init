# init.el
author: jamcha (jamcha.aa@gmail.com)
date: Sep 7, 2016.

My emacs init.el keeps this rule, ["Simple, slim and sleek, yet powerful"](http://scribes.sourceforge.net/) _specifically in writing texts (not coding)_.

This init.el targets Windows emacs or GNU/Linux emacs. Based on [gnupack](http://gnupack.osdn.jp/docs/latest/UsersGuide.html)'s init.el, I added following packages to work without gnupack, because gnupack is difficult to install on poor storage (eMMC 32gb) netbooks with latex system.

- ddskk
- flatui
- helm
- hiwin-mode
- magit
- migemo (and cmigemo)
- org-license
- yatex
- emms (comment out in Windows)
- mew (comment out in Windows)
- navi2ch (comment out in Windows)
- twittering-mode (comment out in Windows)
- mewconf and org-feed are my personal settings of e-mail and RSS.
 
This init.el works well on an old netbook (such as Thinkpad X121e, which passmark cpu score is 616). Emacs replaces heavy web applications to light-weight elisp packages (e.g., firefox to eww, gmail to mew, etc).

I know emacs is a powerful programming environments. But so far I use emacs as a powerful word processor. Therefore I have excluded programming support packages from my settings, like as yasnippet.
