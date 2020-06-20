#My dotfiles#

* Distro: [Void Linux](http://voidlinux.org)
* Window manager: [i3](https://i3wm.org/)
* Editor: [vim](http://vim.org)
* Shell: [zsh + antigen](http://antigen.sharats.me/)
* Browser: Google Chrome
* Terminal Emulator: [rxvt-unicode](http://software.schmorp.de/pkg/rxvt-unicode.html)


##Keyboard##

http://antigen.sharats.me/

Despite using computers with the keyboard with the ABNT2 layout, I use


```
setxkbmap -layout us -variant intl
```

and key Caps Lock have the same functionality the Esc key and vice versa

```
! Swap caps lock and escape
remove Lock = Caps_Lock
keysym Escape = Caps_Lock
keysym Caps_Lock = Escape
add Lock = Caps_Lock
```



My Vim keybindings

* F2 - nerdtree
* F8 - clean highlight search
* F3 - set for using tab for indentation
* F4 - set for using space for indentation
* F5  - toggle paste mode on and off
* C-e + h', 'j', 'k', 'l'  - winresize
* C-n - autocomplete
