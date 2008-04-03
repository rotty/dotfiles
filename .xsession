#!/bin/zsh

# Environment variables
export SSH_ASKPASS=/usr/bin/ssh-askpass-fullscreen
export OOO_FORCE_DESKTOP=gnome

xset dpms 240 600 1200
#xmodmap ~/.xmodmap-`uname -n`

#export GDK_USE_XFT=1 # GTK+ font antialiasing

if [ -z `pidof irexec` ] && command -v irexec; then
    irexec --daemon
    irxevent &
fi

ssh-add

xset b off

if [ -f ~/.xsession-local ]; then
  . ~/.xsession-local
fi

xmodmap -e "add mod5 = Menu"

#emacs -name "main" -eval "(setq gnuserv-kill-quietly t)" -f gnuserv-start &
#emacs -name "erc" -f irc &
#firefox &
#x-terminal-emulator &
#exec larswm
exec gnome-session
#exec fluxbox
