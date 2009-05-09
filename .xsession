#!/bin/zsh

if [ -f ~/.fehbg ]; then
    eval `cat ~/.fehbg`
fi

#xset dpms 240 600 1200
xmodmap ~/.xmodmap-`uname -n`
setxkbmap -option ctrl:nocaps

#export GDK_USE_XFT=1 # GTK+ font antialiasing

if [ -z `pidof irexec` ] && command -v irexec; then
    irexec --daemon
    irxevent &
fi


#ssh-add

xset b off

if [ -f ~/.xsession-local ]; then
  . ~/.xsession-local
fi

gnome-settings-daemon &

exec awesome
