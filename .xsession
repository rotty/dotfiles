#!/bin/zsh

# Environment variable
#export SSH_ASKPASS=/usr/bin/ssh-askpass-fullscreen

xset dpms 120 300 600
#xmodmap ~/.xmodmap-`uname -n`

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

#trayer &
#emacs -name "main" -eval "(setq gnuserv-kill-quietly t)" -f gnuserv-start &
#emacs -name "erc" -f irc &
#firefox &
#mozilla-thunderbird &
#x-terminal-emulator &
exec gnome-session
#exec fluxbox
