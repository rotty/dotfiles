if [ -f /etc/system_info ]; then
    . /etc/system_info
fi
if [ -f ~/.system-info ]; then
    . ~/.system_info
fi

if [ -n "$SYSTEM_NAME" ]; then
    sysdir=.system-$SYSTEM_NAME
else
    sysdir=.system
fi
