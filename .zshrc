#
# Create some "standard" dirs, if needed
#
mkdir -p ~/.private/$HOST

# Link private helper scripts into ~/bin
priv_cbin="$HOME/.private/common/bin"
if [ -d "$priv_cbin" ] && [ `ls -1 $priv_cbin | wc -l` -gt 0 ]; then
    for x in ~/.private/common/bin/*; do
        name=`basename $x`
        if ! [ -e ~/bin/$name ]; then
            ln -s ../.private/common/bin/$name ~/bin/
        fi
    done
fi

#
# History
#
HISTFILE=$HOME/.private/$HOST/zsh_history
HISTSIZE=40000
SAVEHIST=40000

setopt HIST_IGNORE_DUPS
setopt EXTENDED_HISTORY
setopt APPEND_HISTORY
setopt SHARE_HISTORY

# UTF-8 for gnome-terminal
case "$COLORTERM" in
  gnome-terminal)
	echo -en '\e%G'
	;;
esac

alias la='ls -lA'
alias hr='hash -r'
alias recd='cd `pwd`'
alias a='sudo aptitude install'
alias dl='curl -L -O'
alias s='screen -x -R'
alias svn-cd='svn diff | colordiff | less -R'

alias -g _L_='2>&1 | tee ,log'

# Host-Specific stuff
if [ -f ~/.zshrc-`hostname` ]; then
  . ~/.zshrc-`hostname`
fi


#
# KeyChain
#
if command -v keychain > /dev/null; then
    eval `keychain --eval id_rsa`
fi

#
# Prompt
#
autoload colors
colors

if [ -f /etc/system_info ]; then
    . /etc/system_info
fi

if [ "$TERM" = dumb ]; then
    PS1='$ '
    unsetopt zle
else
    if [ -n "$SYSTEM_NAME" ]; then
        sysprompt="($SYSTEM_NAME)"
    else
        sysprompt=""
    fi
    PS1="%{${fg[yellow]}%}%n%{${fg[default]}%}@%{${fg[magenta]}%}%m%{${fg[default]}%}$sysprompt:%{${fg[cyan]}%}%~%{${fg[default]}%}%# "
fi

case $TERM in
  xterm*|rxvt*)
	precmd () { print -Pn "\e]0;%m:%~\a" }
	;;
esac

#
# Conviniency
#
setopt AUTO_CD
setopt MULTIOS
setopt INTERACTIVE_COMMENTS
setopt NO_HUP
setopt NO_NOMATCH

if command -v dircolors > /dev/null; then
  eval `dircolors`
fi
alias ls='ls --color=auto'
alias ll='ls -l'
alias pd='pushd'

if command -v lesspipe > /dev/null; then
  eval `lesspipe`
fi
export LESS="-MR"

#
# Key bindings
#
bindkey "^[[3~"   delete-char		# xterm
bindkey "^[[1~"   beginning-of-line	# Linux console
bindkey "^[OH"    beginning-of-line	# xterm
bindkey "^[[H"    beginning-of-line	# xterm
bindkey "^[[4~"   end-of-line		# Linux console
bindkey "^[OF"    end-of-line		# xterm
bindkey "^[[F"    end-of-line		# xterm
bindkey "\M\b" 	  backward-delete-word	# xterm
bindkey "^[[5D"   backward-word		# xterm
bindkey "^[[5C"   forward-word		# xterm
bindkey "ÿ"	  backward-delete-word	# xterm (silly)
bindkey "^[[3;3~" delete-word           # xterm
bindkey "^[[1;5D" backward-word		# xterm XFree86 4.3
bindkey "^[[1;5C" forward-word		# xterm XFree86 4.3
bindkey "^[O5D"   backward-word		# screen
bindkey "^[O5C"   forward-word		# screen

# mess support
function mess {
  DIR=`~/bin/mess.rb "$@"`
  [[ $? -eq 0 ]] && cd "$DIR"
}

# unique, exported, tied array of $rubylib to colon-seperated $RUBYLIB
typeset -T -U -gx -a RUBYLIB rubylib ':'
rubylib-add()   { rubylib+=("$@") }
rubylib-del()   { for i ("$@") { rubylib[(r)$i]=() } }
rubylib-reset() { rubylib=(); [[ -f ~/.rubylib ]] && source ~/.rubylib }
rubylib-reset


if [ "$TERM" != dumb ]; then
    #
    # unison completion
    #
    autoload _unison
    zstyle -e ':compiletion:*:unison:*' completer _unison

    #
    # TLA
    # autoload _tla
    zstyle -e ':completion:*:tla:*' completer _tla

    # Activate completion
    zstyle ':completion:*' completer _complete
    zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'

    # Use a cache
    zstyle ':completion:*' use-cache on
    zstyle ':completion:*' cache-path ~/.zsh/cache

    # Ignore completion functions for commands you don't have
    zstyle ':completion:*:functions' ignored-patterns '_*'

    autoload -U compinit
    compinit
fi
