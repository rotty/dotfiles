#
# History
#
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

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

# Grep source trees
alias pyfind='find -type f -a \( -name "*.py" -o -name "*.ptl" \) | \
              xargs grep '
alias scmfind='find -type f -a \( -name "*.scm" -o -name "*.ss" \) | xargs grep '
alias cfind='find -type f  -a \( -name "*.c" -o -name "*.cpp" \
                                 -o -name "*.cc" -o -name "*.cxx" \) | \
             xargs grep '
alias hfind='find -type f  -a \( -name "*.h" -o -name "*.hh" \
                                 -o -name "*.H" -o -name "*.hxx" -o -name "*.hpp" \) | \
             xargs grep '
alias lispfind='find -type f -a -name "*.lisp" | xargs grep '
alias jfind='find -type f -a -name "*.java" -print0 | xargs -0 grep '

# Host-Specific stuff
if [ -f ~/.zshrc-`hostname` ]; then
  . ~/.zshrc-`hostname`
fi


#
# Prompt
#
autoload colors
colors

if [ "$TERM" = dumb ]; then
    PS1='$ '
    unsetopt zle
else
    PS1="%{${fg[magenta]}%}%m%{${fg[default]}%}:%{${fg[cyan]}%}%~%{${fg[default]}%}%# "
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

if which dircolors > /dev/null; then
  eval `dircolors`
fi
alias ls='ls --color=auto'
alias ll='ls -l'
alias pd='pushd'

if which lesspipe > /dev/null; then
  eval `lesspipe`
fi
export LESS="-M"

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

#
# ssh host completion
#
zstyle -e ':completion:*:(ssh|scp):*' hosts 'reply=(
    ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) \
		     /dev/null)"}%%[# ]*}//,/ }
    ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
    )'

# History
HISTFILE=$HOME/.zsh_history

# mess support
function mess {
  DIR=`~/bin/mess.rb "$@"`
  [[ $? -eq 0 ]] && cd "$DIR"
}


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
