# ~/.bashrc --- bash configuration 
# If not running interactively, don't do anything
[[ $- != *i* ]] && return
# Set prompt
PS1="\u [\!]:\t:\w\n  >> \[\e[0m\]"
###@! Demon Config
export CFG=$HOME/.config
export SHED=$HOME/shed
export STASH=$SHED/stash
export STAMP=$STASH/tmp
export LAB=$SHED/lab
export BABEL=$SHED/babel
# default Envs
export ESHELL='/usr/bin/bash'
export ORGANIZATION='anticorp'
export TEMP='$HOME/shed/stash/tmp'
export TERM='xterm-256color'
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=''
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -c'
# exe
export PATH="$HOME/go/bin:$HOME/.cargo/bin:$SHED/bin:$HOME/.local/bin:$PATH"
# Useful aliases
macsman() {
    emacsclient -c -e "(man \"$1\")"
}
alias eman=macsman
# IDE ;)
alias vi='emacsclient -t'
alias vim='emacsclient -t'
alias e='emacsclient -c'
# VCS
alias hgsub='find . -name ".hg" -type d | grep -v "\./\.hg" | xargs -n1 dirname | xargs -iREPO hg -R REPO'
alias ..='cd ..'
complete -c man which
