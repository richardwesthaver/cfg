# .bashrc --- bash configuration 
# Set prompt
PS1="\u [\!]:\t:\w\n  >> \[\e[0m\]"
# default Envs
export ESHELL='/usr/bin/bash'
export ORGANIZATION='anticorp'
export TERM='xterm-256color'
export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=''
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -c'

# custom configs
export FREESOUND_CONFIG="~/.config/freesound.json"

# aliases
eman() {
    emacsclient -c -e "(man \"$1\")"
}

alias ec='emacsclient -c'
alias et='emacsclient -t'

alias hgpu='hg pull -u'
alias hgc='hg ci -m'
alias hgp='hg push'
alias hgfe='~/bin/sh/hg-fast-export.sh'
# VCS
alias hgsub='find . -name ".hg" -type d | grep -v "\./\.hg" | xargs -n1 dirname | xargs -iREPO hg -R REPO'

alias q='QHOME=~/q rlwrap -r ~/q/l64/q'
alias ..='cd ..'
complete -c man which