# .zshrc --- zsh configuration file
echo '--++--~ anticorp/ak48 2021 ~--++--'

# Set prompt
PROMPT="%l::%? %n::%~
 >> "

export MANPATH="/usr/local/man:$MANPATH"
export LANG=en_US.UTF-8
export ALTERNATE_EDITOR=''
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -c'
alias vi='emacsclient -t'
alias vim='emacsclient -t'
alias e='emacsclient -c'
alias q='QHOME=~/q rlwrap -r ~/q/m64/q'
# Rust
export PATH="$HOME/.cargo/bin:$HOME/.nimble/bin:/usr/local/bin:/usr/local/sbin:$HOME/bin:$HOME/.nimble/bin:$HOME/.roswell/bin:/usr/local/Gambit/bin:$PATH"

# completions
autoload -Uz compinit && compinit