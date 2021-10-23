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

# Rust
export PATH="$HOME/.cargo/bin:$PATH"
