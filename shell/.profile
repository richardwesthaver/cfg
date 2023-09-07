# .bash_profile --- bash profile

export PYTHON=python3.11
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$HOME/go/bin:$HOME/.nimble/bin:$HOME/.cargo/bin:$HOME/.local/bin:$HOME/bin:$PATH"
export PATH="$HOME/bin/sh:$HOME/bin/py:$HOME/bin/hs:$HOME/bin/ps:$HOME/.roswell/bin:$PATH"
export FREESOUND_CONFIG="$HOME/.config/freesound.json"

source ~/.bashrc
source ~/.bash_login

if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

eval "$(pyenv init --path)"
if command -v rhg>>/dev/null; then alias hg='rhg';fi