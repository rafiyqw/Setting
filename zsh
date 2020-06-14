# Z shell configuration file.

# History
HISTFILE=~/.cache/zsh/history
HISTSIZE=500
SAVEHIST=1000
setopt HIST_EXPIRE_DUPS_FIRST       # expire duplicates first
setopt HIST_IGNORE_DUPS             # do not store duplications
setopt HIST_FIND_NO_DUPS            # ignore duplicates when searching
setopt HIST_REDUCE_BLANKS           # removes blank lines from history

# Colors and Prompt:
#autoload -U colors && colors
PROMPT='%B%F{171}%n%F{135}@%F{99}%M%b %F{81}%~ %f%# '
setopt autocd                       # directly change directory 
bindkey -e                          # emacs style key
setopt NO_CASE_GLOB                 # case-insensitive globbing

# Autocomplete
zstyle :compinstall filename '/home/rafiyq/.zshrc'
autoload -Uz compinit
compinit

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -AlFh'
alias la='ls -A'
alias l='ls -CF'

# more aliases
alias ec='emacsclient -create-frame --alternate-editor=""'

