# Z shell configuration file.

# History
HISTFILE=/home/$USER/.cache/zsh_history
HISTSIZE=500
SAVEHIST=1000
setopt SHARE_HISTORY                # share history across multiple zsh sessions
setopt APPEND_HISTORY               # append to history
setopt INC_APPEND_HISTORY           # adds commands as they are typed
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

# Use modern completion system
autoload -Uz compinit
compinit

# zstyle ':completion:*' auto-description 'specify: %d'
# zstyle ':completion:*' completer _expand _complete _correct _approximate
# zstyle ':completion:*' format 'Completing %d'
# zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select
# eval "$(dircolors -b)"
# zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# zstyle ':completion:*' list-colors ''
# zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
# zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
# zstyle ':completion:*' menu select=long
# zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
# zstyle ':completion:*' use-compctl false
# zstyle ':completion:*' verbose true

# zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
# zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

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

