# Prompt
PROMPT='%2~%f> '

#Change Directory
setopt AUTO_CD
setopt NO_CASE_GLOB
setopt EXTENDED_GLOB

#History
HISTFILE=/home/$USER/.cache/zsh_history
HISTSIZE=500
SAVEHIST=1000
setopt SHARE_HISTORY                # share history across multiple zsh sessions
setopt APPEND_HISTORY               # append to history
setopt INC_APPEND_HISTORY           # adds commands as they are typed
setopt HIST_EXPIRE_DUPS_FIRST       # expire duplicates first
setopt HIST_IGNORE_ALL_DUPS         # do not store duplications
setopt HIST_FIND_NO_DUPS            # ignore duplicates when searching
setopt HIST_REDUCE_BLANKS           # removes blank lines from history

#Correction
setopt CORRECT                      # Correct commands
setopt CORRECT_ALL                  # Correct all arguments

#Completion
# case insensitive path-completion
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
# show descriptions when autocompleting
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' format 'Completing %d'
# partial completion suggestions
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' expand prefix suffix
# list with colors
zstyle ':completion:*' list-colors ''x
# load completion
autoload -Uz compinit && compinit

#Git
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:git:*' formats '(%b)%r'
zstyle ':vcs_info:*' enable git

#Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias ll='ls -alFh'
alias la='ls -A'
alias l='ls -CF'
