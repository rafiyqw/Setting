# Prompt
PROMPT='%B%F{blue}%2~ %(?.%F{214}%(!.#.>).%F{red}%(!.#.>))%b%f '

# Save history 
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=$HOME/.cache/zsh/history

# Options
setopt auto_cd                # cd by typing directory name if it's not a command
setopt auto_list              # automatically list choices on ambiguous completion
setopt auto_menu              # automatically use menu completion
setopt always_to_end          # move cursor to end if word had one match
setopt hist_ignore_all_dups   # remove older duplicate entries from history
setopt hist_reduce_blanks     # remove superfluous blanks from history items
setopt inc_append_history     # save history entries as soon as they are entered
setopt share_history          # share history between different instances
setopt correct_all            # autocorrect commands
setopt interactive_comments   # allow comments in interactive shells

# Completion
autoload -Uz compinit && compinit
zmodload -i zsh/complist
zstyle ':completion:*' menu select      # select completions with arrow keys
zstyle ':completion:*' list-colors ''

# case insensitive path-completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
_comp_options+=(globdots)		        # Include hidden files.

# Git
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT='${vcs_info_msg_0_}'
zstyle ':vcs_info:*' check-for-changes false
zstyle ':vcs_info:*' check-for-staged-changes true

zstyle ':vcs_info:git:*' formats "%b %m%u%c"
#zstyle ':vcs_info:git:*' formats "%r/%S %b %m%u%c"
zstyle ':vcs_info:*' enable git

# Load aliases and shortcuts if existent.
[ -f "$HOME/.config/zsh/aliases" ] && source "$HOME/.config/zsh/aliases"
[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"
