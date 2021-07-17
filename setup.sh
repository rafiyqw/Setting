#!/bin/sh

# config
ln -sfvi $PWD/vim/vimrc $HOME/.vimrc
ln -sfvi $PWD/tmux/tmux.conf $HOME/.tmux.conf
ln -sfvi $PWD/git $HOME/.config/
ln -sfvi $PWD/zsh $HOME/.config/
ln -sfvi $PWD/nvim $HOME/.config/
ln -sfvi $PWD/bin $HOME/.local/

# Applications
# ln -sfv $PWD/apps/firefox-private.desktop $HOME/.local/share/applications/
#ln -sfv $PWD/apps/emacs.desktop $HOME/.local/share/applications/
#ln -sfv $PWD/apps/emacs-term.desktop $HOME/.local/share/applications/
#ln -sfv $PWD/apps/spacemacs.desktop $HOME/.local/share/applications/
#ln -sfv $PWD/apps/spacemacs-term.desktop $HOME/.local/share/applications/

# zsh
#[ ! -d $HOME/.config/zsh ] && mkdir $HOME/.config/zsh
#[ ! -f $HOME/.config/zsh/zshrc ] && wget -O $HOME/.config/zsh/zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
#[ ! -f $HOME/.config/zsh/git ] && wget -O $HOME/.config/zsh/git https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/plugins/git/git.plugin.zsh
#[ ! -f $PWD/zsh/git-plugin.md ] && wget -O $PWD/zsh/git-plugin.md https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/plugins/git/README.md
#ln -sfv $PWD/zsh/zshrc.local $HOME/.zshrc.local
#ln -sfv $HOME/.config/zsh/zshrc $HOME/.zshrc
$PWD/zsh/grml-zsh.sh

