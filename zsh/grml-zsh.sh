#!/bin/sh

zsh_path="$HOME/.config/zsh"
grml_url="https://git.grml.org/f/grml-etc-core/etc"
plugins="$HOME/.config/zsh/plugins"

echo "export ZDOTDIR=~/.config/zsh" >> $HOME/.zprofile

#[ ! -d ~/.config/zsh ] && mkdir ~/.config/zsh
wget -O $zsh_path/.zshrc $grml_url/zsh/zshrc
wget -O $zsh_path/.zshrc.local $grml_url/skel/.zshrc

## Plugins
[ ! -d $plugins ] && mkdir $plugins
wget -O $plugins/git.zsh https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/plugins/git/git.plugin.zsh
echo "source $HOME/.config/zsh/aliases" >> $HOME/.config/zsh/.zshrc.local
echo "source $plugins/git.zsh" >> $HOME/.config/zsh/.zshrc.local

