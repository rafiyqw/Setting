#!/bin/sh

zsh_path="$HOME/.config/zsh"
grml_url="https://git.grml.org/f/grml-etc-core/etc"

echo "export ZDOTDIR=~/.config/zsh" >> $HOME/.zprofile

wget -O $zsh_path/.zshrc $grml_url/zsh/zshrc
wget -O $zsh_path/.zshrc.local $grml_url/skel/.zshrc

