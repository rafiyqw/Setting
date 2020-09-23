#!/bin/sh

# emacs
mkdir -pv /home/$USER/.config/emacs
ln -sfiv $PWD/emacs/early-init.el /home/$USER/.emacs.d/early-init.el
ln -sfiv $PWD/emacs/init.el /home/$USER/.emacs.d/init.el

ln -sfiv $PWD/zshrc /home/$USER/.zshrc
ln -sfiv $PWD/vimrc /home/$USER/.vimrc
ln -sfiv $PWD/tmux.conf /home/$USER/.tmux.conf
