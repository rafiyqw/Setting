#!/bin/sh

# emacs
mkdir -p /home/$USER/.config/emacs
ln -sf $PWD/emacs/early-init.el /home/$USER/.config/emacs/early-init.el
ln -sf $PWD/emacs/init.el /home/$USER/.config/emacs/init.el

ln -sf $PWD/zshrc /home/$USER/.zshrc
ln -sf $PWD/tmux.conf /home/$USER/.tmux.conf
