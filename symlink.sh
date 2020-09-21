#!/bin/sh

# emacs
mkdir -p /home/$USER/.config/emacs
ln -s ./emacs/early-init.el /home/$USER/.config/emacs/early-init.el
ln -s ./emacs/init.el /home/$USER/.config/emacs/init.el

ln -s /home/$USER/myFiles/zshrc /home/$USER/.zshrc
ln -s /home/$USER/myFiles/tmux.conf /home/$USER/.tmux.conf
