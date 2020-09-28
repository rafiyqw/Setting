#!/bin/sh

rm -rf ~/.emacs.d
ln -sfvi ~/.config/spacemacs ~/.emacs.d
ln -sfvi $PWD/spacemacs/spacemacs-master ~/.spacemacs
