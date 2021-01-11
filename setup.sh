#!/bin/sh

# ln -sfv $PWD/zshrc /home/$USER/.zshrc
ln -sfv $PWD/vimrc /home/$USER/.vimrc
ln -sfv $PWD/tmux.conf /home/$USER/.tmux.conf

# emacs & spacemacs
rm -rf emacs/emacs.sh emacs/spacemacs.sh 
touch emacs/emacs.sh emacs/spacemacs.sh 
chmod +x emacs/emacs.sh emacs/spacemacs.sh 

# emacs
tee -a emacs/emacs.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
mkdir -pv ~/.config/emacs
ln -sfv $PWD/emacs/early-init.el ~/.config/emacs/
ln -sfv $PWD/emacs/init.el ~/.config/emacs/
ln -sfv ~/.config/emacs ~/.emacs.d
END

# spacemacs
tee -a emacs/spacemacs.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
ln -sfv ~/.config/spacemacs-master ~/.emacs.d
ln -sfv $PWD/emacs/spacemacs-master ~/.spacemacs
END

