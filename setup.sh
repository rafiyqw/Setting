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
ln -sfv $PWD/emacs ~/.emacs.d
END

# spacemacs
tee -a emacs/spacemacs.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
ln -sfv ~/.config/spacemacs ~/.emacs.d
ln -sfv $PWD/emacs/spacemacs ~/.spacemacs
END

# clone spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.config/spacemacs

