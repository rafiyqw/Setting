#!/bin/sh

ln -sfv $PWD/zshrc /home/$USER/.zshrc
ln -sfv $PWD/vimrc /home/$USER/.vimrc
ln -sfv $PWD/tmux.conf /home/$USER/.tmux.conf

# emacs & spacemacs
rm -rf emacs/emacs.sh emacs/spacemacs-master.sh emacs/spacemacs-develop.sh
touch emacs/emacs.sh emacs/spacemacs-master.sh emacs/spacemacs-develop.sh
chmod +x emacs/emacs.sh emacs/spacemacs-master.sh emacs/spacemacs-develop.sh

# emacs
tee -a emacs/emacs.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
mkdir -pv ~/.config/emacs
ln -sfv $PWD/emacs/early-init.el ~/.config/emacs/
ln -sfv $PWD/emacs/init.el ~/.config/emacs/
ln -sfv ~/.config/emacs ~/.emacs.d
END

# spacemacs-master
tee -a emacs/spacemacs-master.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
ln -sfv ~/.config/spacemacs-master ~/.emacs.d
ln -sfv $PWD/emacs/spacemacs-master ~/.spacemacs
END

# spacemacs-develop
tee -a emacs/spacemacs-develop.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
ln -sfv ~/.config/spacemacs-develop ~/.emacs.d
ln -sfv $PWD/emacs/spacemacs-develop ~/.spacemacs
END

[ ! -d "~/.config/spacemacs-master" ] && git clone -b master https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-master || echo "directory ~/.config/spacemacs-master exist!"

[ ! -d "~/.config/spacemacs-develop" ] && git clone -b develop https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-develop || echo "directory ~/.config/spacemacs-develop exist!"
