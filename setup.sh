#!/bin/sh

ln -sfv $PWD/vim/vimrc ~/.vimrc
ln -sfv $PWD/tmux/tmux.conf ~/.tmux.conf
ln -sfv $PWD/bin ~/.local/bin
ls -sfv $PWD/apps ~/.local/share/applications

# zsh
[ ! -f $PWD/zsh/zshrc ] && wget -O $PWD/zsh/zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
ln -sfv $PWD/zsh/zshrc $HOME/.zshrc
ln -sfv $PWD/zsh/zshrc.local $HOME/.zshrc.local

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
ln -sfv $PWD/emacs/emacs.sh ~/.local/bin/

# spacemacs-master
tee -a emacs/spacemacs-master.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
ln -sfv ~/.config/spacemacs-master ~/.emacs.d
ln -sfv $PWD/emacs/spacemacs-master.el ~/.spacemacs
END
ln -sfv $PWD/emacs/spacemacs-master.sh ~/.local/bin/

# spacemacs-develop
tee -a emacs/spacemacs-develop.sh << END
#!/bin/sh
rm -rf ~/.emacs.d
ln -sfv ~/.config/spacemacs-develop ~/.emacs.d
ln -sfv $PWD/emacs/spacemacs-develop.el ~/.spacemacs
END
ln -sfv $PWD/emacs/spacemacs-develop.sh ~/.local/bin/

[ ! -d "~/.config/spacemacs-master" ] && git clone -b master https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-master
[ ! -d "~/.config/spacemacs-develop" ] && git clone -b develop https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-develop
