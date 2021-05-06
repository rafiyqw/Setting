#!/bin/sh

ln -sfv $PWD/vim/vimrc $HOME/.vimrc
ln -sfv $PWD/tmux/tmux.conf $HOME/.tmux.conf
ln -sfv $PWD/bin $HOME/.local/

# apps
# ln -sfv $PWD/apps/firefox-private.desktop $HOME/.local/share/applications/
# ln -sfv $PWD/apps/emacs.desktop $HOME/.local/share/applications/
# ln -sfv $PWD/apps/spacemacs-dev.desktop $HOME/.local/share/applications/
# ln -sfv $PWD/apps/spacemacs-main.desktop $HOME/.local/share/applications/

# zsh
[ ! -f $PWD/zsh/zshrc ] && wget -O $PWD/zsh/zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
ln -sfv $PWD/zsh/zshrc $HOME/.zshrc
ln -sfv $PWD/zsh/zshrc.local $HOME/.zshrc.local

# emacs flavor
tee $HOME/.local/bin/spacemacs << END
#!/bin/sh
rm -rf ~/.emacs.d
case \$1 in
    "emacs")
        mkdir -pv ~/.config/emacs
        ln -sfv $PWD/emacs/early-init.el ~/.config/emacs/
        ln -sfv $PWD/emacs/init.el ~/.config/emacs/
        ln -sfv ~/.config/emacs ~/.emacs.d
        ;;
    "master")
        ln -sfv ~/.config/spacemacs-master ~/.emacs.d
        ln -sfv $PWD/emacs/spacemacs-master.el ~/.spacemacs
        ;;
    "develop")
        ln -sfv ~/.config/spacemacs-develop ~/.emacs.d
        ln -sfv $PWD/emacs/spacemacs-develop.el ~/.spacemacs
        ;;
esac
/usr/bin/emacs
END
chmod +x $HOME/.local/bin/spacemacs

# clone spacemacs
[ ! -d $HOME/.config/spacemacs-master ] && git clone -b master https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-master
[ ! -d $HOME/.config/spacemacs-develop ] && git clone -b develop https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-develop
