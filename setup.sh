#!/bin/sh

# config
ln -sfv $PWD/vim/vimrc $HOME/.vimrc
ln -sfv $PWD/tmux/tmux.conf $HOME/.tmux.conf
ln -sfv $PWD/git $HOME/.config/
ln -sfv $PWD/bin $HOME/.local/

# Applications
# ln -sfv $PWD/apps/firefox-private.desktop $HOME/.local/share/applications/
ln -sfv $PWD/apps/emacs.desktop $HOME/.local/share/applications/
ln -sfv $PWD/apps/emacs-term.desktop $HOME/.local/share/applications/
ln -sfv $PWD/apps/spacemacs.desktop $HOME/.local/share/applications/
ln -sfv $PWD/apps/spacemacs-term.desktop $HOME/.local/share/applications/

# Fonts
ln -sfv $PWD/fonts/Fira $HOME/.local/share/fonts/
ln -sfv $PWD/fonts/NYFonts $HOME/.local/share/fonts/
ln -sfv $PWD/fonts/SanFranciscoPro $HOME/.local/share/fonts/
ln -sfv $PWD/fonts/SFMonoFonts $HOME/.local/share/fonts/

# zsh
[ ! -d $HOME/.config/zsh ] && mkdir $HOME/.config/zsh
[ ! -f $HOME/.config/zsh/zshrc ] && wget -O $HOME/.config/zsh/zshrc https://git.grml.org/f/grml-etc-core/etc/zsh/zshrc
[ ! -f $HOME/.config/zsh/git ] && wget -O $HOME/.config/zsh/git https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/plugins/git/git.plugin.zsh
[ ! -f $PWD/zsh/git-plugin.md ] && wget -O $PWD/zsh/git-plugin.md https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/plugins/git/README.md
ln -sfv $PWD/zsh/zshrc.local $HOME/.zshrc.local
ln -sfv $HOME/.config/zsh/zshrc $HOME/.zshrc

# emacs flavour
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
cp -v $HOME/.local/bin/spacemacs $HOME/.local/bin/spacemacs-term
sed -i 's/\/usr\/bin\/emacs/\/usr\/bin\/emacs -nw/g' $HOME/.local/bin/spacemacs-term

# clone spacemacs
[ ! -d $HOME/.config/spacemacs-master ] && git clone -b master https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-master
[ ! -d $HOME/.config/spacemacs-develop ] && git clone -b develop https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-develop
