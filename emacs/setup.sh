#!/bin/sh
spacemacs_url="https://github.com/syl20bnr/spacemacs"
spacemacs_path="$HOME/.config/spacemacs/master"

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
[ ! -d $spacemacs_path/master] && git clone -b master $spacemacs_url $spacemacs_path/master
[ ! -d $spacemacs_path/develop ] && git clone -b develop $spacemacs_url $spacemacs_path/develop
