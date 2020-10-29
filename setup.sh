#!/bin/sh

ln -sfv $PWD/zshrc /home/$USER/.zshrc
ln -sfv $PWD/vimrc /home/$USER/.vimrc
ln -sfv $PWD/tmux.conf /home/$USER/.tmux.conf

# emacs & spacemacs
rm -rf emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
touch emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
chmod +x emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
# vanilla emacs
echo "#!/bin/sh" > emacs/vanilla.sh
echo "rm -rf ~/.emacs.d" >> emacs/vanilla.sh
echo "ln -sfv $PWD/emacs ~/.emacs.d" >> emacs/vanilla.sh
# spacemacs
echo "#!/bin/sh" > spacemacs/spacemacs.sh
echo "rm -rf ~/.emacs.d" >> spacemacs/spacemacs.sh
echo "ln -sfv ~/.config/spacemacs ~/.emacs.d" >> spacemacs/spacemacs.sh
echo "ln -sfv $PWD/spacemacs/spacemacs ~/.spacemacs" >> spacemacs/spacemacs.sh
# spacemacs-develop
echo "#!/bin/sh" > spacemacs/spacemacs-dev.sh
echo "rm -rf ~/.emacs.d" >> spacemacs/spacemacs-dev.sh
echo "ln -sfv ~/.config/spacemacs-dev ~/.emacs.d" >> spacemacs/spacemacs-dev.sh
echo "ln -sfv $PWD/spacemacs/spacemacs-dev ~/.spacemacs" >> spacemacs/spacemacs-dev.sh

git clone https://github.com/syl20bnr/spacemacs ~/.config/spacemacs
git clone -b develop https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-dev

