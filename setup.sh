#!/bin/sh

ln -sfv $PWD/zshrc /home/$USER/.zshrc
ln -sfv $PWD/vimrc /home/$USER/.vimrc
ln -sfv $PWD/tmux.conf /home/$USER/.tmux.conf

# emacs & spacemacs
rm -rf emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
touch emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
chmod +x emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
echo "#!/bin/sh" > emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
echo "rm -rf ~/.emacs.d" >> emacs/vanilla.sh spacemacs/spacemacs.sh spacemacs/spacemacs-dev.sh
# vanilla emacs
echo "ln -sfv $PWD/emacs ~/.emacs.d" >> emacs/vanilla.sh
# spacemacs
echo "ln -sfv ~/.config/spacemacs ~/.emacs.d" >> spacemacs/spacemacs.sh
echo "ln -sfv $PWD/spacemacs/spacemacs-master ~/.spacemacs" >> spacemacs/spacemacs.sh
# spacemacs-develop
echo "ln -sfv ~/.config/spacemacs-dev ~/.emacs.d" >> spacemacs/spacemacs.sh
#echo "ln -sfv $PWD/spacemacs/spacemacs-dev ~/.spacemacs" >> spacemacs/spacemacs.sh

git clone https://github.com/syl20bnr/spacemacs ~/.config/spacemacs
git clone -b develop https://github.com/syl20bnr/spacemacs ~/.config/spacemacs-dev

