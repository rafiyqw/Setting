#!/bin/sh

ln -sfv $PWD/zshrc /home/$USER/.zshrc
ln -sfv $PWD/vimrc /home/$USER/.vimrc
ln -sfv $PWD/tmux.conf /home/$USER/.tmux.conf

# emacs
rm -rf emacs/vanilla.sh
touch emacs/vanilla.sh
chmod +x emacs/vanilla.sh
echo "#!/bin/sh" > emacs/vanilla.sh
echo "rm -rf ~/.emacs.d" >> emacs/vanilla.sh
echo "ln -sfv $PWD/emacs ~/.emacs.d" >> emacs/vanilla.sh

# spacemacs
rm -rf spacemacs/spacemacs.sh
touch spacemacs/spacemacs.sh
chmod +x spacemacs/spacemacs.sh
echo "#!/bin/sh" > spacemacs/spacemacs.sh
echo "rm -rf ~/.emacs.d" >> spacemacs/spacemacs.sh
echo "ln -sfv ~/.config/spacemacs ~/.emacs.d" >> spacemacs/spacemacs.sh
echo "ln -sfv $PWD/spacemacs/spacemacs-master ~/.spacemacs" >> spacemacs/spacemacs.sh

git clone https://github.com/syl20bnr/spacemacs ~/.config/spacemacs

