#!/usr/bin/env bash

# copy dotfiles from this directory
# to the directory it's supposed to be
cp -rf ./.scripts/ ~/
cp -rf ./.emacs.d/ ~/
cp ./.xinitrc      ~/.xinitrc
cp ./.fehbg        ~/.fehbg
cp ./.tmux.conf    ~/.tmux.conf
cp ./.vimrc        ~/.vimrc
cp .zshrc          ~/.zshrc

# install leyl theme into emacs
git clone https://github.com/m1cr0xf7/leyl
cp leyl/leyl-theme.el ~/.emacs.d/themes/


echo "Done."
