#!/usr/bin/env bash

cp -rf ./.scripts/ ~/
cp -rf ./.emacs.d/ ~/
cp ./.xinitrc      ~/.xinitrc
cp ./.fehbg        ~/.fehbg
cp ./.tmux.conf    ~/.tmux.conf
cp ./.vimrc        ~/.vimrc
cp .zshrc          ~/.zshrc

echo "Done."
