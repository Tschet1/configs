#!/bin/bash

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install git zsh vim ctags
git clone --recurse-submodule https://github.com/Tschet1/configs.git
mv configs/.* ~/
rm -r configs
chsh -s /bin/zsh
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
zsh
