#!/bin/bash

# detect os
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
echo "Running $1 on ${machine}"


function not_implemented {
    echo "not implemented"
    exit
}

function help_impl {
    echo "usage: $0 help|zsh|ycm|vim|bootstrap|all"
    exit
}

# check if brew is installed on mac
function bootstrap_impl {
    case "${machine}" in
        Linux)  apt-get install git;;
        Mac)    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" && brew install git;;
        *)      not_implemented
    esac

    git clone --recurse-submodule https://github.com/Tschet1/configs.git
    mv configs/.[a-zA-Z]* ~/
    rm -r configs
}

function vim_impl {
    case "${machine}" in
        Linux)  apt-get install vim python3;;
        Mac)    brew install vim python3;;
        *)      not_implemented
    esac

    vim +PluginInstall +qall
}

function zsh_impl {
    case "${machine}" in
        Linux)  apt-get install git zsh;;
        Mac)    brew install git zsh;;
        *)      not_implemented
    esac
    chsh -s /bin/zsh
    git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
}

function ycm_impl {
    case "${machine}" in
        Linux)  not_implemented;;
        Mac)    brew install git zsh vim ctags cmake npm yarn python3;;
        *)      not_implemented
    esac

    # install rust
    mkdir  ~/.zfunc
    rustup completions zsh > ~/.zfunc/_rustup

    # install you complete me
    cd ~
    mkdir ycm_build
    cd ycm_build
    cmake -DPYTHON_LIBRARY=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/lib/libpython.dylib -DPYTHON_INCLUDE_DIR=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/include/python3.7m -G "Unix Makefiles" -DUSE_SYSTEM_LIBCLANG=ON -DUSE_PYTHON2=off . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
    cmake --build . --target ycm_core --config Release
    cd ~
    mkdir regex_build
    cd regex_build
    cmake -DPYTHON_LIBRARY=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/lib/libpython.dylib -DPYTHON_INCLUDE_DIR=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/include/python3.7m -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/third_party/cregex
    cmake --build . --target _regex --config Release

    # add js support
    npm install -g typescript

    # add rust support
    rustup component add rust-src
    cd ~/.vim/bundle/YouCompleteMe/third_party/ycmd/third_party/racerd
    cargo build --release
    cd ~
}

case "$1" in
    help|-h|--help)     help_impl;;
    zsh|-z|--zsh)       zsh_impl;;
    ycm|-y|--ycm)       ycm_impl;;
    vim|-v|--vim)       vim_impl;;
    bootstrap|-b|--bootstrap)    bootstrap_impl;;
    all|-a|--all)       bootstrap_impl && zsh_impl && vim_impl && ycm_impl;;
    *)                  help_impl
esac
