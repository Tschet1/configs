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
        Linux)  sudo apt-get install git;;
        Mac)    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" && brew install git;;
        *)      not_implemented
    esac

    git clone --recurse-submodule https://github.com/Tschet1/configs.git
    mv configs/.[a-zA-Z]* ~/
    rm -r configs
}

function vim_impl {
    case "${machine}" in
        Linux)  sudo apt-get install vim python3;;
        Mac)    brew install vim python3;;
        *)      not_implemented
    esac

    vim +PluginInstall +qall
}

function zsh_impl {
    case "${machine}" in
        Linux)  sudo apt-get install git zsh tmux;;
        Mac)    brew install git zsh tmux;;
        *)      not_implemented
    esac
    chsh -s /bin/zsh || exit
    git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
    if [[ "${machine}" == "Mac" ]]; then
        compaudit | xargs chmod g-w,o-w
    fi

    if [[ "${machine}" == "Linux" ]]; then
        apt install xclip
        alias pbcopy='xclip -sel clip'
    fi
}

function ycm_impl {
    case "${machine}" in
        Linux)  sudo apt install curl python3-dev git zsh vim ctags cmake npm yarn python3;;
        Mac)    brew install git zsh vim ctags cmake npm yarn python3;;
        *)      not_implemented
    esac

    # install rust
    if ! which rustup > /dev/null; then
        curl https://sh.rustup.rs -sSf | sh
        export PATH="$PATH:~/.cargo/bin"
    fi
    mkdir  ~/.zfunc
    rustup completions zsh > ~/.zfunc/_rustup

    # install you complete me
    cd ~
    mkdir ycm_build
    cd ycm_build
    if [[ "${machine}" == "Mac" ]]; then
        cmake -DPYTHON_LIBRARY=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/lib/libpython3.7.dylib -DPYTHON_INCLUDE_DIR=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/include/python3.7m -G "Unix Makefiles" -DUSE_SYSTEM_LIBCLANG=ON -DUSE_PYTHON2=off . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
    else
        cmake -G "Unix Makef  iles" -DUSE_PYTHON2=off . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
    fi
    cmake --build . --target ycm_core --config Release

    cd ~
    mkdir regex_build
    cd regex_build
    if [[ "${machine}" == "Mac" ]]; then
        cmake -DPYTHON_LIBRARY=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/lib/libpython3.7.dylib -DPYTHON_INCLUDE_DIR=/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/include/python3.7m -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/third_party/cregex
    else
        cmake -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/third_party/cregex
    fi
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
