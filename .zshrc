# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="afowler"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)
plugins=(zsh-autosuggestions)
source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=4'
ZSH_AUTOSUGGEST_STRATEGY=match_prev_cmd
#ZSH_AUTOSUGGEST_USE_ASYNC=1

# have a per tab history
unsetopt inc_append_history
unsetopt share_history

alias homepage="cd ~/Documents/Corps-Page; vagrant up"
alias loadKey="ssh-add ~/.ssh/id_rsa"
alias open="xdg-open"

PATH="$PATH:/opt/bin:/opt/local/bin:/usr/local/bin:~/personalScripts"
fpath+=~/.zfunc

export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/home/jam/.local/bin:$PATH"

# tmux
function tmux-cd {
    tmux command-prompt -I $PWD -p "New session dir:" "attach -c %1"
 }

# we want english and not crappy translated CLI
export LANG=en_US.UTF-8

# set vim as standard texteditor
export VISUAL=vim
export EDITOR="$VISUAL"

DISABLE_UNTRACKED_FILES_DIRTY="true"
#function git_prompt_info() {
#  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
#  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$ZSH_THEME_GIT_PROMPT_SUFFIX"
#}
export CMSIS_PATH=/opt/CMSIS_5.6.0/CMSIS
export PATH=/opt/mergehex/:/opt/nrfjprog/:$PATH
export PATH=/opt/lint/:$PATH
#export PATH=/opt/gcc-arm-none-eabi-8-2018-q4-major/arm-none-eabi/bin:/opt/gcc-arm-none-eabi-8-2018-q4-major/bin:$PATH
export ARM_TOOL_VARIANT=mdk_pro_flex
export ARMLMD_LICENSE_FILE=1714@192.9.200.249
#export PATH=/opt/gcc-arm-none-eabi-8-2018-q4-major/bin/:$PATH
#export PATH=/opt/gcc-arm-none-eabi-9-2020-q2-update/bin/:$PATH
#export PATH=/opt/gcc-arm-none-eabi-10-2020-q4-major/bin/:$PATH
export PATH=/opt/gcc-arm-none-eabi-10.3-2021.10/bin/:$PATH

export PATH=$PATH:/opt/SEGGER/SystemView_V332
export PATH=$PATH:/usr/share/segger_embedded_studio_for_arm_5.32a/bin/
export PATH=$PATH:/home/jam/.config/coc/extensions/coc-clangd-data/install/11.0.0/clangd_11.0.0/bin/

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib/

# ZEPHYR
export GNUARMEMB_TOOLCHAIN_PATH=/opt/gcc-arm-none-eabi-10.3-2021.10
export ZEPHYR_TOOLCHAIN_VARIANT=gnuarmemb
export ZEPHYR_BASE=/home/jam/Documents/ncs_ws/zephyr

# nordic shortcuts
function findelf() {
  find ~+ -name "$1.elf"
}
function buildntest() {
  cd build_gcc
  ninja $1 && ctest -R $@ --output-on-failure --interactive-debug-mode
  cd ..
}
function sedallfilesinthisdirectory() {
  find . -name .git -prune -o -type f -exec sed -i {} $@ \;
}

function build_reflector() {
  ninja

  if [ $# -ne 0 ];
  then
    segger="-s $1"
  else
    segger=""
  fi
  find . -type f -name "tp_reflector_*.hex" -exec bash -c "echo 'flash {}'; nrfjprog --eraseall $segger && nrfjprog --program {} $segger && nrfjprog --reset $segger" \;
}

function build_initiator() {
  ninja
  
  if [ $# -ne 0 ];
  then
    segger="-s $1"
  else
    segger=""
  fi
  find . -type f -name "tp_initiator_*.hex" -exec bash -c "echo 'flash {}'; nrfjprog --eraseall $segger && nrfjprog --program {} $segger && nrfjprog --reset $segger" \;
}

function build_reflector_53() {
  ninja

  if [ $# -ne 0 ];
  then
    segger="-s $1"
  else
    segger=""
  fi
  
  bash -c "nrfjprog -f NRF53 --coprocessor CP_NETWORK --recover $segger && nrfjprog -f NRF53 --coprocessor CP_APPLICATION --recover $segger"
  
  find . -type f -name "tp_reflector_*.hex" -exec bash -c "echo 'flash {}'; nrfjprog -f NRF53 --coprocessor CP_NETWORK --chiperase --program {} $segger" \;
  find . -type f -name "*dummy_app_core.hex" -exec bash -c "echo 'flash {}'; nrfjprog -f NRF53 --coprocessor CP_APPLICATION --chiperase --program {} $segger" \;

  bash -c "nrfjprog --reset $segger"
}

function build_initiator_53() {
  ninja

  if [ $# -ne 0 ];
  then
    segger="-s $1"
  else
    segger=""
  fi
  
  bash -c "nrfjprog -f NRF53 --coprocessor CP_NETWORK --recover $segger && nrfjprog -f NRF53 --coprocessor CP_APPLICATION --recover $segger"
 
  find . -type f -name "tp_initiator_*.hex" -exec bash -c "echo 'flash {}'; nrfjprog -f NRF53 --coprocessor CP_NETWORK --chiperase --program {} $segger" \;
  find . -type f -name "*dummy_app_core.hex" -exec bash -c "echo 'flash {}'; nrfjprog -f NRF53 --coprocessor CP_APPLICATION --chiperase --program {} $segger" \;

  bash -c "nrfjprog --reset $segger"
}

function flash_hex_on_dev() {
  nrfjprog --eraseall -s $2 && nrfjprog --program $1 -s $2 && nrfjprog --reset -s $2
}

function flash_hexes_on_53()
{
  nrfjprog -f NRF53 --coprocessor CP_NETWORK --recover -s $3
  nrfjprog -f NRF53 --coprocessor CP_APPLICATION --recover -s $3
  nrfjprog -f NRF53 --coprocessor CP_NETWORK --chiperase --program $1 -s $3
  nrfjprog -f NRF53 --coprocessor CP_APPLICATION --chiperase --program $2 -s $3
  nrfjprog --reset -s $3
}

alias reset_all='nrfjprog -i|xargs -P8 -I{} nrfjprog -r -s {}'

export CTEST_PARALLEL_LEVEL=$(nproc)
export `gnome-keyring-daemon`

alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias fvim='fzf|xargs -i{} vim {}'
alias dragoon="cd /home/jam/Documents/dragoon"
alias sdk="cd /home/jam/Documents/sdk"
alias branchlog="git for-each-ref --sort=-committerdate:iso8601 --format='%(committerdate:iso8601)%09%(refname)' refs/heads"

alias sizeofdh="ninja dh > /dev/null && arm-none-eabi-size stack/projects/dh/build/dh_s140.elf"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# --files: List files that would be searched but do not search
# --no-ignore: Do not respect .gitignore, etc...
# --hidden: Search hidden files and folders
# --follow: Follow symlinks
# --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export NRFXLIB_PATH=/home/jam/Documents/ncs_ws/nrfxlib
export PATH=${HOME}/gn:"$PATH"
#export PATH=/opt/clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04/bin:"$PATH"
export PATH="$PATH":/home/jam/.docker/cli-plugins
export CMAKE_C_COMPILER_LAUNCHER ccache
export CTR_PATH=${HOME}/Documents/ncs_ws/test_ble

# armclang
#export PATH=/opt/ArmCompilerforEmbedded6.18/bin/:"$PATH"
#export ARM_PRODUCT_PATH=/opt/ArmCompilerforEmbedded6.18/sw/mappings/

export PATH="$PATH":/opt/ARM_Compiler_5.06u6/bin/

function git_n_last_used_branches()
{
  git for-each-ref --sort='-committerdate:iso8601' --format=' %(committerdate:iso8601)%09%(refname)' refs/heads|head -n $1|sed -e 's/refs\/heads\///g'
}

alias git_checkout_from_last_used_branches='git_n_last_used_branches 30 | fzf | grep -o "\s[^\s]*$" | xargs git checkout'


# start ssh agent
# is this an interactive shell?
if [[ $- == *i* ]]; then
    # set up ssh key server
    if [[ -x /usr/bin/keychain ]]; then
        eval $(keychain --eval --ignore-missing ~/.ssh/id_rsa ~/.ssh/id_ed25519)
    fi
fi
