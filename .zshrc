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
export PATH=/opt/gcc-arm-none-eabi-9-2020-q2-update/bin/:$PATH
export PATH=/opt/arm-none-eabi-gdb-9.2/bin/:$PATH
export PATH=/opt/ARM_Compiler_5.06u6/bin/:$PATH

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib/

# ZEPHYR
export GNUARMEMB_TOOLCHAIN_PATH=/opt/gcc-arm-none-eabi-9-2020-q2-update
export ZEPHYR_TOOLCHAIN_VARIANT=gnuarmemb
export ZEPHYR_BASE=~/ncs/zephyr

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
  find . -type f -exec sed -i {} $@ \;
}

export CTEST_PARALLEL_LEVEL=$(nproc)
export `gnome-keyring-daemon`

alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias fvim='fzf|xargs -i{} vim {}'
alias dragoon="cd /home/jam/Documents/dragoon"
alias sdk="cd /home/jam/Documents/sdk"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# --files: List files that would be searched but do not search
# --no-ignore: Do not respect .gitignore, etc...
# --hidden: Search hidden files and folders
# --follow: Follow symlinks
# --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
