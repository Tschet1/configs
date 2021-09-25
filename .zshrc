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
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)
plugins=(zsh-autosuggestions)
source ~/.iterm2_shell_integration.zsh
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
alias notify_on_done="terminal-notifier -title 'Task' -message 'Done'"

PATH="$PATH:/opt/bin:/opt/local/bin:/usr/local/bin:~/personalScripts"
fpath+=~/.zfunc

export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"

# we want english and not crappy translated CLI
export LANG=en_US.UTF-8

# set vim as standard texteditor
export VISUAL=vim
export EDITOR="$VISUAL"

# doom
export DOOMDIR=/Users/jan/.config/doom

# Pretty prompt
setopt prompt_subst
autoload -U colors && colors # Enable colors in prompt

# Echoes a username/host string when connected over SSH (empty otherwise)
ssh_info() {
  [[ "$SSH_CONNECTION" != '' ]] && echo "%(!.%{$fg[red]%}.%{$fg[yellow]%})%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:" || echo '%m'
}

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats '<%b>'

# Set up the prompt (with git branch name)
setopt PROMPT_SUBST
PROMPT='$(ssh_info) %{${fg_bold[blue]}%}:: %{$reset_color%}%{${fg[green]}%}%3~ %{${fg[yellow]}%}${vcs_info_msg_0_}%{${fg_bold[$CARETCOLOR]}%}»%{${reset_color}%} '

export PATH=$PATH:$HOME/.config/composer/vendor/bin
export PATH="/usr/local/opt/qt/bin:$PATH:/Users/jan/Library/Python/3.7/bin"
export PATH="$PATH:/usr/local/opt/CMSIS_5"
export PATH="$PATH:/usr/local/opt/llvm/bin"
export ZEPHYR_TOOLCHAIN_VARIANT=gnuarmemb
export GNUARMEMB_TOOLCHAIN_PATH="/usr/local/Caskroom/gcc-arm-embedded/9-2020-q2-update/gcc-arm-none-eabi-9-2020-q2-update/"
