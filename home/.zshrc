
# Path to oh-my-zsh installation
fpath=( "$HOME/.zfunctions" $fpath )
ZSH_CUSTOM=$HOME/.oh-my-zsh-custom
PATH=$PATH:~/.local/bin
eval "$(zoxide init zsh)"

# Load antigen. To find the place where it's installed(yay/pacman -Ql antigen)
source /usr/share/zsh/share/antigen.zsh

# Load antigen.
if [[ `uname` == "Darwin" ]]; 
then
#  MACOS
#  Antigen info macos: brew info antigen
    FZF_BASE="/opt/homebrew/opt/fzf"
   # PATH="/usr/local/homebrew/bin:$PATH"
    ZSH="$HOME/.oh-my-zsh"
    source /opt/homebrew/share/antigen/antigen.zsh
    alias brew-intel="arch -x86_64 /usr/local/homebrew/bin/brew"
    export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
    export TERM=wezterm
else
#  To find the place where it's installed(yay/pacman -Ql antigen)
    ZSH="/usr/share/oh-my-zsh"
   source /usr/share/zsh/share/antigen.zsh
fi


# Needed for zsh autosuggestions
antigen bundle zsh-users/zsh-autosuggestions
antigen apply

# ZSH config

# Oh my zsh config
export EDITOR=nvim
export JJ_CONFIG=$HOME/.config/jj/config.toml
export CLJ_CONFIG=$HOME/.config/clojure
# Set name of the theme to load. If set to "random" it will load a random theme
# each time that oh-my-zsh is loaded.
ZSH_THEME="lambda"

# Enable ZSH plugins. Plugins can be found in "$ZSH/plugins/"
plugins=(vi-mode git z fzf)

# Vi mode config
VI_MODE_SET_CURSOR=true
VI_MODE_RESET_PROMPT_ON_MODE_CHANGE=true
VI_MODE_INDICATOR="%F{yellow}+%f"

# ----- AUTO-COMPLETION ----- #

# initialise completions with ZSH's compinit
autoload -U compinit
compinit
source <(jj util completion --zsh)

source <(kubectl completion zsh)
source <(stern --completion=zsh)

k() {
  kubectl "${@}"
}

ka () {
  kafkactl "${@}"
}

ls () {
  lsd "$@"
}

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias portal='bb ~/scripts/portal-server'

alias k="kubectl"
alias ka="kafkactl"
alias ls="lsd"

# Confirm before overwritting something
alias rm="rm -i"

source $ZSH/oh-my-zsh.sh

# Key Bindings
bindkey -v
zstyle ':completion:*' menu select 

bindkey -s ^f "t\n"
bindkey '^L' autosuggest-accept

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/nmkip/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/home/nmkip/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/nmkip/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/nmkip/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(mise activate zsh)"
