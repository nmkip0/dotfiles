# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set the directory we want to store zinit and plugins
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download Zinit, if it's not there yet
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source/Load zinit
source "${ZINIT_HOME}/zinit.zsh"

# Add in Powerlevel10k
zinit ice depth=1; zinit light romkatv/powerlevel10k

# Add in zsh plugins
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab

# Add in snippets
zinit snippet OMZP::git
zinit snippet OMZP::sudo
zinit snippet OMZP::aws
zinit snippet OMZP::kubectl
zinit snippet OMZP::kubectx
zinit snippet OMZP::command-not-found

# Load completions
autoload -Uz compinit && compinit

zinit cdreplay -q

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Variables
export EDITOR=nvim
export JJ_CONFIG=$HOME/.config/jj/config.toml
export CLJ_CONFIG=$HOME/.config/clojure
export PATH=$PATH:$HOME/.local/bin

# vi mode
export KEYTIMEOUT=1

# Change highlight colors example

# typeset -A ZSH_HIGHLIGHT_STYLES
#
# ZSH_HIGHLIGHT_STYLES[suffix-alias]=fg=blue,underline
# ZSH_HIGHLIGHT_STYLES[precommand]=fg=blue,underline
# ZSH_HIGHLIGHT_STYLES[arg0]=fg=blue

# Keybindings
bindkey -v
bindkey "^H" backward-delete-char
bindkey "^?" backward-delete-char
bindkey '^l' autosuggest-accept
bindkey '^k' history-search-backward
bindkey '^j' history-search-forward

# Allow editing the current command in NeoVim when hitting <esc>C-v
autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd ^v edit-command-line

# History
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath'

ka () {
  kafkactl "${@}"
}

ls () {
  lsd "$@"
}

# Aliases
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias portal='bb ~/scripts/portal-server'
alias ka='kafkactl'
alias ls='lsd'
alias vim='nvim'
alias z='cd'
# Confirm before deleting
alias rm='rm -i'

# Conflicts with kl tool
unalias kl

# Start: Change cursor shape for different vi modes.
# "\e[2 q" = steady block
# "\e[1 q" = blinking block
#
# Set cursor style (DECSCUSR), VT520.
# 0  ⇒  blinking block.
# 1  ⇒  blinking block (default).
# 2  ⇒  steady block.
# 3  ⇒  blinking underline.
# 4  ⇒  steady underline.
# 5  ⇒  blinking bar, xterm.
# 6  ⇒  steady bar, xterm.

function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[2 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[6 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[6 q"
}
zle -N zle-line-init
echo -ne '\e[6 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[6 q' ;} # Use beam shape cursor for each new prompt.

# End: Change cursor shape for different vi modes.

# Shell integrations
eval "$(fzf --zsh)"
eval "$(zoxide init --cmd cd zsh)"
eval "$(mise activate zsh)"

source <(stern --completion=zsh)
source <(jj util completion zsh)

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/nmkip/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/home/nmkip/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/nmkip/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/nmkip/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

# Keep it at the end. It can break completions.
zinit light zsh-users/zsh-syntax-highlighting
