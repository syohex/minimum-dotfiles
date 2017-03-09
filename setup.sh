#!/bin/sh

set -e

install -d $HOME/.zsh
(cd $HOME/.zsh \
  && git clone https://github.com/zsh-users/zaw \
  && git clone https://github.com/zsh-users/zsh-completions)
