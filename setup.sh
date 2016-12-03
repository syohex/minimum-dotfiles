#!/bin/sh

set -e

mkdir -p $HOME/.zsh
(cd $HOME/.zsh \
  && git clone https://github.com/zsh-users/zaw \
  && git clone https://github.com/zsh-users/zsh-completions)
