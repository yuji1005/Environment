#!/bin/sh
# refarence
# http://qiita.com/iri/items/2f9a39b9c47320f4c225
#
# Antigen を導入
#git clone https://github.com/zsh-users/antigen.git ~/projects/antigen
source ~/projects/antigen/antigen.zsh

# Antigen から Prezto を導入
antigen bundle sorin-ionescu/prezto

# Prezto から参照させるため、シンボリックリンクを設置
ln -s ~/.antigen/repos/.zprezto ~/.zprezto

# Prezto の Zsh 設定ファイルへのシンボリックリンクを設置
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done
echo -e "source ~/projects/antigen/antigen.zsh\\nantigen bundle sorin-ionescu/prezto" > ~/.zshrc
