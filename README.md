# Install Emacs

## macOS
While macOS comes with emacs, it is not the most recent version.

### Uninstall old version of Emacs:
First, uninstall the version that comes with macOS (optional, may help with collisions)*:
```bash
# rm /usr/bin/emacs
# rm -rf /usr/share/emacs
```
*Due to a security feature on MacOS, users might not be able to rm /usr/bin/emacs even with sudo. Can also just add an alias to ~/.bash_profile 
```bash
alias emacs="/usr/local/bin/emacs -nw"
```
## Using brew:
```bash
brew update
brew install emacs --with-cocoa
brew linkapps emacs
```
## configure .emacs.d

git clone https://github.com/fedreg/emacs-config.git
(If you have an existing .emacs.d): mv ~/.emacs.d ~/.emacs.bak
cp -r emacs-config ~/.emacs.d
rm -rf emacs-config
