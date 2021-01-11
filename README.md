About as minimal an Emacs config as I can have and still be productive.

This one is geared toward Clojure which is my main language these days.

If you're not a fan of vim bindings, just remove all the evil packages (evil is the vim emulation mode for emacs)
and use a packages such as [general.el](https://github.com/noctuid/general.el) for your custom keybindings.

This config makes your Emacs open near instantly and looks the same in TTY or GUI modes

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
## OR, Downad directly from https://emacsforosx.com/

## configure .emacs.d

stick the `init.el` file from this repo in `~/.emacs.d`
(If you have an existing `.emacs.d/init.el`): `cp ~/.emacs.d/ ~/.emacs.bak`

