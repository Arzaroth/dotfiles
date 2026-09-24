dotfiles
========

This repo contains my dotfile configuration, allowing for a consistent computing experience across multiple machines.

I primarily use GNU/Linux via the [Arch Linux distribution](https://archlinux.org) for everyday use, and the [Debian distribution](https://debian.org) for server use.

## Structure

Dotfiles are organized into logical packages managed with [GNU Stow](https://www.gnu.org/software/stow/):

- `zsh/`      → Zsh configuration (including XDG-based config under `.config/zsh`)
- `emacs/`    → Emacs + Spacemacs (submodules included)
- `tmux/`     → Tmux configuration
- `shell/`    → Bash + shared shell files
- `prompt/`   → Oh My Posh configuration (XDG compliant)

Each directory is a Stow package.

## Deployment

Fetch the Emacs submodules:

```sh
make submodules
```

Deploy all packages:

```sh
make deploy
```

Deploy a single package:

```sh
stow -t "$HOME" zsh
```

Remove all symlinks:

```sh
make clean
```

Re-link (restow everything):
```sh
make restow
```

## New accounts

Clone the repository to `/etc/skel/dotfiles`, then stow into `/etc/skel` itself:

```sh
sudo make skel
```

`make skel` refuses to run from any other location: the symlinks it creates are
relative to the checkout (`.zshrc -> dotfiles/zsh/.zshrc`), so they only resolve
once `useradd` has copied both the links and `dotfiles/` into the new home.

`make skel-clean` removes them again.

## Plugin managers

[Antidote](https://github.com/mattmc3/antidote) (Zsh) and
[tpm](https://github.com/tmux-plugins/tpm) (tmux) are not part of the
checkout. The first interactive Zsh of each account clones them into
`~/.antidote` and `~/.tmux/plugins/tpm`; if the clone fails, the next login
tries again.

Updating is manual:

- `antidote update` updates the Zsh plugins and Antidote itself.
- `prefix + I` installs and `prefix + U` updates tmux plugins, from inside tmux.

Editing `.zsh_plugins.txt` regenerates the plugin bundle on the next shell.
