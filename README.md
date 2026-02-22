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
