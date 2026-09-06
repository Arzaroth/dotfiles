# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `make skel` and `make skel-clean`, stowing into `/etc/skel` for new accounts;
  both refuse to run outside `/etc/skel/dotfiles`.

## [1.2.0] - 2026-09-07

### Added

- This changelog.
- GitHub Actions CI: ShellCheck, `zsh -n` syntax checks, `shfmt` formatting check and a simulated Stow deployment.
- `.editorconfig` and `.shellcheckrc` describing the shell dialect and indentation of each package.
- [worktrunk](https://github.com/max-sixty/worktrunk) shell integration, loaded when `wt` is on `PATH`.
- `reverse-menu-complete` bound to `Shift-Tab` in the completion menu.
- `Ctrl-Backspace` bound to `backward-kill-word`.
- [Ghostty](https://ghostty.org) shell integration, for working-directory inheritance in new windows.

### Changed

- Shell files reformatted with `shfmt`; no behavioural change.
- Ghostty shell integration is now skipped inside tmux and Zellij, which provide their own.
- `TERM` is forced back to `xterm-256color` inside tmux and Zellij instead of being exported unconditionally.
- Spacemacs submodule bumped.

### Fixed

- `[[ ]]` replaced with `[ ]` in `.profile`, which runs under `/bin/sh`.

## [1.1.0] - 2026-06-06

### Added

- [atuin](https://github.com/atuinsh/atuin) shell history, with the up-arrow binding left to Zsh.

### Changed

- Antidote plugin list reordered and trimmed; interactive niceties are deferred.
- Aliases are sourced last, so they win over plugin-provided ones.
- Oh My Zsh update prompt disabled.

### Removed

- Manual `compinit` call, which Antidote already performs.

### Fixed

- Shell hang caused by the Oh My Zsh `yarn` plugin resolving the global bin path.

## [1.0.0] - 2026-02-22

### Added

- Stow packages: `zsh`, `emacs`, `tmux`, `shell`, `prompt`, replacing the flat repository layout.
- `Makefile` with `deploy`, `restow`, `clean` and `list` targets.
- README documenting the structure and deployment.
- XDG-based Zsh configuration under `.config/zsh`, split into numbered fragments.
- `pyenv` initialisation in `.profile`.

### Changed

- Plugin manager switched from zinit to [antidote](https://github.com/mattmc3/antidote).
- Oh My Posh configuration updated to the v3 schema.

### Removed

- Doom Emacs configuration and submodule.
- Vendored `.emacs.default` package tree.
- Outdated Zsh completions shadowing the shipped ones (ohmyzsh/ohmyzsh#12576).

### Fixed

- `.profile` no longer sources `.bashrc` twice.
- `rsync` host completion.
- fzf key bindings and default options.

## [0.4.0] - 2024-06-27

### Added

- [Oh My Posh](https://ohmyposh.dev) prompt configuration.

### Removed

- Powerlevel10k configuration.

### Fixed

- zinit workaround for Oh My Zsh plugins fetched over svn.

## [0.3.0] - 2024-01-30

### Changed

- Plugin manager switched from z-shell/zi to [zdharma-continuum/zinit](https://github.com/zdharma-continuum/zinit).
- Oh My Zsh plugins and libraries picked individually over svn instead of cloning the whole distribution.

### Fixed

- Completion cache directory and completion colours.
- fzf completion script forced to load.

## [0.2.0] - 2023-07-26

### Added

- tmux configuration and [tpm](https://github.com/tmux-plugins/tpm) plugins.
- OSC 1337 support in Zsh.

### Changed

- Oh My Zsh replaced with the [z-shell/zi](https://github.com/z-shell/zi) plugin manager.

## [0.1.0] - 2023-04-27

### Added

- Initial dotfiles: Bash, Zsh with Oh My Zsh, Emacs with chemacs2, Doom Emacs and Spacemacs profiles, shared shell aliases.
- GNU Stow based deployment.

[Unreleased]: https://github.com/Arzaroth/dotfiles/compare/v1.2.0...HEAD
[1.2.0]: https://github.com/Arzaroth/dotfiles/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/Arzaroth/dotfiles/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/Arzaroth/dotfiles/compare/v0.4.0...v1.0.0
[0.4.0]: https://github.com/Arzaroth/dotfiles/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/Arzaroth/dotfiles/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/Arzaroth/dotfiles/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/Arzaroth/dotfiles/releases/tag/v0.1.0
