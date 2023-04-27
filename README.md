dotfiles
========

This repo contains my dotfile configuration, allowing for a consistent computing experience across multiple machines.

I primarily use GNU/Linux via the [Arch Linux distribution](https://archlinux.org) for everyday use, and the [Debian distribution](https://debian.org) for server use.

I manage the various configuration files in this repo using [GNU Stow](https://www.gnu.org/software/stow/). This allows me to set up symlinks for all of my dotfiles using a single command:

~~~sh
stow -R --ignore='\.gitmodules' -t ~/ .
~~~
