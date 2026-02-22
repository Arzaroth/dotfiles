SHELL := /bin/sh
TARGET := $(HOME)
PACKAGES := zsh emacs tmux shell prompt

.PHONY: deploy clean restow list

deploy:
	stow -t $(TARGET) $(PACKAGES)

restow:
	stow -R -t $(TARGET) $(PACKAGES)

clean:
	stow -D -t $(TARGET) $(PACKAGES)

list:
	@echo $(PACKAGES)
