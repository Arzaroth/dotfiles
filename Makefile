SHELL := /bin/sh
TARGET := $(HOME)
SKEL := /etc/skel
PACKAGES := zsh emacs tmux shell prompt

.PHONY: deploy restow clean list skel skel-clean check-skel

deploy:
	stow -t $(TARGET) $(PACKAGES)

restow:
	stow -R -t $(TARGET) $(PACKAGES)

clean:
	stow -D -t $(TARGET) $(PACKAGES)

list:
	@echo $(PACKAGES)

check-skel:
	@test "$(CURDIR)" = "$(SKEL)/dotfiles" || { \
		echo "make: this checkout must be $(SKEL)/dotfiles, not $(CURDIR)" >&2; \
		exit 1; \
	}

skel: check-skel
	$(MAKE) restow TARGET=$(SKEL)

skel-clean: check-skel
	$(MAKE) clean TARGET=$(SKEL)
