SHELL := /bin/sh
TARGET := $(HOME)
SKEL := /etc/skel
PACKAGES := zsh emacs tmux shell prompt

ZDOTDIR ?= $(HOME)
ANTIDOTE := $(ZDOTDIR)/.antidote
TPM := $(HOME)/.tmux/plugins/tpm

.PHONY: deploy restow clean list skel skel-clean check-skel bootstrap submodules

# Fetch everything the shells expect to already be on disk. Kept out of the
# interactive rc files so that a failed clone cannot leave a shell in a broken
# state, and so a login shell never blocks on the network.
bootstrap: submodules $(ANTIDOTE) $(TPM)

submodules:
	git submodule update --init

$(ANTIDOTE):
	git clone --depth=1 https://github.com/mattmc3/antidote.git $@

$(TPM):
	mkdir -p $(dir $@)
	chmod g-rwX $(dir $@)
	git clone https://github.com/tmux-plugins/tpm $@

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
