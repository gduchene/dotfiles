GNUPGHOME ?= ~/.gnupg

${GNUPGHOME}:
	mkdir -m 700 -p $@
	ln -s $(shell realpath --relative-to $@ gpg.conf) $@
