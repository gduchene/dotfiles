GNUPGHOME ?= ~/.gnupg

all: ${GNUPGHOME}

${GNUPGHOME}:
	mkdir -m 700 -p $@
	ln -s $(shell realpath --relative-to $@ gpg.conf) $@/gpg.conf
