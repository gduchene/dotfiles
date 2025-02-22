export ENV_SH := $(realpath ../bin/env.sh)
AGENTS_DIR := ~/Library/LaunchAgents

all: $(foreach plist, $(wildcard *.json), $(AGENTS_DIR)/$(plist:json=plist))

$(AGENTS_DIR)/%.plist: %.json
	plutil -convert xml1 -o $@ - <<< $$(envsubst < $<)
	-launchctl unload $@ 2>/dev/null
	launchctl load $@
