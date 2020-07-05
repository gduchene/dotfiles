export ENV_SH := $(realpath ../bin/env.sh)

all: $(foreach plist, $(wildcard *.plist), ~/Library/LaunchAgents/${plist})

~/Library/LaunchAgents/%.plist: %.plist
	envsubst < $< >$@
	-launchctl unload $@ 2>/dev/null
	launchctl load $@
