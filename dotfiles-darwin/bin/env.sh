#!/usr/bin/env zsh

for env (DOMAIN HOST SSH_AUTH_SOCK UNAME
         XDG_{{CACHE,CONFIG,DATA}_HOME,RUNTIME_DIR}); do
  launchctl setenv ${env} ${(P)env}
done

# PATH is magic on macOS, and since it only matters for Emacs, let’s
# make it work in Emacs only. Yep, it’s pretty bad.
local plist=~/Applications/Emacs.app/Contents/Info.plist
if [[ -f ${plist} ]]; then
  plutil -replace LSEnvironment -json '{"PATH": "'${PATH}'"}' ${plist}
  /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f ${plist:h:h}
fi
