#!/usr/bin/env zsh

zmodload zsh/datetime
echo_log() { echo $(strftime %+) - $@ }

for env (DOMAIN HOST SSH_AUTH_SOCK UNAME
         XDG_{{CACHE,CONFIG,DATA}_HOME,RUNTIME_DIR}); do
  echo_log Setting ${env} to “${(P)env}”.
  launchctl setenv ${env} ${(P)env}
done

# PATH is magic on macOS, and since it only matters for Emacs, let’s
# make it work in Emacs only. Yep, it’s pretty bad.
local plist=~/Applications/Emacs.app/Contents/Info.plist
if [[ -f ${plist} ]]; then
  local json_path='{"PATH": "'${PATH}'"}'
  echo_log Setting PATH in Emacs.app to “${json_path}”.
  plutil -replace LSEnvironment -json ${json_path} ${plist}
  /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f ${plist:h:h}
fi
