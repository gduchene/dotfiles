#!/usr/bin/env zsh

zmodload zsh/datetime
echo_log() { echo $(strftime %+) - $@ }

for env in DOMAIN HOST UNAME XDG_{{CACHE,CONFIG,DATA}_HOME,RUNTIME_DIR}; do
  echo_log Setting ${env} to “${(P)env}”.
  launchctl setenv ${env} ${(P)env}
done

# Some environment variables are magic: setting them with launchctl does
# not seem to work with Emacs. I’m not sure why, but adding those to
# Info.plist does work.
local plist=~/Applications/Emacs.app/Contents/Info.plist

if [[ ! -f ${plist} ]]; then
  echo_log No Info.plist file found at “${plist}”.
  return
fi

plutil -insert LSEnvironment ${plist} 2>/dev/null >&2 || true
for env in PATH SSH_AUTH_SOCK; do
  echo_log Setting LSEnvironment.${env} in Emacs.app to “${(P)env}”.
  plutil -replace LSEnvironment.${env} -string ${(P)env} ${plist}
done
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f ${plist:h:h}
