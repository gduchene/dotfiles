#!/usr/bin/env zsh

zmodload zsh/datetime
echo_log() { echo $(strftime %+) - $@ }

for env (DOMAIN HOST{,_SHORT} UNAME
         XDG_{{CACHE,CONFIG,DATA}_HOME,RUNTIME_DIR}); do
  echo_log Setting ${env} to “${(P)env}”.
  launchctl setenv ${env} ${(P)env}
done
