#!/usr/bin/env zsh

for env in PATH UNAME XDG_{{CACHE,CONFIG,DATA}_HOME,RUNTIME_DIR}; do
  launchctl setenv ${env} ${(P)env}
done
