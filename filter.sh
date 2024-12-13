#!/usr/bin/env sh

args=("$@")

rg_cmd="fd .*spec"

# Replace each string in the command with the corresponding argument piped through xargs
for i in "${!args[@]}"; do
  rg_cmd+=" | xargs rg -e '${args[i]}' -l"
done

eval $rg_cmd
