#!/bin/bash

run_fourmolu_on_all_hs_files() {
  local otherargs=$1
  echo "Running fourmolu on all hs files with extra args: ${otherargs}"
  for i in $(find app-* bench src -name '*hs') Setup.hs
  do
    fourmolu --quiet --mode inplace $otherargs $i
  done
  echo
}

# fourmolu is not always idempotent and it has an option to detect when it isn't
#
# On one hand I want to know when it isn't, on the other hand I found that in
# such cases it does a better job on the second pass and that then it becomes
# idempotent.
#
# So we get this done in three steps:
#
# 1. warn about non-idempotence, if any, and format the rest

run_fourmolu_on_all_hs_files --check-idempotence

# 2. first-pass format files that give idempotence errors, if any

run_fourmolu_on_all_hs_files

# 3. second-pass format files that previously gave idempotence errors
#    and warn in case there still are idempotence errors 

run_fourmolu_on_all_hs_files --check-idempotence
