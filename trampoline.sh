#!/usr/bin/env bash

if [[ ! -f "$HOME/fresh-cache" ]]; then

   stack --no-terminal test $ARGS  && \
   stack --no-terminal haddock $ARGS

else

#  exit 1 still stores cache,
# but we would like to notice the failure

  echo "Cache build - please restart"
  exit 1

fi
