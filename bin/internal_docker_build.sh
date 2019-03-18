#!/bin/bash

if [[ "$CI_PWD" != "" ]]; then
  source $CI_PWD/internal_surveillance/bin/internal_public.sh
  docker build --pull --no-cache --tag=raw996/internal_surveillance:test $FOLDER_INFRASTRUCTURE/internal_surveillance
else
  source $HOME/Documents/git/autosurveillance/bin/internal_public.sh
  docker build --pull --no-cache --tag=raw996/autosurveillance $FOLDER_INFRASTRUCTURE/autosurveillance
fi

