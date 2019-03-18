#!/bin/bash

if [[ "$CI_PWD" != "" ]]; then
  source $CI_PWD/internal_surveillance/bin/internal_public.sh
  echo "$FOLDER_JUNIT IS MY JUNIT LOCATION"

  docker run --rm \
  -u="rstudio" \
  -v $FOLDER_JUNIT/:/junit/ \
  -v $FOLDER_DATA_RAW/:/analyses/data_raw/internal_surveillance/ \
  -v $FOLDER_RESULTS/:/dropbox/analyses/results_shared/internal_surveillance/ \
  -e COMPUTER=test \
  raw996/internal_surveillance:test Rscript /git/internal_surveillance/dofiles/test_1_ipd.R
else
  echo "HI"
  source $HOME/git/internal_surveillance/bin/internal_public.sh
  echo "$DOCKER_COMPOSE_FILE"
  echo "$FOLDER_JUNIT IS MY JUNIT LOCATION"

  $DOCKER_COMPOSE_EXE -f $DOCKER_COMPOSE_FILE \
  run --rm \
  -e COMPUTER=${COMPUTER:-noname} \
  internal_rstudio Rscript /git/internal_surveillance/dofiles/test_1_ipd.R

fi

