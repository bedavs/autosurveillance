#!/bin/bash

if [[ "$CI_PWD" != "" ]]; then
  echo "TESTING"
  export FOLDER_JUNIT=/home/raw996/jenkins/workspace/internal_surveillance/junit/
  export FOLDER_INFRASTRUCTURE=$CI_PWD/internal_surveillance/infrastructure/
  export FOLDER_DATA=/home/raw996/analyses/testing/internal_surveillance/data_raw/
  export FOLDER_RESULTS=/home/raw996/analyses/testing/internal_surveillance/results/

  mkdir -p $CI_PWD/junit
  chmod 777 $CI_PWD/junit

  mkdir -p $FOLDER_JUNIT/

  mkdir -p $FOLDER_DATA_RAW/
  mkdir -p $FOLDER_DATA_CLEAN/
  mkdir -p $FOLDER_RESULTS/
  mkdir -p $FOLDER_DATA_APP/

fi

