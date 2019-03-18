#!/bin/bash

if [[ "$CI_PWD" != "" ]]; then
  echo "TESTING"
  export FOLDER_JUNIT=/home/raw996/jenkins/workspace/internal_surveillance/junit/
  export FOLDER_INFRASTRUCTURE=$CI_PWD/internal_surveillance/infrastructure/
  export FOLDER_DATA_RAW=/home/raw996/analyses/testing/internal_surveillance/data_raw/
  export FOLDER_DATA_CLEAN=/home/raw996/analyses/testing/internal_surveillance/data_clean/
  export FOLDER_RESULTS=/home/raw996/analyses/testing/internal_surveillance/results/

  mkdir -p $CI_PWD/junit
  chmod 777 $CI_PWD/junit
elif [[ "$OSTYPE" == "darwin"* ]]; then
  export FOLDER_JUNIT=$HOME/Documents/analyses/junit/
  export FOLDER_INFRASTRUCTURE=$HOME/Documents/git/autosurveillance/infautosurveillance/
  export DOCKER_COMPOSE_EXE=/usr/local/bin/docker-compose
  export DOCKER_COMPOSE_FILE=$FOLDER_INFRASTRUCTURE/docker-compose-dev.yml
  export FOLDER_RSTUDIO=$HOME/Documents/analyses/autosurveillance/rstudio-$COMP_NAME/
else
  export FOLDER_JUNIT=/dev/null/
  export FOLDER_GIT=/home/raw996/git/
  export FOLDER_PACKAGES=/home/raw996/git/
  export FOLDER_DASHBOARDS=/home/raw996/git/dashboards/
  export FOLDER_INFRASTRUCTURE=/home/raw996/git/dashboards_control/infrastructure/
  export FOLDER_DATA_RAW=/home/raw996/data/data_raw/
  export FOLDER_DATA_CLEAN=/home/raw996/data/data_clean/
  export FOLDER_RESULTS=/home/raw996/data/results/
  export FOLDER_DATA_APP=/home/raw996/data/data_app/
  export DOCKER_COMPOSE_EXE=/usr/local/bin/docker-compose
  export DOCKER_COMPOSE_FILE=$FOLDER_INFRASTRUCTURE/docker-compose-dev.yml
fi

mkdir -p $FOLDER_JUNIT/

mkdir -p $FOLDER_DATA_RAW/
mkdir -p $FOLDER_DATA_CLEAN/
mkdir -p $FOLDER_RESULTS/
mkdir -p $FOLDER_DATA_APP/

