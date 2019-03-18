#!/bin/bash

source $AUTOSURVEILLANCE_FOLDER/bin/internal_public.sh
export COMPUTER=$HOSTNAME

echo $FOLDER_INFRASTRUCTURE

docker-compose -f $FOLDER_INFRASTRUCTURE/docker-compose-dev.yml up

