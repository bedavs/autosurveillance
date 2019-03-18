#!/bin/bash

source $AUTOSURVEILLANCE_FOLDER/bin/internal_public.sh

docker-compose -f $FOLDER_INFRASTRUCTURE/docker-compose-dev.yml down
