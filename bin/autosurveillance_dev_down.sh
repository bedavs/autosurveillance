#!/bin/bash

source $AUTOSURVEILLANCE_GIT/bin/internal_public.sh
export COMPUTER=$HOSTNAME

docker-compose -f $AUTOSURVEILLANCE_GIT/infautosurveillance/docker-compose.yml down

