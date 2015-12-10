#!/usr/bin/env bash

set -e

if [ -z "$AWS_ACCESS_KEY_ID" ]; then
    echo "AWS_ACCESS_KEY_ID is not set"
    exit 1
fi

if [ -z "$AWS_SECRET_ACCESS_KEY" ]; then
    echo "AWS_SECRET_ACCESS_KEY is not set"
    exit 1
fi

docker-compose run wolf wolf-register -c '/cfg/config.yaml' -p '/cfg/plan.yaml'
docker-compose run wolf wolf-execute -c '/cfg/config.yaml' -p '/cfg/plan.yaml' -i '/cfg/execute.json'

docker-compose up
