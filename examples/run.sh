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

stack build wolf --copy-bins

wolf-register -c 'cfg/config.yaml' -p 'cfg/plan.yaml' || true
wolf-execute -c 'cfg/config.yaml' -p 'cfg/plan.yaml' -i 'execute.json'

docker-compose up
