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

stack exec wolf-execute -- -c config.yaml -p plan.yaml -i execute.json

stack exec wolf-decide -- -c config.yaml -p plan.yaml &

stack exec wolf-actor -- --config config.yaml --queue hello-queue --command "python hello.py" &
stack exec wolf-actor -- --config config.yaml --queue world-queue --command "python world.py" &
wait
