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

stack exec wolf-register -- -c 'cfg/config.yaml' -p 'cfg/plan.yaml'
stack exec wolf-execute -- -c 'cfg/config.yaml' -p 'cfg/plan.yaml' -i 'cfg/execute.json'

stack exec wolf-decide -- -c 'cfg/config.yaml' -p 'cfg/plan.yaml' &
stack exec wolf-act -- -c 'cfg/config.yaml' -q 'hello-queue' -x 'cfg/hello.yaml' --containerless hello-world &
stack exec wolf-act -- -c 'cfg/config.yaml' -q 'world-queue' -x 'cfg/world.yaml' --containerless hello-world &
wait
