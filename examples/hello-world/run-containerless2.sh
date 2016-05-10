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
(cd hello-world && stack exec wolf-act2 -- -c '../cfg/config.yaml' -q 'hello-queue' --command-line 'python hello.py') &
(cd hello-world && stack exec wolf-act2 -- -c '../cfg/config.yaml' -q 'bye-queue' --command-line 'python bye.py') &
(cd hello-world && stack exec wolf-act2 -- -c '../cfg/config.yaml' -q 'world-queue' --command-line 'python world.py') &
wait
