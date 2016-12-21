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

function execute {
    local workflowId="$(uuidgen)"
    local input="$(<execute.json)"
    aws swf start-workflow-execution \
        --domain hello-world \
        --workflow-id "$workflowId" \
        --workflow-type name=HelloWorld,version=1.0 \
        --task-list name=hello-world-queue \
        --input "$input"
}

function execute-loop {
    execute
    sleep 30
    execute-loop
}

#stack exec wolf-decider -- --config config.yaml --plan plan.yaml &
#stack exec wolf-actor   -- --config config.yaml --queue hello-queue --command "python hello.py" &
#stack exec wolf-actor   -- --config config.yaml --queue world-queue --command "python world.py" &

execute-loop &

wait
