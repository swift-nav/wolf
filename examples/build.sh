#!/usr/bin/env bash

set -e

docker build -t swift-nav/wolf:v0.1 ..
docker build -t swift-nav/hello-world:v0.1 hello-world
