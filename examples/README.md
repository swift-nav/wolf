# Wolf Examples

This is a hello world example using wolf, a Simple Workflow
library. To build this example:

    $ ./build.sh

To run this example, export your AWS credentials and run:

    $ export AWS_ACCESS_KEY_ID=***
    $ export AWS_SECRET_ACCESS_KEY=***
    $ ./run.sh

## Background

Wolf provides commands and configuration to run containerized tasks
with Simple Workflow. Using a few conventions around input, output,
and storage, wolf composes tasks into workflows that can be run in
containers in any language environment.

### Containers

Tasks are run from containers. This example defines a single
container, [./hello-world][4], that contains two python scripts,
[./hello-world/hello.py][5] and [./hello-world/world.py][6].

Tasks' JSON input is accessible in the contents of the file at
`/app/data/input.json` - this is workflow specific input, where these
is an inital input that is subsequently transformed by each task.

JSON metadata about the workflow is accessible in the contents of the
file at `/app/data/control.json` - useful identifiers about the
workflow can be found there.

Tasks' JSON output should be written to the file at
`/app/data/output.json` - this output will serve as input into the
next task in the workflow.

Any artifacts needing to be stored besides simple outputs can be
written to the directory `/app/store`, where all files will be
uploaded to S3.

The initial input for the this example is found in
[./execute.json][7].

### Configuration

The general configuration for wolf is found in
[./cfg/config.yaml][8] - it contains information around AWS region,
timeouts, credential environment variable names, SWF domains, and S3
prefix and bucket information. NOTE: change the S3 bucket to somewhere
you have permissions to write to.

Tasks are composed together in the workflow's plan configuration found
in [./cfg/plan.yaml][9], specifying that we want to run the Hello
task, followed by 15 seconds of Sleep, followed by the World
task. This workflow loops on itself and will continuously keep
running - after World ends, Hello will start.

The configuration for each task is found in a specific yaml file
containing the task's container parameters: which image to use, which
command to run, etc. The Hello's task configuration is found in
[./cfg/hello.yaml][10] and the World's task configuration is found in
[./cfg/world.yaml][11].

Finally, the specification of containers to run is found in
[./docker-compose.yml][12] - Hello and World containers will be
started up along with a Decide container to drive the workflow.

### Sample Output

    hello_1  |
    hello_1  | {u'run_uid': u'e9e59f03-556f-4d2a-92cb-b6ba0f451263'}
    hello_1  |
    hello_1  |
    hello_1  | {u'step-0': u'execute'}
    hello_1  |
    hello_1  |
    hello_1  | hello worker
    hello_1  |
    hello_1  |
    hello_1  | {'step-1': 'hello', u'step-0': u'execute'}
    hello_1  |
    hello_1  |
    hello_1  | {'step-1': 'hello', u'step-0': u'execute', 'storage': 'true'}
    hello_1  |
    world_1  |
    world_1  | {u'run_uid': u'e9e59f03-556f-4d2a-92cb-b6ba0f451263'}
    world_1  |
    world_1  |
    world_1  | {u'step-1': u'hello', u'step-0': u'execute'}
    world_1  |
    world_1  |
    world_1  | world worker
    world_1  |
    world_1  |
    world_1  | {'step-2': 'world', u'step-1': u'hello', u'step-0': u'execute'}
    world_1  |
    world_1  |
    world_1  | {'step-2': 'world', u'step-1': u'hello', u'step-0': u'execute', 'storage': 'true'}
    world_1  |

### Dependencies

To build and run this example, the following dependencies are required:

+ [stack][1] is a tool for building Haskell projects. See the linked downloads page.
+ [docker][2] is container tooling.
+ [docker-compose][3] is container tooling.

[1]: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md
[2]: https://www.docker.com/
[3]: https://docs.docker.com/compose/install/
[4]: https://github.com/swift-nav/wolf/blob/master/examples/hello-world/Dockerfile
[5]: https://github.com/swift-nav/wolf/blob/master/examples/hello-world/hello.py
[6]: https://github.com/swift-nav/wolf/blob/master/examples/hello-world/world.py
[7]: https://github.com/swift-nav/wolf/blob/master/examples/execute.json
[8]: https://github.com/swift-nav/wolf/blob/master/examples/cfg/config.yaml
[9]: https://github.com/swift-nav/wolf/blob/master/examples/cfg/plan.yaml
[10]: https://github.com/swift-nav/wolf/blob/master/examples/cfg/hello.yaml
[11]: https://github.com/swift-nav/wolf/blob/master/examples/cfg/world.yaml
[12]: https://github.com/swift-nav/wolf/blob/master/examples/docker-compose.yml
