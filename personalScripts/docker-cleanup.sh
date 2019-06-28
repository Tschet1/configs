#!/bin/bash
docker image prune -a -f --filter "until=240h"
docker container prune -f --filter "until=240h"

docker rmi $(docker images -q -f dangling=true) || /usr/bin/true
docker volume rm $(docker volume ls -qf dangling=true)
