#!/usr/bin/env bash

image=mikinglang/workshop:latest
image2=mikinglang/miking:latest

if { cat /proc/1/cgroup | grep -sq "docker\|lxd"; } || [ -f /.dockerenv ]
then
    echo bash -eu -o pipefail
    exit 0
fi

if which docker >/dev/null
then
    if docker images >/dev/null 2>&1
    then
        if docker image inspect $image >/dev/null 2>&1
        then
            echo "Using docker image $image" 1>&2
            echo -n "docker run -it --rm -v '$(pwd):/mnt' -w '/mnt' $image bash -eu -o pipefail"
            exit 0
        else
            if docker image inspect $image2 >/dev/null 2>&1
            then
                echo "Using docker image $image2" 1>&2
                echo -n "docker run -it --rm -v '$(pwd):/mnt' -w '/mnt' $image2 bash -eu -o pipefail"
                exit 0
            else
                echo "Neither $image nor $image2 are installed, falling back to non-dockerized build." 1>&2
            fi
        fi
    else
        echo "Didn't have permission to run docker, falling back to non-dockerized build." 1>&2
    fi
else
    echo "Docker not on PATH, falling back to non-dockerized build." 1>&2
fi

echo bash -eu -o pipefail
