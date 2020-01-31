#!/bin/bash

## This will build all MGSAT and derived Docker images
## The Dockerfile is a multi-stage file, and therefore
## will not work on RHEL 7 olden Docker. It needs
## the newer one from docker.com.
## In dire need, you can split the Dockerfile into
## several single-stage files and build separately.
## Set envar MGSAT_REGISTRY if you are going to push
## the resulting images to the registry.
## Set envar MGSAT_PUSH_REGISTRY to something if you
## want the resulting images to be immediately pushed into
## the registry. If registry requires logging in, you have to
## be already logged in before you run this script

this_dir=$(cd $(dirname $0); pwd)

MGSAT_TAG=2.8
push_reg=
if [ -n "$MGSAT_REGISTRY" ]; then
    registry_pref=${MGSAT_REGISTRY}/
    if [ -n "$MGSAT_PUSH_REGISTRY" ]; then
        push_reg=1
    fi
else
    registry_pref=""
fi
MGSAT_DOCKERFILE="${this_dir}/Dockerfile.mgsat_rocker_ver"

docker_build() {
    target=$1
    docker build -f "$MGSAT_DOCKERFILE" \
        --tag $registry_pref$target:latest \
        --tag $registry_pref$target:$MGSAT_TAG \
        --target $target .

    if [ -n "$push_reg" ]; then
        docker push $registry_pref$target:$MGSAT_TAG
        docker push $registry_pref$target:latest
    fi
}

mkdir -p mgsat_build_context

pushd mgsat_build_context

touch .dummy_sentinel.txt

docker_build mgsat-deps
docker_build mgsat
docker_build cgfease
docker_build multiomig

popd
