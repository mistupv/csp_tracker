#!/bin/bash

run_linux() {
    echo "Creating the Erlang representation of the CSP file..."
    echo "[]." > output.txt
    ./parseCspForPl "$1" output.pl
    ./translator
    echo "Created."
}

run_docker() {
    echo "Running createoutput.sh through docker"
    if [ docker info > /dev/null 2>&1 ]; then
        echo "Error! Docker is not running or the current user has no permissions."
    else
        docker run -t -v $PWD:/mnt --rm debian:10-slim /bin/bash -c "cd /mnt && ./createoutput.sh $1"
    fi
}

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)  run_linux $1;;
    *)       run_docker $1;;
esac
