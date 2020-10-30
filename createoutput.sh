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
    #docker run -t -v $PWD:/mnt ubuntu:20.04 /bin/bash -c "cd /mnt && ./createoutput.sh $1"
    docker run -t -v $PWD:/mnt debian:10-slim /bin/bash -c "cd /mnt && ./createoutput.sh $1"
}

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)  run_linux $1;;
    *)       run_docker $1;;
esac
