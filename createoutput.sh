#!/bin/bash

echo "Creating the Erlang representation of the CSP file..."
echo "[]." > output.txt
./parseCspForPl $1 output.pl
./translator
echo "Created."