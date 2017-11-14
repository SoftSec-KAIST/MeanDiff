#!/bin/bash

sudo docker run -v $(pwd):/src lifter_bap:latest /bin/bash -c 'eval $(opam config env) && cd /src && make'
