#!/bin/bash

sudo docker run -v $(pwd):/src lifter_binsec:latest /bin/bash -c 'eval $(opam config env) && cd /src && make'
