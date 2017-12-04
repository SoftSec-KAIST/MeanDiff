#!/bin/bash

sudo docker run -v $(pwd):/src lifter_pyvex:latest /bin/bash -c 'cd /src && make'
