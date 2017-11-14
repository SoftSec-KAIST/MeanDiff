#!/bin/bash

sudo docker run -v $(pwd):/src build_meandiff:latest /bin/bash -c 'cd /src && make'

# fix permissions
sudo chown -R $(whoami):users build
