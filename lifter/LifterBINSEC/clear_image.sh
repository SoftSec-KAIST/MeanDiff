#!/bin/bash

sudo docker rmi $(sudo docker images -qa -f "dangling=true")
