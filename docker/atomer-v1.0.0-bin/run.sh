#!/usr/bin/env bash

# Author: Dominik Harmim <iharmim@fit.vut.cz>

docker build -t infer-atomer-v1.0.0-bin .
docker run -it --rm infer-atomer-v1.0.0-bin /bin/bash -l
