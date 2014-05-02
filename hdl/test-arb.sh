#!/bin/bash

set -e

ghdl -a ram.vhdl arbiter.vhdl arbtb.vhdl
ghdl -e arbtb
ghdl -r arbtb --ieee-asserts=disable --stop-time=5000ns --wave=dump.ghw

