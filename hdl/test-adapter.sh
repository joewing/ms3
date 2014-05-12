#!/bin/bash

set -e

ghdl -a ram.vhdl adapter.vhdl adaptertb1.vhdl
ghdl -e adaptertb1
ghdl -r adaptertb1 --ieee-asserts=disable --stop-time=5000ns --wave=dump.ghw

