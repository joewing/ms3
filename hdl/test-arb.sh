#!/bin/bash

ghdl -a ram.vhdl arbiter.vhdl arbtb.vhdl
ghdl -e arbtb
ghdl -r arbtb --ieee-asserts=disable --stop-time=5ms --wave=dump.ghw

