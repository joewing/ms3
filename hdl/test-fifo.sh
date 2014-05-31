#!/bin/bash

set -e

ghdl -a fifo.vhdl fifotb.vhdl
ghdl -e fifotb
ghdl -r fifotb --ieee-asserts=disable --stop-time=5000ns --wave=dump.ghw

