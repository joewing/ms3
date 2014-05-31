#!/bin/bash

set -e

ghdl -a fifo.vhdl rfifotb.vhdl
ghdl -e rfifotb
ghdl -r rfifotb --ieee-asserts=disable --stop-time=5000ns --wave=dump.ghw

