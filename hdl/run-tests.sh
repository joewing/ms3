#!/bin/bash

MAIN_MEM="(ram (word_size 4)(latency 10)(burst 0))"

set -e

function run()
{
    echo "(machine"                         >  input.model
    echo "  (addr_bits 32)"                 >> input.model
    echo "  (target fpga)"                  >> input.model
    echo "  (frequency 250000000)"          >> input.model
    echo "  (max_cost 64)"                  >> input.model
    echo ")"                                >> input.model
    echo "(memory"                          >> input.model
    echo "(main (memory $MAIN_MEM))"        >> input.model
    echo "(subsystem (id 0)(word_size 4)"   >> input.model
    echo "(depth 65536)"                    >> input.model
    echo "(memory"                          >> input.model
    cat input.mem                           >> input.model
    echo ")))"                              >> input.model
    echo "(benchmarks"                      >> input.model
    echo "(trace (id 0)(name test))"        >> input.model
    echo ")"                                >> input.model
    cat input.mem
    pypy ../memgen.py input.model > mem.vhdl 2> /dev/null
    ghdl -a mem.vhdl tb.vhdl
    ghdl -e tb
    set +e
    ghdl -r tb --ieee-asserts=disable --stop-time=5ms
    if [[ $? -ne 0 ]] ; then
        echo "Run failed; generating waveform"
        ghdl -r tb --ieee-asserts=disable --stop-time=5ms --wave=dump.ghw
        exit -1
    fi
    set -e
    rm input.mem input.model mem.vhdl
}

ghdl -a ram.vhdl arbiter.vhdl adapter.vhdl

echo "Testing prefetch..."
ghdl -a prefetch.vhdl
echo "(prefetch (stride 16) (memory (main)))" > input.mem
run

echo "Testing cache..."
ghdl -a cache.vhdl
for ((lsize=1; lsize<=32; lsize=lsize*2)) ; do
    for ((lcount=1; lcount<=16; lcount=lcount*2)) ; do
        for ((a=1; a<=8; a=a*2)) ; do
            for p in 'lru' 'mru' 'fifo' 'plru' ; do
                for wb in 'true' 'false' ; do
                    echo -n "(cache (line_size $lsize)" >  input.mem
                    echo -n "(line_count $lcount)"      >> input.mem
                    echo -n "(associativity $a)"        >> input.mem
                    echo -n "(policy $p)"               >> input.mem
                    echo -n "(write_back $wb)"          >> input.mem
                    echo "(memory (main)))"             >> input.mem
                    run
                done
            done
        done
    done
done

echo "Testing spm..."
ghdl -a spm.vhdl
for ((size=8; size<=256; size=size*2)) ; do
    for ((wsize=2; wsize<=8; wsize=wsize*2)) ; do
        echo "(spm (size $size)(word_size $wsize)(memory (main)))" > input.mem
        run
    done
done

echo "Testing split..."
ghdl -a split.vhdl
echo -n "(split (offset 24)"            >  input.mem
echo -n "(bank0 (join))(bank1 (join))"  >> input.mem
echo    "(memory (main)))"              >> input.mem
run

echo "Testing offset..."
ghdl -a offset.vhdl
echo "(offset (value 8)(bank (join))(memory (main)))" > input.mem
run
echo "(offset (value -16)(bank (join))(memory (main)))" > input.mem
run
echo "(offset (value 3)(bank (join))(memory (main)))" > input.mem
run

echo "Testing eor"
ghdl -a eor.vhdl
echo "(xor (value 2)(bank (join))(memory (main)))" > input.mem
run
echo "(xor (value 16)(bank (join))(memory (main)))" > input.mem
run
echo "(xor (value 257)(bank (join))(memory (main)))" > input.mem
run

echo "Success"
