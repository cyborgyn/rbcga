#!/bin/sh
ASM=nasm

if [ "$1" == "dosbox" ]; then
    echo Building for dosbox...
    $ASM -f bin CGA.ASM -o CGA.COM -dNASM=true -dDOSBOX=true
else
    echo Building all...
    $ASM -f bin CGATST.ASM -o CGATST.COM
    $ASM -f bin GDCTST.ASM -o GDCTST.COM
    $ASM -f bin CGA.ASM -o CGA.COM -dNASM=true
fi
