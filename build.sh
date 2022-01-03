#!/bin/sh
ASM=nasm

$ASM -f bin CGATST.ASM -o CGATST.COM
$ASM -f bin GDCTST.ASM -o GDCTST.COM
