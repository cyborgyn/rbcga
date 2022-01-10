#!/bin/bash
./build.sh
kermit -s GDCTST.COM
kermit -C "remote host gdctst.com, exit"