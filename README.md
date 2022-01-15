# CGA Emulator for DEC Rainbow 100B

It's in Proof-of-concept state. Right now, it can emulate on real hardware CGA hi-res mode, by copying/refreshing the usual RAM Framebuffer to the Graphics Option's RAM to display. It steals 10-12% CPU time for that with the help of GDC's Verticla Retrace IRQ, and finishes in 50 retraces with a full screen. Since the Vertical freq is around 55Hz-60Hz, it's a around 1 frame/s (a tiny little bit more).

## Features
- Partial INT 10h handling
- CGA 640x200 black & white screen switching (mode = 6)
- Color/mono 80x25 char switching back (mode = 2 or 3)
- Contignous screen full refresh in 640x200 mode

## Requirements to use
- DEC Rainbow 100B (B is needed for redirected IRQ numbers)
- 896 kByte RAM, with ram expansion (need to have a shadow RAM buffer location at B800:0000 + 16k)
- Graphics Option
- Black & White monitor VR201
- MS DOS 2.0+

## What will not work
- CGA IO ports can not be amulated (no chance to do that)
- CGA 320x200 4 color modes
- BIOS character bitmaps not found in Rainbow @ FFA6E-FFE6E (it's a ROM area)

## How to build it under DOS
Tools needed:
- TinyAsm in your PATH: https://github.com/nanochess/tinyasm

Under DOS prompt, jut run
```
C:\RBCGA>BUILD.BAT
```
This also works in MSDOS under Rainbow itself.

## How to build it under LINUX
Tools needed:
- Netwide Assembler in you path: https://www.nasm.us/

In the proper directory, just run
```
# build.sh
```

# How to use it
You will most probably also need to run CodeBlue PC emulator first:
```
> cb.exe /af c:\command.com
```
Then start the cga emulator:
```
> CGA.COM
```

If you are finished running your CGA application, just uninstall CGA.COM:
```
> CGA.COM OFF
```
That's all for now.
