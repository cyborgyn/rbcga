;-----------------------------------------------------------
; NEC 7220 GDC defines
;-----------------------------------------------------------
GDC_CMD_RESET:	equ 0x00		; Reset command, 8 params
GDC_CMD_SYNC:	equ 0xfe		; Sync setup command, 8 params
; 0.bit: DE??

GDC_CMD_VSYNC:	equ 0x6e		; VSync setup command
; 0. bit: 1 if master mode
SYNC_MASTER:	equ 0x01
SYNC_SLAVE:		equ 0x00

GDC_CMD_CCHAR:	equ 0x48		; Cursor & Character characteristics, 3 params
; p1 = 	7b	: display cursor if 1
;		6-5b= 0
;		4-0b: lines/char row - 1
; p2 = 	7-6b: blinking rate lower 2 bits
;		5b	: blinking cursor if 1
;		4-0b: cursor top line number in the row
; p3 = 	7-3b: cursor bottom line number in the row
;		2-0b: blinking rate upper 3 bits

GDC_CMD_BLANK:	equ 0x0c		; Blank display
GDC_CMD_NOBLANK:equ 0x0d		; Show display

GDC_CMD_CURS:	equ 0x49		; Cursor setup, ? params
GDC_CMD_MASK:	equ 0x4a		; Load Mask register, 2 params
; p1 = low byte
; p2 = hi byte

GDC_CMD_FIGS:	equ 0x4c		; Figure Setup, 1-11 params
; TODO: p1, p2-p11

GDC_CMD_FIGD:	equ 0x6c		; Figure Draw (start set up figure drawing)
GDC_CMD_GCHRD:	equ 0x68		; Graphics char draw / area filling start
GDC_CMD_CURD:	equ 0xe0		; Cursor address read
GDC_CMD_LPRD:	equ 0xc0		; Light Pen address read

GDC_CMD_PITCH:	equ 0x47		; Pitch command, 1 param
; p1 = number of word in RAM for a single line

GDC_CMD_START:	equ 0x6b		; End idle mode, start display

GDC_CMD_ZOOM:	equ 0x00		; Zoom command, 1 param
; p1 =  7-4b: display zoom factor
;		3-0b: graphics & char writing zoom factor

GDC_CMD_RDAT:	equ 0xa0		; Read data from display memory
; The same type and mod subfields as WDAT
GDC_CMD_WDAT:	equ 0x20		; Write data into display memory
; command flags (can be ORed with command):
D_REPLACE:		equ 0x00
D_COMPLEMENT:	equ 0x01
D_RESET0:		equ 0x10
D_RESET1:		equ 0x11
D_WORD:			equ 0x00 << 3
D_LOWBYTE:		equ 0x10 << 3
D_HIBYTE:		equ 0x11 << 3

GDC_CMD_PRAM:	equ 0x70		; PRAM load, 1-16 params
; lower nible is the PRAM start address (0-16)


; - PRAM:
; The output of the Write Buffer is the inverse of its input. If a word is written into the buffer as
; FFB6h, it will be read out of the buffer as 0049h. To have the same data written out to the bitmap
; as was received from the CPU requires an added inversion step. You can exclusive or (XOR) the
; CPU data with FFh to pre-invert the data before going through the Write Buffer. Alternately, you
; can write zeros into the Foreground Register and ones into the Background Register to re-invert the
; data after it leaves the Write Buffer and before it is written to the bitmap. Use one method or the
; other, not both.

;-----------------------------------------------------------
; Rainbow 100 Graphics Option specific defines
;-----------------------------------------------------------
GDCP_RESET:     equ 0x50
GDCP_IND_DATA:  equ 0x51
GDCP_IND_ADDR:  equ 0x53        ; Accepts GDCIR_*
GDCP_PRAM:      equ 0x52        ; Also mentioned as Write Buffer

GDCP_WMASK:     equ 0x54        ; Can outw to this
GDCP_WMASK_H:   equ 0x55

GDCP_FIFO_CMD:  equ 0x57
GDCP_FIFO_PAR:  equ 0x56        ; Both these ports write to FIFO

; Flags for GDCP_IND_ADDR
; After selecting indirect address, one can write to
; GDCP_IND_DATA. Even for PRAM a dummy write is needed.
GDCIR_PRAM:     equ 0x01^0xff   ; Write Buffer, also clears
GDCIR_PTRN_MUL: equ 0x02^0xff   ; Pattern Multiplier
GDCIR_PTRN:     equ 0x04^0xff   ; Pattern Register
GDCIR_FBG:      equ 0x08^0xff   ; Foreground/background register
GDCIR_ALUPS:    equ 0x10^0xff   ; ALU/PS register
GDCIR_COLOR:    equ 0x20^0xff   ; Color map
GDCIR_MODE:     equ 0x40^0xff   ; Mode register
GDCIR_SCROLL:   equ 0x80^0xff   ; Scroll register

; Mode register flags
GDCM_RES_LOW:   equ 0x00 << 0   ; 384 pixel wide
GDCM_RES_HI:    equ 0x01 << 0   ; 800 pixel wide
GDCM_WORD:      equ 0x00 << 1   ; 16 bits/RMW from PRAM (Write Buffer)
GDCM_VECT:      equ 0x01 << 1   ; 1 bit/RMW from pattern generator
GDCM_PLANE0:    equ 0x00 << 2   ; Plane 0
GDCM_PLANE1:    equ 0x01 << 2   ; Plane 1
GDCM_PLANE2:    equ 0x02 << 2   ; Plane 2
GDCM_PLANE3:    equ 0x03 << 2   ; Plane 3
GDCM_READ:      equ 0x00 << 4   ; Read mode
GDCM_WRITE:     equ 0x01 << 4   ; Write mode
GDCM_SCROLL:    equ 0x00 << 5   ; Write to scroll enabled
GDCM_NOSCROLL:  equ 0x01 << 5   ; Write to scroll disabled
GDCM_NOINT:     equ 0x00 << 6   ; Interrupts disabled
GDCM_INT:       equ 0x01 << 6   ; Interrupts endabled
GDCM_BLANK:     equ 0x00 << 7   ; Video out is blanked
GDCM_NOBLANK:   equ 0x01 << 7   ; Video out is enabled
