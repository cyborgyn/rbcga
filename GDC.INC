;-----------------------------------------------------------
; NEC 7220 GDC defines
;-----------------------------------------------------------



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
