;----------------------------------------------------------------------------
; Functions for CGA emulator, 640x200 B/W mode, Simple Refresh
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; SetIRQ & ReloadIRQ
; IN:
;	- CS:CX: new IRQ address
;-------------------------------
OldIRQ:
	dw 0x0000,0x0000
SetIRQ:
	push	es
	xor		ax,ax				; Save the old IRQ vector
	mov		es,ax
	mov		bx,GDC_IRQ_ADDR
	es mov	ax,word [bx]
	mov		word [OldIRQ - PART_TSR],ax
	es mov	ax,word [bx+2]
	mov		word [OldIRQ - PART_TSR+2],ax
	cli
	
	es mov	word [bx],cx
	es mov	word [bx+2],cs
	
	sti
	pop		es
	ret
	
ReloadIRQ:
	push	es
	xor		ax,ax				; Reload the old IRQ vector
	mov		es,ax
	mov		bx,GDC_IRQ_ADDR
	cli
	mov		ax,word [OldIRQ - PART_TSR]
	es mov	word [bx],ax
	mov		ax,word [OldIRQ - PART_TSR+2]
	es mov	word [bx+2],ax
	sti
	pop		es
	ret
	
;----------------------------------------------------------------------------
; Setup screen for 640x200 black&white
;-------------------------------
InitCGA640x200x2:
	push	ax					; Save used registers
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es

	mov     ax,cs               ; Set DS point to TSR segment
	mov     ds,ax
	
	; Clear the framebuffer memory
	mov		ax,word [SETUP.DisplaySeg - PART_TSR]
	mov		es,ax
	xor		di,di
	mov		cx,0x2000
	xor		ax,ax
	cld
	rep stosw					; clear 16k memory
    
    ; Temporarily set up dummy IRQ handler
	mov		cx,(wait_irq_handler - PART_TSR)
	call	SetIRQ
	
	mov		si,(screen640x200 - PART_TSR)
	xor		cx,cx
	xor		dx,dx
.cont_setup:
	lodsb
	cmp		al,0xff
	jz		.wait_vrt
	cmp     al,0xfe
	jz      .wait_gdc_ready
	cmp		al,0
	jz		screen_ready
	mov		cl,al
	lodsb
	mov		dl,al
.rep_out:
	lodsb
	out		dx,al
	loop	.rep_out
	jmp		.cont_setup

.wait_gdc_ready:
    push    cx              ;use cx as a time-out loop counter
    in      al,0x56         ;first check if the FIFO is full
    test    al,2
    jz      .gnb2            ;jump if not
    mov     cx,0x8000        ;wait for FIFO not full or reasonable 
.gnb0:
    in      al,0x56         ;time, whichever happens first
    test    al,2            ;has a slot opened up yet?
    jz      .gnb2            ;jump if yes
    loop    .gnb0            ;if loop count exceeded, go on anyway
.gnb2:
    mov     al,0x0d          ;issue a screen-on command to GDC
    out     0x57,al
    in      al,0x56         ;did that last command fill it?
    test    al,2
    jz      .gnb4            ;jump if not
    mov     cx,0x8000
.gnb3:
    in      al,0x56         ;read status register
    test    al,2            ;test FIFO full bit
    jnz     .gnb4            ;jump if FIFO not full
    loop    .gnb3            ;loop until FIFO not full or give up
.gnb4:
    mov     ax,0x40d         ;issue another screen-on, 
    out     0x57,al         ;wait for FIFO empty
    mov     cx,0x8000
.gnb5:
    in      al,0x56         ;read the GDC status
    test    ah,al           ;FIFO empty bit set?
    jnz     .gnb6            ;jump if not.
    loop    .gnb5
.gnb6:
    pop     cx
    jmp     .cont_setup

;-------------------------------
; Wait for vertical retrace
; Used during screen setup
.wait_vrt:
    cli
	xor		ax,ax
	mov		byte [irq_handled - PART_TSR],al
    sti
	mov 	cx,8000				; time-out after 8000 cycles
.do_wait:
	test	byte [irq_handled - PART_TSR],0xff
	jz		.irq_handled
	jmp     .do_wait
.irq_handled:

	xor 	cx,cx
	jmp		.cont_setup

;-------------------------------
; Screen is ready, we can set up
; IRQ handler
screen_ready:
	call	ReloadIRQ			; Restore old IRQ handler
	mov		cx,(VRetrace640x480S - PART_TSR)
	call	SetIRQ
	
	; v-- This can be moved to the declarative part --v
	; Reset GDC Cursor Position to 0,0
	mov		al,GDC_CMD_CURS		; Reset cusrsor to x=0,y=0
	out		GDCP_FIFO_CMD,al
	xor		ax,ax
	out		GDCP_FIFO_PAR,al
	out		GDCP_FIFO_PAR,al
	
	; Setup GDC for plane 0 unfiltered writes
	mov		al,GDCIR_FBG		; Select Foreground/Background indirect register
	out		GDCP_IND_ADDR,al
	mov		al,0x0f				; Inverted foreground/background
	out		GDCP_IND_DATA,al
	
	mov		al,GDCIR_ALUPS		; Select ALU/PS register
	out		GDCP_IND_ADDR,al
	mov		al,0x0e				; Write to plane 0 only, replace mode
	out		GDCP_IND_DATA,al
	
	mov 	al,GDCIR_PRAM       ; Reset Write Buffer pointer
	out		GDCP_IND_ADDR,al
	out 	GDCP_IND_DATA,al
	; A-- This can be moved to the declarative part --A
	
	cs mov	word [REGSTORE.SS - PART_TSR],ss		; #18
	cs mov	word [REGSTORE.SP - PART_TSR],sp		; #18
	mov		sp,cs									; #2
	mov		ss,sp									; #2
	mov		sp,(IrqStack.Top - PART_TSR)+4*2		; #4
	
	mov		ax,200				; IRQ BX -> scan line count down counter
	push	ax
	mov		ax,0				; IRQ DI -> even lines pointer
	push	ax
	mov		ax,0x2000			; IRQ SI -> odd lines pointer
	push	ax
	mov		ax,word [(SETUP.DisplaySeg - PART_TSR)]
	push	ax

	cs mov	ss,word [REGSTORE.SS - PART_TSR]		; #18
	cs mov	sp,word [REGSTORE.SP - PART_TSR]		; #18
	
	mov		al,GDCIR_MODE
	out		GDCP_IND_ADDR,al
	mov		al, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_NOBLANK
	out		GDCP_IND_DATA,al
	
	pop		es
	pop		ds
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax					; Restore saved registers
	ret

;----------------------------------------------------------------------------
wait_irq_handler:
	cs mov	byte [irq_handled - PART_TSR],0xff
	iret
irq_handled:
	db 0

;----------------------------------------------------------------------------
; Setup screen for onboard text video out
;-------------------------------
InitChar80:
	push	ax					; Save used registers
	push	bx
	push	es
    
    ; TODO: this could occasionally not work: Need to sync up with the IRQ handler
    
	; Turning off vertical retrace IRQ, blanking graphics screen
	mov		al,GDCIR_MODE
	out		GDCP_IND_ADDR,al
	mov		al, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_BLANK
	out		GDCP_IND_DATA,al
	
	call 	ReloadIRQ
	
	mov		al,GDC_CMD_BLANK
	out		GDCP_FIFO_CMD,al
	mov		al,0x83
	out		0x0a,al
	
	pop		es
	pop		bx
	pop 	ax					; Restore saved registers
	ret

;----------------------------------------------------------------------------
; Vertical Retrace IRQ handler:
; Copy 3 scan lines at a time to GDC
;----------------------------------------------------------------------------
VRetrace640x480S:
    cli
	cs mov	word [REGSTORE.SS - PART_TSR],ss		; #18
	cs mov	word [REGSTORE.SP - PART_TSR],sp		; #18
	mov		sp,cs									; #2
	mov		ss,sp									; #2
	mov		sp,(IrqStack.Regs - PART_TSR)			; #4
	push	ax										; #15 * 6
	push	bx
	push	cx
	push	dx
	push 	si
	push	di
	push	ds										; #14 * 2
	push	es
	
	mov 	ax,sp									; #2
	mov		sp,(IrqStack.Top - PART_TSR)			; #4
	
	; Restore IRQ state
	pop		ds		; CGA frame buffer segment
	pop		si		; Odd scan line pointer
	pop		di		; Even scan line pointer
	pop		bx		; Line count down counter
	
	push	ax		; save register file SP			; #15
													; Total = #183
	; Register Backup-restore total = 327
	
	; Turn off DGC interrupts
	mov		al,GDCIR_MODE
	out		GDCP_IND_ADDR,al
	mov		al, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_NOBLANK
	out		GDCP_IND_DATA,al
	
	; Save registers
	;mov		word [REGSTORE.AX - PART_TSR],ax		; #18 * 8 cycles = 144
	;mov		word [REGSTORE.BX - PART_TSR],bx
	;mov		word [REGSTORE.CX - PART_TSR],cx
	;mov		word [REGSTORE.DX - PART_TSR],dx
	;mov		word [REGSTORE.SI - PART_TSR],si
	;mov		word [REGSTORE.DI - PART_TSR],di
	;mov		word [REGSTORE.DS - PART_TSR],ds
	;mov		word [REGSTORE.ES - PART_TSR],es		; restore: 17*8 = 136
	; Register Backup-restore total = 280
	
	; need to track cx,si,di -> 3*(18+17) = 105
	; push-pop: 3*(15+12) = 81
	
	; Initialize a few registers
	mov 	dx,GDCP_PRAM		; We will use it in the loop as out port
	
	; First copy even lines, from BA00:0000
	; All 8000 bytes are packed next to each other
	; starting from DS:SI
	
	mov		cx,2				; In 1 VRetrace we display 3 can lines
.nextLine:
	push	cx
	
	; We gained 50+ cycles by eliminating cursor positioning in GDC
	xchg	si,di				; # 4 | Continue with the odd/even other lines
	; each line is 80 (5x16) bytes long
	mov		cl,5
.nextBytesInLine:

	; Prepare the GDC to write from Write Buffer
	mov		ax,0x0200+GDC_CMD_FIGS	; # 4 |
	out		GDCP_FIFO_CMD,al    ; #14 |
	mov		al,ah				; # 2 | Direction 2, WDAT will come
	out		GDCP_FIFO_PAR,al    ; #14 |
	mov		ax,7				; # 4 | 8 words waiting in Write Buffer
	out		GDCP_FIFO_PAR,al    ; #14 |
	mov		al,ah               ; # 2 |
	out		GDCP_FIFO_PAR,al    ; #14 |
	                            ; =68 cycles
	
	lodsw                       ; Write 16 bytes to Write Buffer
	out		dx,al               ; #21 / byte = 336 Cycles
	mov		al,ah
	out		dx,al
	lodsw
	out		dx,al
	mov		al,ah
	out		dx,al
	lodsw
	out		dx,al
	mov		al,ah
	out		dx,al
	lodsw
	out		dx,al
	mov		al,ah
	out		dx,al
	lodsw
	out		dx,al
	mov		al,ah
	out		dx,al
	lodsw
	out		dx,al
	mov		al,ah
	out		dx,al
	lodsw
	out		dx,al
	mov		al,ah
	out		dx,al
	lodsw
	out		dx,al
	mov		al,ah
	out		dx,al
	
	mov		ax,GDC_CMD_WDAT | D_REPLACE | D_WORD +0xff00	; # 4 |
	out		GDCP_FIFO_CMD,al	; #14 | Start the write
	mov		al,ah				; # 2 | Set write mask, only LSB is used anyways...
	out		GDCP_FIFO_PAR,al    ; #14 |
	out		GDCP_FIFO_PAR,al    ; #14 |
	                            ; =48 cycles
	loop	.nextBytesInLine    ; #17 |
	                            ; =469 cycles / 16 bytes (overhead = 133) -> 2345 cycles / line
	pop		cx
	dec 	bx					; If all 200 scan lines finished,
	jz		.restart			; We need to restart
	loop	.nextLine
	
.irq_finished:
	; Turn on DGC interrupts again
	mov		al,GDCIR_MODE
	out		GDCP_IND_ADDR,al
	mov		al, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_NOBLANK
	out		GDCP_IND_DATA,al

	pop 	ax		; Stack pointer where the registers are		; #12
	
	; Save internal state
	push	bx		; Line count down counter	| #12 * 4
	push	di		; Even scan line pointer
	push	si		; Odd scan line pointer
	push	ds		; CGA framebuffer segment
	mov 	sp,ax	; Restore registers from different position ; #2
	
	pop		es									; #12 * 8
	pop		ds
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	cs mov	ss,word [REGSTORE.SS - PART_TSR]	; #17 | Original application stack segment & pointer
	cs mov	sp,word [REGSTORE.SP - PART_TSR]	; #17
												; Total = #144
	sti
	iret

.restart:
	mov		al,GDC_CMD_CURS		; Reset cusrsor to x=0,y=0
	out		GDCP_FIFO_CMD,al
	xor		ax,ax
	out		GDCP_FIFO_PAR,al
	out		GDCP_FIFO_PAR,al
	
	mov		si,0x2000			; Reset pointers + counter
	mov		di,0
	mov		bx,200
	
	jmp 	.irq_finished

screen640x200:
        db 1, GDCP_FIFO_CMD, GDC_CMD_RESET
        ; Mode, words/line-2, HSync+VSync, VSync+HFrontPorch, HBackPorch,
        ; VFrontPorch, Active lines, AltiveLines + VBackPorch
        ; 640x200
        db 8, GDCP_FIFO_PAR, 0x06, 0x26, 0x64, 0x1c, 0x08, 0x0d, 0xc8, 0x50
        ; The pitch (RAM line width) is 40 words = 640 pixels
        db 1, GDCP_FIFO_CMD, GDC_CMD_PITCH
        db 1, GDCP_FIFO_PAR, 0x28
        ; Set GDC mode
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_PLANE0 | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_BLANK
        ; Start display
        db 1, GDCP_FIFO_CMD, GDC_CMD_START
        ; Set zoom to none
        db 1, GDCP_FIFO_CMD, GDC_CMD_ZOOM
        db 1, GDCP_FIFO_PAR, 0
        ; Set write memory to reset to 0
        ; Reset to 1 and Reset to 0 doesn't require parameter
        db 1, GDCP_FIFO_CMD, GDC_CMD_WDAT | D_RESET0 | D_WORD
        ; Setup PRAM... for what???
        db 1, GDCP_FIFO_CMD, GDC_CMD_PRAM | 0
        db 4, GDCP_FIFO_PAR, 0x00, 0x00, 0xff, 0x0f
        db 1, GDCP_FIFO_CMD, GDC_CMD_PRAM | 8
        db 2, GDCP_FIFO_PAR, 0xff, 0xff
        ; Setup cursor (hide it)
        db 1, GDCP_FIFO_CMD, GDC_CMD_CCHAR
        db 3, GDCP_FIFO_PAR, 0x00, 0x00, 0x00
        ; Set vsync to generate (master mode)
        db 1, GDCP_FIFO_CMD, GDC_CMD_VSYNC | SYNC_MASTER
        ; Reset the video card
        db 1, GDCP_RESET, 0x00
        ; Enable IRQ
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_PLANE0 | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_BLANK
        db 0xff         ; Wait for vertical retrace IRQ
        ; Disable IRQ
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_PLANE0 | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_BLANK
        db 0xfe     ; Wait for GDC ready
        ; Load color map
        db 1, GDCP_IND_ADDR, GDCIR_COLOR
        db 32,GDCP_IND_DATA
        ; Color map (inverse, F=> no light .. 0=> full light)
        ; [red,green]
        db 0xff     ;black                                                                                                                                                                                           [75/988]
        db 0x00     ;white
        db 0xf0     ;cyan
        db 0x0f     ;magenta
        db 0x00     ;
        db 0x0f     ;red
        db 0xff     ;blue
        db 0xf0     ;green
        db 0xaa     ;dk grey
        db 0xf8     ;dk cyan
        db 0x8f     ;dk magenta
        db 0x88     ;
        db 0x8f     ;red
        db 0xff     ;blue
        db 0xf8     ;green
        db 0x77     ;dk grey
        ; [grey,blue]
        db 0xff     ;black,black
        db 0x00     ;white,white
        db 0xa0     ;lightgrey,cyan
        db 0x50     ;v      ,magenta
        db 0x1f     ;v
        db 0x2f     ;v      ,red
        db 0x50     ;v      ,blue
        db 0x4f     ;v      ,green
        db 0x6a     ;medgrey,dk grey
        db 0x78     ;v      ,dk cyan
        db 0x88     ;       ,dk magenta
        db 0x9f     ;v
        db 0xbf     ;       ,dk red
        db 0xc8     ;       ,dk blue
        db 0xdf     ;v      ,dk green
        db 0xe7     ;dkgrey ,grey       ; [red,green]
        db 0xfe
        ; Enable IRQ
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_BLANK
        db 0xff         ; Wait for vertical retrace IRQ
        ; Disable IRQ & enable Scroll writing
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_SCROLL | GDCM_NOINT | GDCM_BLANK
        ; Write scroll table
        db 1, GDCP_IND_ADDR, GDCIR_SCROLL
        db 128, GDCP_IND_DATA   ; Need to slice into 2, to make it 256
        db 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f
        db 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f
        db 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f
        db 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f
        db 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f
        db 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f
        db 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f
        db 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f
        db 128, GDCP_IND_DATA
        db 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f
        db 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f
        db 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf
        db 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf
        db 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf
        db 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf
        db 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef
        db 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
        ; Disable scroll write
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_NOBLANK
        db 0xff         ; Wait for vertical retrace IRQ
        ; Disable IRQ & enable Scroll writing
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_NOBLANK

        ; Initialize pattern multiplier to 1 (negated)
        db 1, GDCP_IND_ADDR, GDCIR_PTRN_MUL
        db 1, GDCP_IND_DATA, 0xff
        ; Initialize patter register, to output everything
        db 1, GDCP_IND_ADDR, GDCIR_PTRN
        db 1, GDCP_IND_DATA, 0xff

        ; Enable all foreground registers
        db 1, GDCP_IND_ADDR, GDCIR_FBG
        db 1, GDCP_IND_DATA, 0xf0

        ; Enable planes 0-3, REPLACE logic
        db 1, GDCP_IND_ADDR, GDCIR_ALUPS
        db 1, GDCP_IND_DATA, 0

        ; Set GDC load mask to FFFF
        db 1, GDCP_FIFO_CMD, GDC_CMD_MASK
        db 2, GDCP_FIFO_PAR, 0xff, 0xff
        ; Set WriteBuffer mask to 0000 (inverted)
        db 1, GDCP_WMASK, 0
        db 1, GDCP_WMASK_H, 0

        db 0xfe
        ; Enable IRQ
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_NOBLANK
        db 0xff         ; Wait for vertical retrace IRQ
        ; Disable IRQ & enable Scroll writing
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_NOBLANK

        ; Fill WriteBuffer with 0 inverted
        db 1, GDCP_IND_ADDR, GDCIR_PRAM
        db 1, GDCP_IND_DATA, 0
        db 16, GDCP_PRAM, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
        db 1, GDCP_IND_ADDR, GDCIR_PRAM
        db 1, GDCP_IND_DATA, 0

        db 0xfe         ; Wait for GDC to finish
        ; Clear screen
        db 1, GDCP_FIFO_CMD, GDC_CMD_CURS
        db 2, GDCP_FIFO_PAR, 0,0
        db 1, GDCP_FIFO_CMD, GDC_CMD_FIGS
        db 3, GDCP_FIFO_PAR, 2, 0xff, 0xff
        db 1, GDCP_FIFO_CMD, 0x22
        db 2, GDCP_FIFO_PAR, 0xff, 0xff

        db 0xfe         ; Wait for GDC to finish
        ; Enable IRQ
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_NOBLANK
        db 0xff         ; Wait for vertical retrace IRQ
        ; Disable IRQ & enable Scroll writing
        db 1, GDCP_IND_ADDR, GDCIR_MODE
        db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_NOBLANK
        ; Unblank monitor
        db 1, GDCP_FIFO_CMD, GDC_CMD_NOBLANK
        ; Switch to Graphics Option card
        db 1, 0x0a, 0x87
        db 0
