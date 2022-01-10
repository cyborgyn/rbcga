;-----------------------------------------------------------
; Test GDC 4 color 800x240 screen mode, by displaying bitmap
; copying first into CGA format, and transfering into GDC
;-----------------------------------------------------------
    use16
    cpu 8086
    org 0x100
%include "GDC.INC"
start:
;-------------------------------
; Load picture
;-------------------------------
    mov     ah,0x3d             ; DOS OpenFile function
    mov     al,0x20             ; Single read
    mov     dx,filename
    int     0x21

    mov     bx,ax               ; File handler (for read)

    mov     ah,0x3f             ; DOS FileRead function
    mov     cx,16000            ; Read 16000 bytes
    xor     al,al
    mov     dx,picture          ; DS:picture is the buffer
    int     0x21

    mov     ah,0x3e             ; DOS FileClose
    int     0x21

;-------------------------------
; Copy picture data into CGA format
;-------------------------------
    mov     ax,ds
    add     ax,cgapic1          ; CGA EVEN lines mem
    mov     es,ax
    mov     si,picture+80
    cld                         ; Direction flag=0, for movsb

.copy_plane:
    mov     cx,100              ; Copy 100 lines
    mov     di,7920             ; Target address last line
.copy_lines:
    push    cx
    mov     cx,80               ; Single line
    rep movsb                   ; Copy line
    pop     cx
    sub     di,160              ; Previous target line address
    add     si,80               ; Skip source line (interlace)
    loop    .copy_lines
    
    mov     ax,ds
    add     ax,cgapic2
    mov     bx,es
    cmp     ax,bx               ; Are we done?
    jz      .copy_ready
    mov     es,ax
    mov     si,picture          ; Odd lines source address
    jmp     .copy_plane
.copy_ready:

;-------------------------------
; Setup screen
;-------------------------------
	cld
	mov		si,screen
	xor		cx,cx
	xor		dx,dx
.cont_setup:
	lodsb
	cmp		al,0xff
	jz		.wait_vrt
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

;-------------------------------
; Wait for vertical retrace
; Used during screen setup
;-------------------------------
.wait_vrt:						; Wait a vertical retrace IRQ
	cli
	xor		ax,ax				; Save the old IRQ vector
	mov		es,ax
	mov		byte [irq_handled],al
	mov		bx,GDC_IRQ_ADDR
	es mov	cx,word [bx]
	push	cx
	es mov	cx,word [bx+2]
	push	cx
	
	mov		cx,cs				; Write our new, wait IRQ handler
	es mov	word [bx],cx
	mov		cx,wait_irq_handler
	es mov	word [bx+2],cx
	
	mov 	cx,8000				; time-out after 8000 cycles
	sti
.do_wait:
	test	byte [irq_handled],0xff
	jz		.irq_handled
	loop	.do_wait
.irq_handled:
	cli
	pop		cx
	es mov	word [bx+2],cx
	pop 	cx
	es mov	word [bx],cx
	sti
	xor 	cx,cx
	jmp		.cont_setup

	; One line == 80 bytes == 40 words
	; One line in GDC mem == 128 bytes == 64 words
	; Line feed == two GDC memory lines
screen_ready:
;-------------------------------
; Wait for key stroke
;-------------------------------
.waitkey:
    mov     ah,0x06             ; Console function
    mov     dl,0xff             ; read char
    int     0x21
    jz      .waitkey

;-------------------------------
; Copy picture to framebuffer
;-------------------------------
	; Setup GDC for plane 0 unfiltered writes
	; Reset GDC Cursor Position to 0,0
	mov		al,GDC_CMD_CURS
	out		GDCP_FIFO_CMD,al
	xor		ax,ax
	out		GDCP_FIFO_PAR,al
	out		GDCP_FIFO_PAR,al
	
	mov		al,GDCIR_FBG		; Select Foreground/Background indirect register
	out		GDCP_IND_ADDR,al
	mov		al,0x0f				; Inverted foreground & background
	out		GDCP_IND_DATA,al
	
	mov		al,GDCIR_ALUPS		; Select ALU/PS register
	out		GDCP_IND_ADDR,al
	mov		al,0x0e				; Write to plane 0 only, replace mode
	out		GDCP_IND_DATA,al
	
	mov 	al,GDCIR_PRAM       ; Reset Write Buffer pointer
	out		GDCP_IND_ADDR,al
	out 	GDCP_IND_DATA,al
	
	; Initialize a few registers
	xor		cx,cx				; We need CH=00
	mov 	dx,GDCP_PRAM		; We will use it in the loop as out port
	
	; First copy even lines, from BA00:0000
	; All 8000 bytes are packed next to each other
	; starting from DS:SI
	mov		ax,cs
	add		ax,cgapic1
	mov		ds,ax
	
	xor		ax,ax
	mov		di,ax
	mov 	si,0x2000			; odd lines
	; we have 200 lines, interlaced in SI,DI,SI,DI, etc...
	mov		cl,200
.nextLine:
	push	cx					; 
	
	test    cl,1
	jnz     .dontWait
.wait_gdc_ready:
	in      al,GDCP_STATUS		; read the GDC status
	test    al,0x20             ; In vertical sync?
	jz      .wait_gdc_ready    	; wait if not.
	test    al,0x04		        ; FIFO empty & Vertical sync bit set?
	jz      .wait_gdc_ready    	; wait if not.
.dontWait:
	
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
	loop	.nextLine
	
picture_ready:
	
;-------------------------------
; Wait for key stroke
;-------------------------------
.waitkey:
    mov     ah,0x06             ; Console function
    mov     dl,0xff             ; read char
    int     0x21
    jz      .waitkey

;-------------------------------
; Reset char screen, and exit
;-------------------------------
	mov		al,GDC_CMD_BLANK
	out		GDCP_FIFO_CMD,al
	mov		al,0x83
	out		0x0a,al
    mov     ax,0x4C00           ; Exit to DOS
    int     0x21


wait_irq_handler:
	cs mov	byte [irq_handled],0xff
	iret
irq_handled:
	db 0

screen:
	db 1, GDCP_FIFO_CMD, GDC_CMD_RESET
	; Mode, words/line-2, HSync+VSync, VSync+HFrontPorch, HBackPorch, 
	; VFrontPorch, Active lines, AltiveLines + VBackPorch
	; 640x200
	db 8, GDCP_FIFO_PAR, 0x02, 0x26, 0x64, 0x1c, 0x08, 0x0d, 0xc8, 0x50
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
	db 0xff		; Wait for vertical retrace IRQ
	; Disable IRQ
	db 1, GDCP_IND_ADDR, GDCIR_MODE
	db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_PLANE0 | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_BLANK
	; Load color map
	db 1, GDCP_IND_ADDR, GDCIR_COLOR
	db 32,GDCP_IND_DATA
	; Color map (inverse, F=> no light .. 0=> full light)
	; [red,green]
	db 0xff     ;black
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
	db 0xe7     ;dkgrey ,grey	; [red,green]
	
	; Enable IRQ
	db 1, GDCP_IND_ADDR, GDCIR_MODE
	db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_BLANK
	db 0xff		; Wait for vertical retrace IRQ
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
	
	; Fill WriteBuffer with 0 inverted
	db 1, GDCP_IND_ADDR, GDCIR_PRAM
	db 1, GDCP_IND_DATA, 0
	db 16, GDCP_PRAM, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff
	db 1, GDCP_IND_ADDR, GDCIR_PRAM
	db 1, GDCP_IND_DATA, 0
	
	db 1, GDCP_IND_ADDR, GDCIR_MODE
	db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_NOBLANK
	db 0xff		; Wait for vertical retrace IRQ
	; Disable IRQ & enable Scroll writing
	db 1, GDCP_IND_ADDR, GDCIR_MODE
	db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_NOBLANK
	; Clear screen
	db 1, GDCP_FIFO_CMD, GDC_CMD_CURS
	db 2, GDCP_FIFO_PAR, 0,0
	db 1, GDCP_FIFO_CMD, GDC_CMD_FIGS
	db 3, GDCP_FIFO_PAR, 2, 0xff, 0xff
	db 1, GDCP_FIFO_CMD, 0x22
	db 2, GDCP_FIFO_PAR, 0xff, 0xff
	
	; Unblank monitor
	db 1, GDCP_FIFO_CMD, GDC_CMD_NOBLANK
	; Switch to Graphics Option card
	db 1, 0x0a, 0x87
	db 0

filename:
    db "rainbow.raw",0
picture:
    ; times 16000 db?		; buffer
cgapic1:    equ (($ - $$ + 16000 + 0x100) >> 4) + 1
cgapic2:    equ cgapic1 + 0x200
    ; CGA format buffer
