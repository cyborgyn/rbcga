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
	cmp		al,0
	jz		.screen_ready
	cmp		al,0xff
	jz		.wait_vrt
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
	mov		al,byte [irq_handled]
	cmp		al,0xff
	jz		.irq_handled
	loop	.do_wait
.irq_handled:
	pop		cx
	es mov	word [bx+2],cx
	pop 	cx
	es mov	word [bx],cx
	xor 	cx,cx
	jmp		.cont_setup

;-------------------------------
; Copy picture to framebuffer
;-------------------------------
.screen_ready:
    ; TODO: copy picture to framebuffer

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
    mov     ax,0x0003           ; CGA 80x25 color mode
    int     0x10
    mov     ah,0x02             ; Set cursor position
    mov     bh,0x00
    mov     dx,0x0000           ; row,col = 0
    int     0x10
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
	; 800x240
	db 8, GDCP_FIFO_PAR, 0x12, 0x30, 0x64, 0x08, 0x03, 0x03, 0xf0, 0x40
	; The pitch (RAM line width) is 64 words
	db 1, GDCP_FIFO_CMD, GDC_CMD_PITCH
	db 1, GDCP_FIFO_PAR, 0x40
	; Set GDC mode
	db 1, GDCP_IND_ADDR, GDCIR_MODE
	db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_PLANE0 | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_BLANK
	; Start display
	db 1, GDCP_FIFO_CMD, GDC_CMD_START
	; Set zoom to none
	db 1, GDCP_FIFO_CMD, GDC_CMD_ZOOM
	db 1, GDCP_FIFO_PAR, 0
	; Set write memory to reset to 0
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
	db 1, GDCP_RESET, 0x00
	; Enable IRQ
	db 1, GDCP_IND_ADDR, GDCIR_MODE
	db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_PLANE0 | GDCM_WRITE | GDCM_NOSCROLL | GDCM_INT | GDCM_BLANK
	db 0xff		; Wait for vertical retrace IRQ
	; Disable IRQ
	db 1, GDCP_IND_ADDR, GDCIR_MODE
	db 1, GDCP_IND_DATA, GDCM_RES_HI | GDCM_WORD | GDCM_PLANE0 | GDCM_WRITE | GDCM_NOSCROLL | GDCM_NOINT | GDCM_BLANK
	
	; TODO: Load color map
	; TODO: load scroll map
	; TODO: Initialize pattern & pattern multiply
	; TODO: set foreground & background colors
	; TODO: enable plane 0-3 replace logic
	db 1, GDCP_FIFO_CMD, GDC_CMD_NOBLANK
	db 0

filename:
    db "rainbow.raw",0
picture:
    ; times 16000 db?		; buffer
cgapic1:    equ (($ - $$ + 16000 + 0x100) >> 4) + 1
cgapic2:    equ cgapic1 + 0x200
    ; CGA format buffer
