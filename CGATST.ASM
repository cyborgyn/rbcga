use16
cpu 8086
org 0x100
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
; Setup screen
;-------------------------------
    mov     ah,0x0b             ; Set color palette
    mov     bh,0                ; Background color
    mov     bl,4                ; Black
    int     0x10
    mov     ax,0x0006           ; CGA 640x200 BW graphics mode
    int     0x10

;-------------------------------
; Copy picture data
;-------------------------------
    mov     ax,0xb800           ; CGA EVEN lines mem
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
    loop .copy_lines
    
    mov     ax,es
    cmp     ax,0xb800           ; Are we done?
    jnz     .waitkey
    mov     ax,0xba00           ; Copy odd lines as well
    mov     es,ax
    mov     si,picture          ; Odd lines source address
    jmp .copy_plane
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

filename:
    db "rainbow.raw",0
picture:
