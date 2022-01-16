;-----------------------------------------------------------
; Test CGA B&W 640x200 screen mode, by displaying bitmap
; - int 0x10, setscreen 0x0006
; - use 16k 0xB800:0000 CGA screen buffer
; - int 0x10, setscreen 0x0003
; - int 0x10, cursorpos 0x0200
; Note:
; The bitmap is a truncated 1bpp BMP file, without headers,
; with exactly 640x200 pixels. The row order in BMP is
; reversed, while the column order is the same as screen.
;-----------------------------------------------------------
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
; Detect CGA.com
;-------------------------------
    mov     ax,0xff00           ; Detect command
    int     0x10
    cmp     ax,'C'<<8|'G'
    jnz     .CGACOM_NotPresent
    
    mov     ax,0xff01           ; Get display segment
    int     0x10
    mov     word [display],ax

.CGACOM_NotPresent:
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
    mov     ax,[display]        ; CGA EVEN lines mem (normally B800h)
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
    cmp     ax,[display]        ; Are we done?
    jnz     PictureReady
    add     ax,0x200            ; Copy odd lines as well
    mov     es,ax
    mov     si,picture          ; Odd lines source address
    jmp .copy_plane
PictureReady:
;-------------------------------
; Write filename at the bottom
;-------------------------------
    ; x = (80-12)/2=34, y=200-8, l=12
    ; BIOS Char Map is at FFA6:000E, 8 bytes a char
    mov     es,[display]
    mov     di,(100-4)*80+34    ; Interlaced!!
    mov     ax,0xffa6
    mov     ds,ax
    mov     bx,filename

.next_char:
    xor     ax,ax
    cs mov  al,byte [bx]
    or      al,al
    jz      .waitkey
    shl     ax,1                ; AX=AX*8
    shl     ax,1
    shl     ax,1
    add     ax,0x0e
    mov     si,ax
    mov     cx,4
.copy_char:
    movsb                       ; Even line
    add     di,0x2000-1
    movsb                       ; Odd line
    sub     di,0x2000+1-80      ; Next even line
    loop    .copy_char
    sub     di,4*80-1           ; Next even line to the right
    inc     bx
    jmp     .next_char
    
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

display:
    dw 0xb800
filename:
    db "rainbow.raw",0
picture:
