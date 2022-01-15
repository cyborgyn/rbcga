;********************************************************************
;                                                                   *
;       p r o c e d u r e    i n i t _ o p t i o n                  *
;                                                                   *
;       purpose:        initialize the graphics option              *
;                                                                   * 
;       entry:          dx = 1     medium resolution                *
;                       dx = 2     high resolution                  *
;       exit:           all shadow bytes initialized                *
;       register usage: none, all registers are saved               *
;********************************************************************
cseg    segment byte    public  'codesg'
extrn   alups:near,pattern_register:near,pattern_mult:near,fgbg:near
        public  init_option
        assume  cs:cseg,ds:dseg,es:dseg,ss:nothing
init_option     proc    near
        push    ax              ;save the registers
        push    bx
        push    cx
        push    dx
        push    di
        push    si
        cld                     ;make sure that stos incs.
;
;First we have to find out what the interupt vector is for the 
;graphics option.  If this is a Model 100-A, interrupt vector 
;22h is the graphics interrupt.  If this is a Model 100-B, the 
;interrupt vector is relocated up to A2.  If EE00:0F44h and 
;04<>0, we have the relocated vectors of a Model 100-B and need 
;to OR the msb of our vector.
;
        mov     ax,ds
        mov     word ptr cs:segment_save,ax
        push    es              ;save valid es
        mov     bx,0ee00h       ;test if vectors are relocated
        mov     es,bx
        mov     ax,88h               ;100-A int. vector base addr
        test    es:byte ptr 0f44h,4  ;relocated vectors?
        jz      g0                   ;jump if yes
        mov     ax,288h              ;100-B int. vector base addr
g0:     mov     word ptr g_int_vec,ax
        pop     es
        cmp     dx,1            ;medium resolution?
        jz      mid_res         ;jump if yes
        jmp     hi_res          ;else is high resolution
mid_res:
        mov     al,00           ;medium resolution reset command
        out     57h,al
        mov     gbmod,030h      ;mode = med res, text, no readback
        call    mode            ;turn off graphics output 
        mov     al,12h          ;p1. refresh, draw enabled during 
        out     056h,al         ;retrace
        mov     al,16h          ;p2. 24 words/line minus 2
        out     056h,al         ;384/16 pixels/word=24 words/line
        mov     al,61h          ;p3. 3 bits vs/5 bits hs width - 1
        out     056h,al         ;vs=3, hs=2
        mov     al,04           ;p4. 6 bits hfp-1, 2 bits vs high 
        out     056h,al         ;byte, 2 words hfp, no vs high byte
        mov     al,02           ;p5. hbp-1, 3 words hbp
        out     056h,al
        mov     al,03           ;p6. vertical front porch, 3 lines
        out     056h,al
        mov     al,0f0h         ;p7. active lines displayed
        out     056h,al
        mov     al,40h          ;p8. 6 bits vbp/2 bits lines/field
        out     056h,al         ;high byte, vbp=16 lines
        mov     al,047h         ;pitch command, med res, straight up
        out     057h,al
        mov     al,32           ;med res memory width for vert. pitch
        out     056h,al
        mov     word ptr nmritl,3fffh
        mov     word ptr xmax,383       ;384 pixels across in med res
        mov     byte ptr num_planes,4   ;4 planes in med res
        mov     byte ptr shifts_per_line,5 ;rotates for 32 wds/line
        mov     byte ptr words_per_line,32 ;words in a line
        jmp     common_init
hi_res: mov     al,00           ;high resolution reset command
        out     57h,al
        mov     gbmod,031h      ;mode = high res, text, no readback
        call    mode            ;disable graphics output 
        mov     al,12h          ;p1. refresh, draw enabled during 
        out     056h,al         ;retrace
        mov     al,30h          ;p2. 50 words/line - 2
        out     056h,al
        mov     al,64h          ;p3. hsync w-1=4(low 5 bits), vsync 
        out     056h,al         ;w=3(upper three bits)
        mov     al,08           ;p4. hor fp w-1=2(upper 2 bits),
        out     056h,al         ;vsync high byte = 0
        mov     al,03           ;p5. hbp-1. 3 words hbp
        out     056h,al
        mov     al,03           ;p6. vertical front porch, 3 lines
        out     056h,al
        mov     al,0f0h         ;p7. active lines displayed
        out     056h,al 
        mov     al,40h          ;p8. 6 bits vbp/2 bits lines per field
        out     056h,al         ;high byte. vbp=16 lines
        mov     al,047h         ;pitch command, high res, straight up
        out     057h,al
        mov     al,64           ;high res pitch is 64 words/line
        out     056h,al
        mov     word ptr nmritl,7fffh
        mov     word ptr xmax,799       ;800 pixels across
        mov     byte ptr num_planes,2   ;2 planes in high res
        mov     byte ptr shifts_per_line,6 ;shifts for 64 wds/line
        mov     byte ptr words_per_line,64 ;number of words/line
common_init:
        mov     al,00           ;setup start window display for memory 
        mov     startl,al       ;location 00
        mov     starth,al
        mov     al,06bh         ;start command
        out     057h,al         ;start the video signals going
        mov     al,046h         ;zoom command
        out     057h,al
        mov     al,0            ;magnification assumed to be 0
        out     056h,al
        mov     al,22h          ;setup R/M/W memory cycles for 
        out     57h,al          ;figure drawing
;
;Initialize PRAM command. Start window at the address in startl,
;starth.  Set the window length for 256 lines. Fill PRAM parameters 
;8 and 9 with all ones so GDC can do graphics draw commands without 
;altering the data we want drawn.
;
        mov     al,070h         ;issue the pram command, setup 
        out     057h,al         ;GDC display
        mov     al,startl       ;p1. display window starting address 
        out     056h,al         ;low byte
        mov     al,starth       ;p2. display window starting address 
        out     056h,al         ;high byte
        mov     al,0ffh         ;p3. make window 256 lines
        out     056h,al
        mov     al,0fh          ;p4. high nibble display line on 
        out     056h,al         ;right, the rest = 0
        mov     al,078h         ;issue pram command pointing to p8
        out     057h,al
        mov     al,0ffh         ;fill pram with ones pattern
        out     056h,al
        out     056h,al
        mov     al,04bh         ;issue the cchar command
        out     057h,al
        xor     al,al           ;initialize cchar parameter bytes 
        mov     cchp1,al        ;graphics cursor is one line, not 
        out     056h,al         ;displayed, non-blinking
        mov     cchp2,al
        out     056h,al
        mov     cchp3,al
        out     056h,al
        mov     al,06fh         ;vsync command
        out     057h,al
        out     050h,al         ;reset the graphics board
        mov     al,0bfh
        out     53h,al
        mov     al,byte ptr gbmod  ;enable, then disable interrupts 
        or      al,40h             ;to flush the interrupt hardware 
        out     51h,al             ;latches
        mov     cx,4920            ;wait for a vert sync to happen
g1:     loop    g1
        mov     al,0bfh            ;disable the interrupts
        out     53h,al
        mov     al,byte ptr gbmod
        out     51h,al
        call    assert_colormap    ;load colormap 
        call    inscrl             ;initialize scroll map
        mov     bl,1               ;set pattern multiplier to 16-bl 
        call    pattern_mult       ;see example "pattern_mult"
        mov     bl,0ffh            ;set pattern data of all bits set
        call    pattern_register   ;see example "pattern_register"
        mov     bl,0f0h            ;enable all foreground registers
        call    fgbg               ;see example "fgbg"
        mov     bl,0               ;enable planes 0-3, REPLACE logic
        call    alups              ;see example "alups"
        mov     di,offset p1       ;fill the p table with ff's.
        mov     al,0ffh
        mov     cx,16
        rep     stosb
        mov     al,0               ;enable all gb mask writes.
        mov     gbmskl,al
        mov     gbmskh,al
        mov     al,0ffh            ;set GDC mask bits
        mov     gdcml,al
        mov     gdcmh,al
        mov     word ptr curl0,0   ;set cursor to top screen left
        mov     ax,word ptr gbmskl ;fetch and issue the graphics
        out     54h,al             ;option text mask
        mov     al,ah
        out     55h,al
        call    setram                  ;then set ram to p1 thru p16 data
        mov     word ptr ymax,239
        mov     al,0dh
        out     57h,al             ;enable the display
        pop     si                 ;recover the registers
        pop     di
        pop     dx
        pop     cx
        pop     bx
        pop     ax      
        ret
init_option     endp


;********************************************************************
;*                                                                  *
;*      g r a p h i c s   s u b r o u t i n e s                     *
;*                                                                  *
;********************************************************************
;
gsubs   proc near
public  setram,assert_colormap,gdc_not_busy,imode,color_int,scrol_int
public  cxy2cp,mode
;
;********************************************************************
;                                                                   *
;       s u b r o u t i n e    a s s e r t _ c o l o r m a p        *
;                                                                   *
;       colormap is located at clmpda which is defined in           *
;       procedure "set_color"                                       *
;                                                                   *
;       entry:          clmpda = colormap to be loaded              *
;       exit:           none                                        *
;       register usage: ax,bx                                       *
;********************************************************************
;
assert_colormap:
        cld
        call    gdc_not_busy    ;make sure nothing's happening
;
;The graphics interrupt vector "giv" is going to be either 22h or 
;A2h depending on whether this is a Model 100-A or a Model 100-B 
;with relocated vectors. Read the old vector, save it, then 
;overwrite it with the new vector.
;
        push    es
        xor     ax,ax
        mov     es,ax
        mov     bx,word ptr g_int_vec   ;fetch address of "giv"
        cli                             ;temp. disable interrupts
        mov     ax,es:[bx]              ;read the old offset
        mov     word ptr old_int_off,ax
        mov     ax,es:[bx+2]            ;read the old segment
        mov     word ptr old_int_seg,ax
        mov     word ptr es:[bx],offset color_int ;load new offset.
        mov     ax,cs
        mov     es:[bx+2],ax            ;load new int segment
        sti                             ;re-enable interrupts
        pop     es
        mov     byte ptr int_done,0     ;clear interrupt flag
        or      byte ptr gbmod,40h      ;enable graphics interrupt
        call    mode
ac1:    test    byte ptr int_done,0ffh  ;has interrupt routine run?
        jz      ac1
        push    es                      ;restore interrupt vectors
        xor     ax,ax
        mov     es,ax
        mov     bx,word ptr g_int_vec   ;fetch graphics vector offset
        cli
        mov     ax,word ptr old_int_off ;restore old interrupt vector
        mov     es:[bx],ax
        mov     ax,word ptr old_int_seg 
        mov     es:[bx+2],ax
        sti
        pop     es
        cld                             ;make lods inc si
        ret
color_int:
        push    es
        push    ds
        push    si
        push    cx
        push    ax
        mov     ax,word ptr cs:segment_save ;can't depend on es or ds 
        mov     ds,ax                       ;reload segment registers
        mov     es,ax
        cld
        and     byte ptr gbmod,0bfh     ;disable graphics interrupts
        call    mode
        mov     si,offset clmpda        ;fetch color source
        mov     al,0dfh                 ;get the color map's attention
        out     053h,al
        mov     cx,32           ;32 color map entries
ci1:    lodsb                   ;fetch current color map data
        out     051h,al         ;load color map
        loop    ci1             ;loop until all color map data loaded
        mov     byte ptr int_done,0ffh  ;set "interrupt done" flag
        pop     ax
        pop     cx
        pop     si
        pop     ds
        pop     es
        iret
;
;********************************************************************
;                                                                   *
;       s u b r o u t i n e    c x y 2 c p                          *
;                                                                   *
;       CXY2CP takes the xinit and yinit numbers, converts them to  *
;       an absolute memory location and puts that location into     *
;       curl0,1,2.  yinit is multiplied by the number of words per  *
;       line.  The lower 4 bits of xinit are shifted to the left    *
;       four places and put into curl2. xinit is shifted right four *
;       places to get rid of pixel information and then added to    *
;       yinit times words per line.  This result becomes curl0,     *
;       curl1.                                                      *
;                                                                   *
;       entry:          xinit = x pixel location                    *
;                       yinit = y pixel location                    *
;       exit:           curl0,1,2                                   *
;       register usage: ax,bx,cx,dx                                 *
;********************************************************************
;
cxy2cp: mov     cl,byte ptr shifts_per_line
        mov     ax,yinit        ;compute yinit times words/line
        shl     ax,cl           ;ax has yinit times words/line
        mov     bx,xinit        ;calculate the pixel address
        mov     dx,bx           ;save a copy of xinit
        mov     cl,4            ;shift xinit 4 places to the left
        shl     bl,cl           ;bl has pixel within word address
        mov     curl2,bl        ;pixel within word address
        mov     cl,4            ;shift xinit 4 places to right 
        shr     dx,cl           ;to get xinit words
        add     ax,dx
        mov     word ptr curl0,ax   ;word address
        ret
;********************************************************************
;                                                                   *
;       s u b r o u t i n e    g d c _ n o t _ b u s y              *
;                                                                   *
;       gdc_not_busy will put a harmless command into the GDC and   *
;       wait for the command to be read out of the command FIFO.    *
;       This means that the GDC is not busy doing a write or read   *
;       operation.                                                  *
;                                                                   *
;       entry:          none                                        *
;       exit:           none                                        *
;       register usage: ax                                          *
;********************************************************************
;
gdc_not_busy:
        push    cx              ;use cx as a time-out loop counter
        in      al,056h         ;first check if the FIFO is full
        test    al,2
        jz      gnb2            ;jump if not
        mov     cx,8000h        ;wait for FIFO not full or reasonable 
gnb0:   in      al,056h         ;time, whichever happens first
        test    al,2            ;has a slot opened up yet?
        jz      gnb2            ;jump if yes
        loop    gnb0            ;if loop count exceeded, go on anyway
gnb2:   mov     al,0dh          ;issue a screen-on command to GDC
        out     057h,al
        in      al,056h         ;did that last command fill it?
        test    al,2
        jz      gnb4            ;jump if not
        mov     cx,8000h
gnb3:   in      al,056h         ;read status register
        test    al,2            ;test FIFO full bit
        jnz     gnb4            ;jump if FIFO not full
        loop    gnb3            ;loop until FIFO not full or give up
gnb4:   mov     ax,40dh         ;issue another screen-on, 
        out     057h,al         ;wait for FIFO empty
        mov     cx,8000h
gnb5:   in      al,056h         ;read the GDC status
        test    ah,al           ;FIFO empty bit set?
        jnz     gnb6            ;jump if not.
        loop    gnb5
gnb6:   pop     cx
        ret
;********************************************************************
;                                                                   *
;       s u b r o u t i n e    i m o d e                            *
;                                                                   *
;       issue Mode command with the parameters from register gbmod  *
;                                                                   *
;       entry:          gbmod                                       *
;       exit:           none                                        *
;       register usage: ax                                          *
;********************************************************************
;
imode:  call    gdc_not_busy
        mov     al,0bfh         ;address the mode register through 
        out     53h,al          ;the indirect register
        mov     al,gbmod
        out     51h,al          ;load the mode register
        ret
mode:   mov     al,0bfh         ;address the mode register through 
        out     53h,al          ;the indirect register
        mov     al,gbmod
        out     51h,al          ;load the mode register
        ret
;********************************************************************
;                                                                   *
;       s u b r o u t i n e    i n s c r l                          *
;                                                                   *
;       initialize the scroll map                                   *
;                                                                   *
;       entry:          none                                        *
;       exit:           none                                        *
;       register usage: ax,bx,cx,dx,di,si                           *
;********************************************************************
;
inscrl: cld
        mov     cx,256          ;initialize all 256 locations of the 
        xor     al,al           ;shadow area to desired values
        mov     di,offset scrltb    
insc0:  stosb
        inc     al
        loop    insc0
;
;The graphics interrupt vector is going to be either 22h or A2h 
;depending on whether this is a Model 100-A or a Model 100-B with 
;relocated vectors.  Read the old vector, save it, and overwrite it
;with the new vector.  Before we call the interrupt, we need to 
;make sure that the GDC is not in the process of writing something 
;out to the bitmap.
;
ascrol: call    gdc_not_busy          ;check if GDC id busy
        push    es
        xor     ax,ax
        mov     es,ax
        mov     bx,word ptr g_int_vec
        cli                           ;temporarily disable interrupts
        mov     ax,es:[bx]            ;read the old offset
        mov     word ptr old_int_off,ax
        mov     ax,es:[bx+2]          ;read the old segment
        mov     word ptr old_int_seg,ax
        mov     word ptr es:[bx],offset scrol_int ;load new offset
        mov     ax,cs
        mov     es:[bx+2],ax          ;load new interrupt segment
        sti                           ;re-enable interrupts
        pop     es
        mov     byte ptr int_done,0   ;clear interrupt flag
        or      byte ptr gbmod,40h    ;enable graphics interrupt
        call    mode
as1:    test    byte ptr int_done,0ffh ;has interrupt routine run?
        jz      as1
        push    es                    ;restore the interrupt vectors
        xor     ax,ax
        mov     es,ax
        mov     bx,word ptr g_int_vec ;fetch graphics vector offset
        cli
        mov     ax,word ptr old_int_off ;restore old interrupt vector
        mov     es:[bx],ax
        mov     ax,word ptr old_int_seg 
        mov     es:[bx+2],ax
        sti
        pop     es
        ret
;
;Scrollmap loading during interrupt routine.
;Fetch the current mode byte and enable scroll map addressing.
;
scrol_int:
        push    es
        push    ds
        push    si
        push    dx
        push    cx
        push    ax
        cld
        mov     ax,word ptr cs:segment_save ;can't depend on ds
        mov     ds,ax                       ;reload it
        mov     es,ax
        and     byte ptr gbmod,0bfh   ;disable graphics interupts
        mov     al,gbmod        ;prepare to access scroll map 
        mov     gtemp1,al       ;first save current gbmod
        and     gbmod,0dfh      ;enable writing to scroll map
        call    mode            ;do it
        mov     al,07fh         ;select scroll map and reset scroll
        out     53h,al          ;map address counter
        mov     dl,51h          ;output port destination.
        xor     dh,dh
        mov     si,offset scrltb  ;first line's high byte address=0
        mov     cx,16             ;256 lines to write to
        test    byte ptr gbmod,1  ;high resolution?
        jnz     ins1              ;jump if yes  
        shr     cx,1            ;only 128 lines if medium resolution
ins1:   lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        lodsw                   ;fetch two scrollmap locations
        out     dx,al           ;assert the even byte
        mov     al,ah
        out     dx,al           ;assert the odd byte
        loop    ins1
        mov     al,gtemp1       ;restore previous mode register 
        mov     gbmod,al
        call    mode
        mov     byte ptr int_done,0ffh  ;set interrupt-done flag
        pop     ax
        pop     cx
        pop     dx
        pop     si
        pop     ds
        pop     es
        iret                    ;return from interrupt
;********************************************************************
;                                                                   *
;       s u b r o u t i n e    s e t r a m                          *
;                                                                   *
;       set video ram to a value stored in the p table              *
;                                                                   *
;       entry:          16 byte p1 table                            *
;       exit:           none                                        *
;       register usage: ax,bx,cx,dx,di,si                           *
;********************************************************************
;
setram: mov     byte ptr twdir,2  ;set write direction to --->
        call    gdc_not_busy      ;make sure that the GDC isn't busy
        mov     al,0feh           ;select the write buffer 
        out     053h,al
        out     051h,al         ;reset the write buffer counter
        mov     si,offset p1    ;initialize si to start of data 
        mov     cx,10h          ;load 16 chars into write buffer
setr1:  lodsb                   ;fetch byte to go to write buffer
        out     52h,al
        loop    setr1
        mov     al,0feh         ;select the write buffer
        out     053h,al
        out     051h,al         ;reset the write buffer counter
        mov     al,049h         ;issue GDC cursor location command
        out     57h,al
        mov     al,byte ptr curl0 ;fetch word location low byte
        out     56h,al            ;load parameter
        mov     al,byte ptr curl1 ;fetch word location high byte
        out     56h,al            ;load parameter
        mov     al,4ah            ;set the GDC mask to all F's
        out     57h,al
        mov     al,0ffh
        out     56h,al
        out     56h,al
        mov     al,04ch           ;issue figs command
        out     57h,al
        mov     al,byte ptr twdir ;direction to write.
        out     56h,al
        mov     al,nmritl         ;number of GDC writes, low byte
        out     56h,al
        mov     al,nmrith         ;number of GDC writes, high byte
        out     56h,al
        mov     al,22h            ;wdat command 
        out     57h,al
        mov     al,0ffh      ;p1 and p2 are dummy parameters
        out     56h,al       ;the GDC requires them for internal
        out     56h,al       ;purposes - no effect on the outside
        ret
segment_save    dw      0         ;ds save area for interrupts
gsubs   endp
        cseg    ends
dseg    segment byte    public  'datasg'
extrn   clmpda:byte
public  xmax,ymax,alu,d,d1,d2,dc
public  curl0,curl1,curl2,dir,fg,gbmskl,gbmskh,gbmod,gdcml,gdcmh
public  nmredl,nmredh,nmritl,nmrith,p1,prdata,prmult,scrltb,startl
public  gtemp3,gtemp4,starth,gtemp,gtemp1,gtemp2,twdir,xinit,xfinal
public  yinit,yfinal,ascrol,num_planes,shifts_per_line
public  words_per_line,g_int_vec
;
;variables to be remembered about the graphics board states
;
alu     db      0       ;current ALU state
cchp1   db      0       ;cursor/character 
cchp2   db      0       ;    size definition
cchp3   db      0       ;        parameter bytes
curl0   db      0       ;cursor          - low byte  
curl1   db      0       ;  location      - middle byte 
curl2   db      0       ;    storage     - high bits & dot address
dc      dw      0       ;figs command dc parameter
d       dw      0       ;figs command d parameter
d2      dw      0       ;figs command d2 parameter
d1      dw      0       ;figs command d1 parameter
dir     db      0       ;figs direction.
fg      db      0       ;current foreground register
gbmskl  db      0       ;graphics board mask register - low byte
gbmskh  db      0       ;                             - high byte
gbmod   db      0       ;graphics board mode register 
gdcml   db      0       ;GDC mask register bits - low byte
gdcmh   db      0       ;                       - high byte
g_int_vec       dw      0   ;graphics option's interrupt vector
gtemp   dw      0           ;temporary storage
gtemp1  db      0           ;temporary storage
gtemp2  db      0           ;temporary storage
gtemp3  db      0           ;temporary storage
gtemp4  db      0           ;temporary storage
int_done        db      0   ;interrupt-done state
nmredl  db      0         ;number of read operations - low byte
nmredh  db      0         ;                          - high byte
nmritl  db      0         ;number of GDC writes - low byte
nmrith  db      0         ;                     - high byte
num_planes      db      0 ;number of planes in current resolution
old_int_seg     dw      0 ;old interrupt segment
old_int_off     dw      0 ;old interrupt offset
p1      db      16 dup (?) ;shadow write buffer & GDC parameters
prdata  db      0         ;pattern register data
prmult  db      0         ;pattern register multiplier factor
scrltb  db      100h dup (?) ;scroll map shadow area
si_temp dw      0
startl  db      0         ;register for start address of display
starth  db      0
twdir   db      0         ;direction for text mode write operation
shifts_per_line db      0 ;shift factor for one line of words
words_per_line  db      0 ;words/scan line for current resolution
xinit   dw      0         ;x initial position
yinit   dw      0         ;y initial position
xfinal  dw      0         ;x final position
yfinal  dw      0         ;y final position
xmax    dw      0
ymax    dw      0
dseg            ends
        end



;********************************************************************
;                                                                   *
;       p r o c e d u r e    g r a p h i c s _ o n                  *
;                                                                   *
;       purpose:        enable graphics output on single            *
;                       color monitor                               *
;                                                                   *
;       entry:          gbmod contains mode register shadow byte    *
;       exit:           none                                        *
;       register usage: ax                                          *
;********************************************************************
;
dseg    segment byte    public  'datasg'
extrn   gbmod:byte      ;defined in procedure 'init_option'
dseg    ends
cseg    segment byte    public  'codesg'
extrn   imode:near      ;defined in procedure 'init_option'
        public  graphics_on
        assume  cs:cseg,ds:dseg,es:dseg,ss:nothing
;
graphics_on     proc    near
        mov     al,87h
        out     0ah,al          ;enable graphics on monochrome line
        or      byte ptr gbmod,080h  ;enable graphics output in gbmod
        call    imode           ;assert new mode register
        ret                     ;
graphics_on     endp
cseg    ends
        end

;********************************************************************
;                                                                   *
;       p r o c e d u r e    g r a p h i c s _ o f f                *
;                                                                   *
;       purpose:        disable graphics output to single           *
;                       (color) monitor                             *
;                                                                   *
;       entry:          gbmod contains mode register shadow byte    *
;       exit:           none                                        *  
;       register usage: ax                                          *
;********************************************************************
;
dseg    segment byte    public  'datasg'
extrn   gbmod:byte      ;defined in procedure 'init_option'
dseg    ends
cseg    segment byte    public  'codesg'
extrn   imode:near      ;defined in procedure 'init_option'
        public  graphics_off
        assume  cs:cseg,ds:dseg,es:dseg,ss:nothing
;
graphics_off    proc    near
        and     byte ptr gbmod,07fh ;disable graphics output in gbmod
        call    imode           ;assert new mode register
        mov     al,83h
        out     0ah,al          ;turn off graphics on monochrome line
        ret
graphics_off    endp
cseg    ends
        end


;************************************************************************
;*                                                                      *
;*              p r o c e d u r e   c h a n g e  c o l o r m a p        *
;*                                                                      *
;*  purpose: change a color in the colormap.                            *
;*  entry:   ax = new color                                             *
;*                al = high nibble = red data
;*                     low nibble  = green data
;*                ah = high nibble = grey data
;*                     low nibble  = blue data
;*           bx = palette entry number                                  *
;*                                                                      *
;*  exit:                                                               *
;*                                                                      *
;************************************************************************
extrn   fifo__empty:near
cseg    segment byte    public  'codesg'
        public  change__colormap
        public  load__colormap
        assume  cs:cseg,ds:dseg,es:dseg,ss:nothing

change__colormap proc    near
        mov     si,offset clmpda        ;colormap shadow.
        mov     [si+bx],al              ;store the red and green data.
        add     bx,16
        mov     [si+bx],ah              ;store the grey and blue data.
        jmp     load__colormap           ;asssert the new colors.
change__colormap endp




;************************************************************************
;*                                                                      *
;*              p r o c e d u r e   l o a d  c o l o r m a p            *
;*                                                                      *
;*  purpose: move the data currently in clmpda into the graphics        *
;*              option's colormap.                                      *
;*  entry:   si points to a list of 32 bytes to be loaded into the      *
;*           graphics option colormap.                                  *
;*  exit:                                                               *
;*                                                                      *
;*                                                                      *
;************************************************************************


load__colormap   proc    near

        mov     si,offset clmpda        ;assume clmpda contains color map
;wait for a vertical retrace to start. because of the way the hardware is
;constructed it is best if we load the colormap during a time when the gdc is
;not trying to apply addresses to it from the bitmap. we could have set up
;an interrupt but this is an easier way of doing things and, under the
;curcumstances, good enough. we want to make sure that we catch the beginning
;of a vertical retrace so first we check for vertical retrace inactive and
;then look for the retrace to start.

        mov     bl,20h          ;wait for no retrace.
here1:  in      al,56h          ;read gdc status register
        test    al,bl           ;verticle sync active?
        jnz     here1           ;keep jumping until it isn't.

here2:  in      al,56h          ;now wait vert retrace to start.
        test    al,bl           ;keep looping until vert sync goes active.
        jz      here2

;3)enable colormap writes by enabling it through an access to the indirect
;register select port 53h.

        mov     al,0dfh         ;get the color map's attention
        out     53h,al

;4)now the 16 words composing the entire colormap will be transfered from
;the 32 byte table that si is pointing to. the 16 words are transfered as
;32 bytes, first the 16 bytes containing the red and green information and
;then the 16 bytes containing the grey and blue data.

        cld                     ;make sure that the lods increments si.
        mov     dx,51h
        mov     cx,32           ;32 color map entries
here3:  lodsb                   ;fetch current color map data
        out     dx,al           ;load color map
        loop    here3           ;loop if not all 32 color map datas loaded
        call    fifo__empty      ;gdc status check, see example 03
        ret
load__colormap   endp
cseg    ends
dseg    segment byte    public  'datasg'
public  clmpda


;colormaps:
;---------
;in general, colormap format is 16 bytes of red and green data,then
;16 bytes of grey and blue data. 0 specifies full intensity, while 0fh
;specifies zero intensity. an possible color map for a 100b, monochrome
;monitor only system in medium resolution (16 colors) would look as follows:

;clmpda                 db      0ffh    ; no red or green data
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                                       ;grey data, no blue data
;                       db      0ffh    ;black
;                       db      00fh    ;white
;                       db      01fh    ;light grey
;                       db      02fh    ;v
;                       db      03fh    ;v
;                       db      04fh    ;v
;                       db      05fh    ;v
;                       db      06fh    ;v
;                       db      07fh    ;medium gray
;                       db      08fh    ;v
;                       db      09fh    ;v
;                       db      0afh    ;v
;                       db      0bfh    ;v
;                       db      0cfh    ;v
;                       db      0dfh    ;v
;                       db      0efh    ;dark grey
;
;on a 100a, only the lower two bits of the monochrome nibble are
;significant, giving only four shades of grey,as opposed to 16 shade on
;the 100b. a sample map for the 100a, monochrome only system, medium
;or high resolution, would look as follows:
;
;clmpda                 db      0ffh    ;no red or green info
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                                       ;grey info, no blue
;                       db      0ffh    ;black
;                       db      0cfh    ;white
;                       db      0dfh    ;light grey
;                       db      0efh    ;dark grey
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;
;a hires color map for a 100b would consist of 4 colors defined that
;utilize all 4 bits of the grey nibble and  would look like this:

;clmpda                 db      0ffh    ;no red or green data
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                       db      0ffh
;                                       ;grey info, no blue info
;                       db      0ffh    ;black
;                       db      00fh    ;white
;                       db      06fh    ;light grey
;                       db      0afh    ;dark grey
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;                       db      0ffh    ;black
;
;in a dual monitor configuration, medium resolution mode, on a 100b,
;there are 4 bits each of red,green,blue and grey. an example colormap would
;be as follows:

;clmpda                 db      0ffh    ;black  -red data,green data
;                       db      000h    ;white
;                       db      0f0h    ;cyan
;                       db      00fh    ;magenta
;                       db      000h    ;
;                       db      00fh    ;red
;                       db      0ffh    ;blue
;                       db      0f0h    ;green
;                       db      0aah    ;dk grey
;                       db      0f8h    ;dk cyan
;                       db      08fh    ;dk magenta
;                       db      088h    ;
;                       db      08fh    ;red
;                       db      0ffh    ;blue
;                       db      0f8h    ;green
;                       db      077h    ;dk grey
;
;                       db      0ffh    ;black,black -grey data,blue data
;                       db      000h    ;white,white
;                       db      010h    ;lightgrey,cyan
;                       db      020h    ;v      ,magenta
;                       db      03fh    ;v
;                       db      04fh    ;v      ,red
;                       db      050h    ;v      ,blue
;                       db      06fh    ;v      ,green
;                       db      07ah    ;medgrey,dk grey
;                       db      0f8h    ;v      ,dk cyan
;                       db      098h    ;       ,dk magenta
;                       db      0afh    ;v
;                       db      0bfh    ;       ,dk red
;                       db      0c8h    ;       ,dk blue
;                       db      0dfh    ;v      ,dk green
;                       db      0e7h    ;dkgrey ,grey
;
;on a 100a, dual monitor configuration, in medium resolution mode, there
;are 4 bits each of red, green, and blue data, all 16 colors, but only 2
;bits of grey data, allowing for only 4 shades grey.

;on a 100a, in high resolution, dual monitor configuration, there are 4
;displayable colors and 2 levels of grey.

;on a 100b, in high resolution, dual monitor configuration, there are 4
;displayable colors and 4 levels of grey.

;in the case of a color monitor only system, the green data must be mapped
;to the monochrome output. for a single color monitor system, medium resolution,
;on a 100b, a sample color map would be as follows:

clmpda                  db      0ffh    ;black  -red data,green mapped to grey
                        db      00fh    ;white
                        db      0ffh    ;cyan
                        db      00fh    ;magenta
                        db      00fh    ;
                        db      00fh    ;red
                        db      0ffh    ;blue
                        db      0ffh    ;green
                        db      0afh    ;gray
                        db      0ffh    ;dk cyan
                        db      08fh    ;dk magenta
                        db      08fh    ;
                        db      08fh    ;dk red
                        db      0ffh    ;dk blue
                        db      0ffh    ;dl green
                        db      07fh    ;gray
;
                        db      0ffh    ;black -green data,blue data
                        db      000h    ;white
                        db      000h    ;cyan
                        db      0f0h    ;magenta
                        db      00fh    ;
                        db      0ffh    ;red
                        db      0f0h    ;blue
                        db      00fh    ;green
                        db      0aah    ;gray
                        db      088h    ;dk cyan
                        db      0f8h    ;dk magenta
                        db      08fh    ;
                        db      0ffh    ;dk red
                        db      0f8h    ;dk blue
                        db      08fh    ;dk green
                        db      077h    ;gray

;as with the previous examples, the same differences apply to high
;resolution (only four colors are displayable) and on the 100a, only
;the lower two bits on the grey nibble are significant (giving only
;four shades of green, since the green data must be output through the
;monochrome line, in either high or medium resolution.

dseg    ends
        end


;*****************************************************************************
;                                                                            *
;        p r o c e d u r e    a l u p s                                      *
;                                                                            *
;        purpose:        Set the ALU / Plane Select Register                 *
;                                                                            *
;        entry:          bl = value to load into ALU/PS Register             *
;                                                                            *
;                                                                            *
;*****************************************************************************
;
cseg     segment byte    public  'codesg'
         extrn   fifo_empty:near
         public  alups
         assume  cs:cseg,ds:nothing,es:nothing,ss:nothing
alups    proc    near                    
         call    fifo_empty
         mov     al,0efh         ;select the ALU/PS Register
         out     53h,al
         mov     al,bl           ;move ALU/PS value to al
         out     51h,al          ;load value into ALU/PS Register
         ret
alups    endp
cseg     ends
         end
		 

;*****************************************************************************
;                                                                            *
;        p r o c e d u r e    f g b g                                        *
;                                                                            *
;        purpose:        Load the Foreground/Background Register             *
;                                                                            *
;        entry:          bl = value to load into the FgBg register           *
;                                                                            *
;                                                                            *
;*****************************************************************************
cseg     segment byte    public  'codesg'
         extrn   fifo_empty:near
         public  fgbg
         assume  cs:cseg,ds:nothing,es:nothing,ss:nothing
fgbg     proc    near
         call    fifo_empty
         mov     al,0f7h         ;select the Foreground/Background Register
         out     53h,al
         mov     al,bl
         out     51h,al          ;load the Foreground/Background Register
         ret
fgbg     endp
cseg     ends
         end


;*******************************************************************************
;                                                                              *
;                                                                              *
;               p r o c e e d u r e     r i t v i d                            *
;                                                                              *
;                                                                              *
;this proceedure will take the contents of the 64k buffer vidsg and insert     *
;that data into the graphics option.                                           *
;                                                                              *
;                                                                              *
;                                                                              *
;                                                                              *
;*******************************************************************************


extrn   vidseg:near     ;dummy declaration- vidsg is undefined!!!

extrn   nmritl:word,gbmod:byte,gtemp:word,num__planes:byte,curl0:byte
extrn   ginit:near,ifgbg:near,gdc__not__busy:near,ialups:near

        dseg    segment byte    public 'datasg'

;       define the graphics commands
;
curs    equ     49h     ;cursor display position specify command
figs    equ     4ch
gmask   equ     4ah     ;sets which of the 16 bits/word affected
wdat    equ     20h     ;read modify write operation replacing screen data
s__off   equ     0ch     ;blank the display command
s__on    equ     0dh     ;turn display on command
;
;       define the graphics board port addresses
;
graf    equ     50h     ;graphics board base address port 0
gindo   equ     51h     ;graphics board indirect port enable out address
chram   equ     52h     ;character ram
gindl   equ     53h     ;graphics board indirect port in load address
cmaskh  equ     55h     ;character mask high
cmaskl  equ     54h     ;character mask low
gstat   equ     56h     ;gdc status reg (read only)
gpar    equ     56h     ;gdc command parameters (write only)
gread   equ     57h     ;gdc data read from vid mem (read only)
gcmd    equ     57h     ;gdc command port (write only)

;define the indirect register select enables

clrcnt  equ     0feh    ;clear character ram counter
patmlt  equ     0fdh    ;pattern multiplier register
patreg  equ     0fbh    ;pattern data register
fgbg    equ     0f7h    ;foreground/background enable
alups   equ     0efh    ;alu function plane select register
colmap  equ     0dfh    ;color map
modreg  equ     0bfh    ;mode register
scrlmp  equ     07fh    ;scroll map register

dseg ends

        assume  cs:cseg,ds:dseg,es:dseg,es:nothing

        cseg    segment byte    public 'codesg'
public  ritvid

ritvid  proc    near

;the video data is in vidseg. the last byte in vidseg is the resolution flag.
;if flag is=0 then mid res else is high res. init the option to that resolution.

        mov     ax,vidseg       ;setup es to point at the video buffer.
        mov     es,ax
        mov     si,0ffffh ;fetch the hires/lowres flag from vidbuf last byte.
        mov     al,es:[si]
        test    al,0ffh         ;high res?
        jnz     rt1             ;jump if yes.
        mov     dx,1
        jmp     rt2
rt1:    mov     dx,2
rt2:    call    ginit           ;assert the new resolution.

;init leaves us in text mode with a fg=f0 and a alups=0.

        mov     bl,0fh          ;put all ones into the bg, all 0's into the
        call    ifgbg           ;fg because the char ram inverts incoming data.
        mov     word ptr nmritl,07      ;do eight writes per access.
        test    byte ptr gbmod,1        ;high res?
        jnz     rt3                     ;jump if yes.
        mov     word ptr gtemp,2047     ;8 words writes/plane mid res.
        jmp     rt4
rt3:    mov     word ptr gtemp,4096     ;8 word writes/plane high res.

rt4:    mov     cl,byte ptr num__planes  ;fetch number of planes to be written.
        xor     ch,ch

;enable a plane to be written.

rt5:    push    cx                      ;save plane writing counter.
        mov     bl,byte ptr num__planes  ;select a plane to write enable.
        sub     bl,cl                   ;this is the plane to write enable.
        mov     cl,bl
        mov     bl,0feh                 ;put a 0 in that planes select position.
        ror     bl,cl
        and     bl,0fh                  ;keep in replace mode.
        call    ialups                  ;assert the new alups.

;fill that plane with data 8 words at a time from vidseg.

        mov     word ptr curl0,0        ;start the write at top left corner.
        mov     si,0                    ;start at the beginning of the vidbuf.
        mov     cx,word ptr gtemp       ;number of 8 word writes to fill plane.
rt6:    push    cx                      ;save 8 word write count.
        call    gdc__not__busy    ;wait until gdc has finished previous write.

        mov     cx,16           ;fetch 16 bytes.
rt7:    mov     al,es:[si]      ;fill ptable with data to be written.
        inc     si
        out     52h,al
        loop    rt7

        mov     al,curs         ;assert the position to start the write.
        out     57h,al
        mov     ax,word ptr curl0
        out     56h,al
        mov     al,ah
        out     56h,al
        mov     al,figs         ;init left gdc mask as ffffh and gbmask as 0.
        out     57h,al          ;all we need is to start the write.
        mov     al,2
        out     56h,al
        mov     al,7
        out     56h,al
        xor     al,al
        out     56h,al
        mov     al,22h
        out     57h,al
        mov     al,0ffh
        out     56h,al
        out     56h,al
        add     word ptr curl0,08       ;next location to be written.
        pop     cx
        loop    rt6             ;keep looping until this plane all written.

        pop     cx              ;keep looping until all planes written.
        loop    rt5
        ret

ritvid  endp

cseg ends

end

;************************************************************************
;*                                                                      *
;*      p r o c e d u r e s   t o   w r i t e   a   c o l o r           *
;*                                                                      *
;*      t o   a   r e c t a n g l e   o n   t h e   s c r e e n         *
;*                                                                      *
;*                                                                      *
;*                                                                      *
;*                                                                      *
;*                                                                      *
;*                                                                      *
;************************************************************************
        public  set_all_screen,set_rectangle
extrn   curl0:word,gbmod:byte,alups:near,xmax:word,ymax:word
extrn   fgbg:near,fifo_empty:near
dseg    segment byte    public  'datasg'
;
;       define the GDC commands
;
curs    equ     49h     ;cursor address specify command
figs    equ     4ch     ;figure specify command.
s_on    equ     0dh     ;bctrl command for screen on.
;
;       define the graphics board port addresses
;
cmaskl  equ     54h     ;write mask low byte
cmaskh  equ     55h     ;write mask high byte
gstat   equ     56h     ;GDC status reg (read only)
gcmd    equ     57h     ;GDC command port (write only)
xstart  dw      0
ystart  dw      0
xstop   dw      0
ystop   dw      0
nmritl   dw      0
dseg            ends
cseg    segment byte    public  'codesg'
        assume  cs:cseg,ds:dseg,es:nothing,ss:nothing
        subttl set all screen

;************************************************************************
;*                                                                      *
;*              p r o c e d u r e   s e t   a l l   s c r e e n         *
;*                                                                      *
;*  purpose:    set all of the screen to a user defined color.          *
;*  entry:      di is the color to clear the screen to.                 *
;*  exit:                                                               *
;*  registers:                                                          *
;*  stack usage:                                                        *
;*                                                                      *
;*                                                                      *
;*                                                                      *
;************************************************************************
set_all_screen  proc    near
;
;load ax and bx with 0. ax and bx will be used as the upper left corner
; of the rectangle to be written. load cx and dx with the maximum x and
;y of the screen. cx and dx are used to define the bottom right corner
;of the screen.
;
        mov     ax,0                    ;start at the top left corner.
        mov     bx,0
        mov     cx,word ptr xmax        ;fetch the bottom right corner
        mov     dx,word ptr ymax        ;coordinates.
        jmp     set_rectangle           ;lower right max setup by init.
set_all_screen  endp


        subttl set a rectangle to one color


;************************************************************************
;*                                                                      *
;*              p r o c e d u r e   s e t   r e c t a n g l e           *
;*                                                                      *
;*  purpose:    set a user defined screen rectangle to a user           *
;*              defined color.                                          *
;*  entry:      ax has the start x in pixels                            *
;*              bx has the start y in scan lines                        *
;*              cx has the stop x in pixels                             *
;*              dx has the stop y in scan lines                         *
;*              di is the color to clear the screen to.                 *
;*  exit:                                                               *
;*  registers:                                                          *
;*  stack usage:                                                        *
;*                                                                      *
;*                                                                      *
;*                                                                      *
;************************************************************************
set_rectangle           proc    near
;
;save the start/stop coordinates; then, check to see if the option is 
;currently occupied before making any changes to its current state.
;this example is not checking for valid entry values.  ax must be less
;than cx.  bx must be less than dx.
;
        mov     word ptr xstart,ax
        mov     word ptr ystart,bx
        mov     word ptr xstop,cx
        mov     word ptr ystop,dx
        call    fifo_empty    ;wait for an unoccupied graphics option.
;
;assert the new screen color to both sides of the foreground/background
;register.  put the option into replace mode with all planes enabled.
;put the option into write-enabled word mode.
;
        mov     bx,di           ;di passes the color. only lowest nibble valid.
        mov     bh,bl           ;combine the color number into both fg and bg.
        mov     cl,4            ;shift the color up to the upper nibble.
        shl     bh,cl
        or      bl,bh           ;combine the upper nibble with old lower.
        call    fgbg            ;issue to fgbg register.
        xor     bl,bl                   ;assert replace mode, all planes.
        call    alups
        and     byte ptr gbmod,0fdh     ;put into word mode.
        or      byte ptr gbmod,10h      ;put into write-enable mode.
        mov     al,0bfh
        out     53h,al
        mov     al,byte ptr gbmod
        out     51h,al
;
;do the rectangle write.
;
;write one column at a time.  since the GDC is a word device, we have to
;take into account that our write window may start on an odd pixel not
;necessarily on a word boundary.  the graphics options's write mask must be
;set accordingly.
;
;do a write buffer write to the entire rectangle defined by the start/stop
;values.  calculate the first curl0.  calculate the number of scans per 
;column to be written.
;
        mov     ax,word ptr xstart      ;turn pixel address into word address.
        mov     cl,4
        shr     ax,cl
        mov     dx,word ptr ystart      ;turn scan start into words per line*y.
        test    byte ptr gbmod,1        ;high resolution?
        jnz     set1                    ;jump if yes.
        mov     cl,5                    ;medium resolution = 32 words per line.
        jmp     set2
set1:   mov     cl,6                    ;high resolution = 64 words per line.
set2:   shl     dx,cl
        add     dx,ax                   ;combine x and y word addresses.
        mov     word ptr curl0,dx       ;first curl0.
        mov     ax,word ptr ystop       ;sub start from stop.
        sub     ax,word ptr ystart
        mov     word ptr nmritl,ax
;
;program the text mask. 
;
;there are four possible write conditions: 
;
;a)partially write disabled to theleft 
;b)completely write enabled 
;c)partially write disabled to the right 
;d)partially write disabled to both left and right
;
;the portion to be write disabled to the left will be the current xstart 
;pixel information.  as we write a column, we update the current xstart 
;location.  only the first xstart will have a left hand portion write 
;disabled.  only the last will have a right hand portion disabled.  if the 
;first is also the last, a portion of both sides will be disabled.
;
cls1:   mov     bx,0ffffh       ;calculate the current write mask.
        mov     cx,word ptr xstart
        and     cx,0fh          ;eliminate all but pixel information.
        shr     bx,cl           ;shift in a 0 for each left  pixel to disable.
;
;write buffer write is done by columns.  take the current xstart and use it 
;as the column to be written to.  when the word address of xstart is greater 
;than the word address xstop, we are finished.  there is a case where the 
;current word address of xstop is equal to the current word address of xstart. 
;in that case, we have to be concerned about write disabling the bits to the 
;right.  when xstop becomes less than xstart, we are done.
;
        mov     ax,word ptr xstart      ;test to see if word xstop is equal
        and     ax,0fff0h               ;to word xstart.
        mov     cx,word ptr xstop
        and     cx,0fff0h
        cmp     ax,cx                   ;below?
        jb      cls3                    ;jump if yes.
        je      cls2                    ;jump if equal. do last write.
        jmp     exit                    ;all done. exit.
;
;we need to set up the right hand write disable. this is also the last write.
;bx has the left hand write enable mask in it. preserve and combine with the
;right hand mask which will be (f-stop pixel address) bits on the right.
;
cls2:   mov     cx,word ptr xstop       ;strip pixel info out of xstop.
        and     cx,0fh
        inc     cx                      ;make endpoint inclusive of write.
        mov     ax,0ffffh               ;shift the disable mask.
        shr     ax,cl                   ;wherever there is a one, we want to
        xor     ax,0ffffh               ;enable writes.
        and     bx,ax                   ;combine right and left masks.
;
;bx currently has the mask bytes in it. where we have a one we want to make a
;zero so that that particular bit will be write enabled.
;
cls3:   xor     bx,0ffffh       ;invert so where there is a 1 we write disable.
;
;assert the new text mask. make sure that the GDC is not busy before we change
;the mask.
;
cls4:   call    fifo_empty              ;make sure that the GDC isn't busy.
        mov     al,bh                   ;assert the upper write mask.
        out     cmaskh,al
        mov     al,bl                   ;assert the lower write mask.
        out     cmaskl,al
;
;position the GDC at the top of the column to be written.  this address was
;calculated earlier and the word need only be fetched and applied. the number
;of scans to be written has already been calculated.
;
        mov     al,curs                 ;assert the GDC cursor address.
        out     57h,al
        mov     ax,word ptr curl0       ;assert the word address low byte.
        out     56h,al
        mov     al,dh                   ;assert the word address high byte.
        out     56h,al
;
;start the write operation. write mask, alups, gbmod and fgbg are set up. 
;GDC is positioned.
;
        mov     al,figs         ;assert figs to GDC.
        out     57h,al
        xor     al,al           ;direction is down.
        out     56h,al
        mov     ax,word ptr nmritl
        out     56h,al          ;assert number of write operations to perform.
        mov     al,ah
        out     56h,al
        mov     al,22h          ;assert write data command.
        out     57h,al
        mov     al,0ffh
        out     56h,al
        out     56h,al
;
;update the xstart coordinate for the start of the next column write.
;strip off the pixel information and then add 16 pixels to it to get the next
;word address.
;
        and     word ptr xstart,0fff0h  ;strip off pixel info.
        add     word ptr xstart,16      ;address the next word.
        inc     word ptr curl0
        jmp     cls1                    ;check for another column to clear.
exit:   ret
set_rectangle   endp
cseg            ends
                end


;*****************************************************************************
;                                                                            *
;        p r o c e d u r e    p a t t e r n _ r e g i s t e r                *
;                                                                            *
;        purpose:        Load the Pattern Register                           *
;                                                                            *
;        entry:          bl = basic bit pattern data                         *
;                                                                            *
;        caution:        You must load the Pattern Multiplier before         *
;                        loading the Pattern Register                        *
;                                                                            *
;*****************************************************************************
;
;The following are some register values and the corresponding output patterns 
;when the repeat factor is a one:
;
;          Value          Pattern
;          -----          -------
;           0FFh          11111111
;           0AAh          10101010
;           0F0h          11110000
;           0CDh          11001101
;
;The following are the same register values and the corresponding output
;patterns when the repeat factor is a three:
;
;          Value          Pattern
;          -----          -------
;           0FFh          111111111111111111111111
;           0AAh          111000111000111000111000
;           0F0h          111111111111000000000000
;           0CDh          111111000000111111000111
;
cseg     segment byte    public  'codesg'
         extrn   fifo_empty:near
         public  pattern_register
         assume  cs:cseg,ds:nothing,es:nothing,ss:nothing
pattern_register proc    near
         call    fifo_empty
         mov     al,0fbh         ;select the Pattern Register
         out     53h,al
         mov     al,bl           ;set up the pattern data
         out     51h,al          ;load the Pattern Register
         ret
pattern_register endp
cseg     ends
         end

;*****************************************************************************
;                                                                            *
;        p r o c e d u r e    p a t t e r n _ m u l t                        *
;                                                                            *
;        purpose:        Load the Pattern Multiplier                         *
;                                                                            *
;        entry:          bl = basic bit pattern repeat factor (1 - 16)       *
;                                                                            *
;        caution:        You must load the Pattern Multiplier before         *
;                        loading the Pattern Register                        *
;                                                                            *
;*****************************************************************************
;
cseg     segment byte    public  'codesg'
         extrn   fifo_empty:near
         public  pattern_mult
         assume  cs:cseg,ds:nothing,es:nothing,ss:nothing
pattern_mult     proc    near
         call    fifo_empty
         dec     bl              ;adjust bl to be zero-relative
         not     bl              ;invert it (remember Pattern Register is
                                 ;multiplied by 16 minus multiplier value)
         mov     al,0fdh         ;select the Pattern Multiplier
         out     53h,al
         mov     al,bl           ;load the Pattern Multiplier 
         out     51h,al
         ret             
pattern_mult     endp
cseg     ends
         end
		 
;*****************************************************************************
;                                                                            *
;       p r o c e d u r e    p i x e l                                       *
;                                                                            *
;       purpose:        Draw a pixel                                         *
;                                                                            *
;       entry:          xinit = x location                                   *
;                       yinit = y location                                   *
;                       valid x values  = 0-799 high resolution              *
;                                       = 0-383 medium resolution            *
;                       valid y values  = 0-239 medium or high resolution    *
;                                                                            *
;*****************************************************************************
;
;Do a vector draw of one pixel at location xinit,yinit.  Assume that the
;Graphics Option is already set up in terms of Mode Register, FG/BG, ALU/PS.
;
dseg    segment byte    public  'datasg'
extrn   gbmod:byte,curl0:byte,curl1:byte,curl2:byte,xinit:word,yinit:word
dseg    ends
cseg    segment byte    public  'codesg'
        public  pixel
        assume  cs:cseg,ds:dseg,es:dseg,ss:nothing
pixel   proc    near
;
;Convert the starting x,y coordinate pair into a cursor position word value.
;
        mov     al,gbmod        ;are we in medium resolution mode?
        test    al,01
        jz      pv1             ;jump if yes
        mov     cl,06           ;use 64 words per line as a divisor
        jmp     pv2
pv1:    mov     cl,05           ;use 32 words per line as a divisor
pv2:    xor     dx,dx           ;set up for 32bit/16bit math by clearing 
        mov     ax,yinit        ;upper 16 bits
        shl     ax,cl
        mov     bx,ax           ;save lines*words per line
        mov     ax,xinit        ;compute the number of extra words on last line
        mov     cx,16           ;16 bits per word
        div     cx              ;ax now has number of extra words to add in
        add     ax,bx           ;dx has the less than 16 dot address left over
        mov     curl0,al        ;this results in the new cursor memory address
        mov     curl1,ah
        mov     cl,04           ;dot address is high nibble of byte 
        shl     dl,cl           
        mov     curl2,dl
;
;Position the cursor.
;
        mov     al,49h                  ;send out the cursor command byte.
        out     57h,al
        mov     ax,word ptr curl0       ;assert cursor location low byte.
        out     56h,al
        mov     al,ah                   ;assert cursor location high byte.
        out     56h,al
        mov     al,byte ptr curl2       ;assert cursor pixel location.
        out     56h,al
;
;Assert the figs command to draw one pixel's worth of vector.
;
        mov     al,4ch          ;assert the FIGS command
        out     57h,al
        mov     al,02h          ;line drawn to the right.
        out     56h,al
        mov     al,6ch          ;tell the GDC to draw the pixel when ready.
        out     57h,al
        ret
pixel   endp
cseg    ends
        end

;******************************************************************************
;                                                                             *
;        p r o c e d u r e    v e c t o r                                     *
;                                                                             *
;        purpose:        Draw a vector                                        *
;                                                                             *
;        entry:          xinit = starting x location                          *
;                        yinit = starting y location                          *
;                        xfinal= ending x location                            *
;                        yfinal= ending y location                            *
;                        valid x values = 0 - 799 high resolution             *
;                                         0 - 383 medium resolution           *
;                        valid y values = 0 - 239 high or medium resolution   *
;        exit:                                                                *
;                                                                             *
;******************************************************************************
;
;Assume start and stop co-ordinates to be in registers and
;all other incidental requirements already taken care of.  This code positions
;the cursor, computes the FIGS parameters DIR, DC, D, D2, and D1, and then
;implements the FIGS and FIGD commands.
;What is not shown here, is that the Mode Register is set up for vector
;operations, the write mode and planes select is set up in the ALU/PS Register,
;the FGBG Register is set up with foreground and background colors, and the
;Pattern Multiplier/Register are loaded. In vector mode all incoming data
;is from the Pattern Register. We have to make sure that the GDC's pram 8 and
;9 are all ones so that it will try to write all ones to the bitmap. The
;external hardware will get in there and put the Pattern Register's data
;into the bitmap.
;
;This same basic setup can be used for area fills, arcs and such.
;
extrn    fifo_empty:near,gbmod:byte,p1:byte
cseg     segment byte    public  'codesg'
         public  vector
         assume  cs:cseg,ds:dseg,es:dseg,ss:nothing
vector   proc    near
         call    fifo_empty
         mov     al,78h
         out     57h,al          ;set pram bytes 8 and 9
         mov     al,0ffh
         out     56h,al
         out     56h,al
;
;Convert the starting x,y coordinate pair into a cursor position word value.
;
         mov     al,gbmod        ;are we in low resolution mode?
         test    al,01
         jz      v11             ;jump if yes
         mov     cl,06           ;use 64 words per line as a divisor
         jmp     v2
v11:     mov     cl,05           ;use 32 words per line as a divisor
v2:      xor     dx,dx           ;set up for 32bit/16bit math by clearing
         mov     ax,yinit        ;upper 16 bits
         shl     ax,cl
         mov     bx,ax           ;save lines*words per line
         mov     ax,xinit        ;compute the no. of extra words on last line
         mov     cx,16           ;16 bits per word
         div     cx              ;ax now has number of extra words to add in
         add     ax,bx           ;dx has the less than 16 dot address left over
         mov     curl0,al        ;this results in the new cursor memory address
         mov     curl1,ah
         mov     cl,04           ;dot address is high nibble of byte 
         shl     dl,cl           ;
         mov     curl2,dl
         mov     al,49h          ;set cursor location to that in curl0,1,2
         out     57h,al          ;issue the GDC cursor location command
         mov     al,curl0        ;fetch word low address
         out     56h,al
         mov     al,curl1        ;word middle address
         out     56h,al
         mov     al,curl2        ;dot address (top 4 bits) and high word addr
         out     56h,al
;
;Draw a vector. 
;
        mov     ax,word ptr xinit       ;is this a single point draw?
        cmp     word ptr xfinal,ax      ;if yes then start=stop coordinates.
        jnz     v1                      ;jump if definitely not.
        mov     ax,word ptr yinit       ;maybe. check y coordinates.
        cmp     word ptr yfinal,ax
        jnz     v1                      ;jump if definitely not.
        mov     al,04ch                 ;program a single pixel write 
        out     57h,al                  ;operation
        mov     al,2                    ;direction is to the right..
        out     56h,al
        mov     al,06ch
        out     57h,al
        ret
v1:     mov     bx,yfinal       ;compute delta y
        sub     bx,yinit        ;delta y negative now?
        jns     quad34          ;jump if not (must be either quad 3 or 4)
quad12: neg     bx              ;delta y is negative, make absolute
        mov     ax,xfinal       ;compute delta x
        sub     ax,xinit        ;delta x negative?
        js      quad2           ;jump if yes
quad1:  cmp     ax,bx           ;octant 2?
        jbe     oct3            ;jump if not
oct2:   mov     p1,02           ;direction of write
        jmp     vxind   ;abs(deltax)>abs(deltay), independent axis=x-axis
oct3:   mov     p1,03           ;direction of write
        jmp     vyind   ;abs(deltax)=<abs(deltay), independent axis=y-axis
quad2:  neg     ax              ;delta x is negative, make absolute
        cmp     ax,bx           ;octant 4?
        jae     oct5            ;jump if not
oct4:   mov     p1,04           ;direction of write
        jmp     vyind   ;abs(deltax)=<abs(deltay), independent axis=y-axis
oct5:   mov     p1,05           ;direction of write
        jmp     vxind   ;abs(deltax)>abs(deltay), independent axis=x-axis
quad34: mov     ax,xfinal       ;compute delta x
        sub     ax,xinit
        jns     quad4           ;jump if delta x is positive
quad3:  neg     ax              ;make delta x absolute instead of negative
        cmp     ax,bx           ;octant 6?
        jbe     oct7            ;jump if not
oct6:   mov     p1,06           ;direction of write
        jmp     vxind   ;abs(deltax)>abs(deltay), independent axis=x-axis
oct7:   mov     p1,07           ;direction of write
        jmp     vyind   ;abs(deltax)<=abs(deltay), independent axis=y-axis
quad4:  cmp     ax,bx           ;octant 0?
        jae     oct1            ;jump if not
oct0:   mov     p1,0            ;direction of write
        jmp     vyind   ;abs(deltax)<abs(deltay), independent axis=y-axis
oct1:   mov     p1,01           ;direction of write
        jmp     vxind   ;abs(deltax)=>(deltay), independent axis=x-axis
;
vyind:  xchg    ax,bx           ;put independent axis in ax, dependent in bx
vxind:  and     ax,03fffh       ;limit to 14 bits
        mov     dc,ax           ;DC=abs(delta x)-1
        push    bx              ;save abs(delta y)
        shl     bx,01           ;multiply delta y by two
        sub     bx,ax
        and     bx,03fffh       ;limit to 14 bits
        mov     d,bx            ;D=2*abs(delta y)-abs(delta x)
        pop     bx              ;restore (abs(delta y)
        push    bx              ;save abs(delta y)
        sub     bx,ax
        shl     bx,1
        and     bx,03fffh       ;limit to 14 bits
        mov     d2,bx           ;D2=2*(abs(delta y)-abs(delta x))
        pop     bx
        shl     bx,1
        dec     bx
        and     bx,03fffh       ;limit to 14 bits
        mov     d1,bx           ;D1=2*abs(delta y)-1
vdo:    mov     al,04ch         ;issue the FIGS command
        out     57h,al
        mov     al,08           ;construct P1 of FIGS command
        or      al,byte ptr p1
        out     56h,al          ;issue a parameter byte
        mov     si,offset dc
        mov     cx,08           ;issue the 8 bytes of DC,D,D2,D1
vdo1:   mov     al,[si]         ;fetch byte
        out     56h,al          ;issue to the GDC
        inc     si              ;point to next in list
        loop    vdo1            ;loop until all 8 done
        mov     al,06ch         ;start the drawing process in motion
        out     57h,al          ;by issuing FIGD
        ret
vector  endp
cseg    ends
dseg    segment byte    public  'datasg'
        public  curl0,curl1,curl2,dc,d,d2,d1,dm,dir,xinit,yinit
        public  xfinal,yfinal
curl0   db      0
curl1   db      0
curl2   db      0
dc      dw      0
d       dw      0
d2      dw      0
d1      dw      0
dm      dw      0
dir     dw      0
xinit   dw      0
yinit   dw      0
xfinal  dw      0
yfinal  dw      0
dseg    ends
        end


;*****************************************************************************
;                                                                            *
;       p r o c e d u r e    c i r c l e                                     *
;                                                                            *
;       purpose:        Draw a circle in medium resolution mode              *
;                                                                            *
;       entry:          xinit = circle center x coordinate (0-799)           *
;                       yinit = circle center y coordinate (0-239)           *
;                       radius = radius of the circle in pixels              *
;                                                                            *
;       caution:        This routine will only work in medium resolution     *
;                       mode. Due to the aspect ratio of high resolution     *
;                       mode, circles appear as ellipses.                    *
;                                                                            *
;******************************************************************************
;
;Draw an circle. 
;
;This code positions the cursor and computes the FIGS parameters DIR, DC,
;D, D2, and D1. It then implements the actual FIGS and FIGD commands.
;What you don't see here is that the Mode Register is set up for vector
;operations, the write mode and planes select are set up in the ALU/PS,
;the FGBG Register is loaded with foreground and background colors and the
;Pattern Multiplier/Register are loaded. In vector mode, all incoming data
;is from the Pattern Register. We have to make sure that the GDC's pram 8 and
;9 are all ones so that it will try to write all ones to the bitmap. The
;external hardware will get in there and put the Pattern Register's data
;into the bitmap.
;
extrn   gbmod:byte,curl0:byte,curl1:byte,curl2:byte,xinit:word,yinit:word
extrn   fifo_empty:near,dc:word,d:word,d2:word,d1:word,dm:word,dir:word
dseg    segment byte    public  'datasg'
        public  radius,xad,yad
xad     dw      0
yad     dw      0
radius  dw      0
dseg    ends
cseg    segment byte    public  'codesg'
        public  circle
        assume  cs:cseg,ds:dseg,es:dseg,ss:nothing
circle  proc    near
        call    fifo_empty
        mov     al,78h
        out     57h,al                  ;set pram bytes 8 and 9
        mov     al,0ffh
        out     56h,al
        out     56h,al
        mov     word ptr d1,-1          ;set FIGS D1 parameter
        mov     word ptr dm,0           ;set FIGS D2 parameter
        mov     bx,word ptr radius      ;get radius
        mov     ax,0b505h               ;get 1/1.41
        inc     bx
        mul     bx
        mov     word ptr dc,dx          ;set FIGS DC parameter
        dec     bx
        mov     word ptr d,bx           ;set FIGS D parameter
        shl     bx,1
        mov     word ptr d2,bx          ;set FIGS D2 parameter
        mov     ax,word ptr xinit       ;get center x
        mov     word ptr xad,ax         ;save it
        mov     ax,word ptr yinit       ;get center y
        sub     ax,word ptr radius      ;subtract radius
        mov     word ptr yad,ax         ;save it
        call    acvt                    ;position cursor
        mov     byte ptr dir,01h        ;arc 1
        call    avdo                    ;draw it
        call    acvt                    ;position cursor
        mov     byte ptr dir,06h        ;arc 6
        call    avdo                    ;draw it
        mov     ax,word ptr xinit       ;get center x
        mov     word ptr xad,ax         ;save it
        mov     ax,word ptr yinit       ;get center y
        add     ax,word ptr radius      ;add in radius
        mov     word ptr yad,ax         ;save it
        call    acvt                    ;position cursor
        mov     byte ptr dir,02h        ;arc 2
        call    avdo                    ;draw it
        call    acvt                    ;position cursor
        mov     byte ptr dir,05h        ;arc 5
        call    avdo                    ;draw it
        mov     ax,word ptr xinit       ;get center x
        sub     ax,word ptr radius      ;subtract radius
        mov     word ptr xad,ax         ;save it
        mov     ax,word ptr yinit       ;get center y
        mov     word ptr yad,ax         ;save it
        call    acvt                    ;position cursor
        mov     byte ptr dir,03h        ;arc 3
        call    avdo                    ;draw it
        call    acvt                    ;position cursor
        mov     byte ptr dir,00h        ;arc 0
        call    avdo                    ;draw it
        mov     ax,word ptr xinit       ;get center x
        add     ax,word ptr radius      ;add in the radius
        mov     word ptr xad,ax         ;save it
        mov     ax,word ptr yinit       ;get center y
        mov     word ptr yad, ax        ;save it
        call    acvt                    ;position cursor
        mov     byte ptr dir,07h        ;arc 7
        call    avdo                    ;draw it
        call    acvt                    ;position cursor
        mov     byte ptr dir,04h        ;arc 4
        call    avdo                    ;draw it
        ret
;
;Convert the starting x,y coordinate pair into a cursor position word value.
;
acvt:
        mov     al,gbmod        ;are we in low resolution mode?
        test    al,01
        jz      av1             ;jump if yes
        mov     cl,06           ;use 64 words per line as a divisor
        jmp     av2
av1:    mov     cl,05           ;use 32 words per line as a divisor
av2:    xor     dx,dx           ;set up for 32bit/16bit math by 
        mov     ax,word ptr yad    ;clearing upper 16 bits
        shl     ax,cl
        mov     bx,ax           ;save lines*words per line
        mov     ax,word ptr xad    ;compute no. of extra words on last line
        mov     cx,16           ;16 bits per word
        div     cx              ;ax now has number of extra words to add in
        add     ax,bx           ;dx has the less than 16 dot address left over
        mov     curl0,al        ;this results in the new cursor memory address
        mov     curl1,ah
        mov     cl,04           ;dot address is high nibble of byte 
        shl     dl,cl           ;
        mov     curl2,dl
        mov     al,49h          ;set cursor location to that in curl0,1,2
        out     57h,al          ;issue the GDC cursor location command
        mov     al,curl0        ;fetch word low address
        out     56h,al
        mov     al,curl1        ;word middle address
        out     56h,al
        mov     al,curl2        ;dot address (top 4 bits) and high word addr
        out     56h,al
        ret
avdo:   call    fifo_empty
        mov     al,4ch          ;issue the FIGS command
        out     57h,al
        mov     al,020h         ;construct P1 of FIGS command
        or      al,byte ptr dir
        out     56h,al          ;issue a parameter byte
        mov     si,offset dc
        mov     cx,10           ;issue the 10 bytes of DC,D,D2,D1
avdo1:  mov     al,[si]         ;fetch byte
        out     56h,al          ;issue to the GDC
        inc     si              ;point to next in list
        loop    avdo1           ;loop until all 10 done
        mov     al,6ch          ;start the drawing process in motion
        out     57h,al          ;by issuing FIGD command
        ret
circle  endp
cseg    ends
        end


