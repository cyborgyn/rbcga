;-----------------------------------------------------------
;            CGA Emulator for DEC Rainbow 100
;
; It's a TSR, that implements CGA BIOS calls, and emulates
; CGA framebuffer.
;
; Requirements:
; - DEC Rainbow 100B
; - Original 8088 CPU
; - 896 kByte RAM
; - MSDOS 3.10b
; - B800:0000-3FFF address in DOS is free
;
; Features v0.1:
; - CGA 640x200 1bpp hi resolution CGA mode
; - Periodic framebuffer synchronization
; - Switching back to built-in char screen
;-----------------------------------------------------------
VERSION:            equ 0x0002
    use16
    cpu     8086
    org     0x100
;-----------------------------------------------------------
    jmp     Main
;-----------------------------------------------------------
; TSR part (will be copied over to the beginning of CS)
;-----------------------------------------------------------
PART_TSR:
INT10:          ; Entry point for Int10

;---------------------
; INT 10 declarations
;---------------------
.F_CGA_MODESWITCH:  equ 0x00
.F_CGACOM:          equ 0xff
.F_CGACOM_DETECT:   equ 0x00
.F_CGACOM_DISPSEG:  equ 0x01
.Magic:             equ 'C'<<8|'G'
;---------------------
; Actual INT 10:
;---------------------
    cmp     ah,.F_CGA_MODESWITCH
    jz      .F_mode_switch
    cmp     ah,.F_CGACOM
    jz      .F_cga_com_functions    ; ah = 'C'

.Exit:
    ; for non-handled function, jump to original int handler
    db 0xEA     ; JMP FAR
.original:
    dw 0x0000,0x0000

.F_mode_switch:
    cmp     al,0x06                 ; 640x200 B/W graphics (CGA,EGA,MCGA,VGA)
    jz      Init640x200
    cmp     al,02                   ; 80x25 16 shades of gray text (CGA,EGA,MCGA,VGA)
    jz      InitChar80
    cmp     al,03                   ; 80x25 16 color text (CGA,EGA,MCGA,VGA)
    jz      InitChar80
    iret

.F_cga_com_functions:
    cmp     al,.F_CGACOM_DISPSEG
    jb      .F_detection_function   ; al = 00
    jz      .F_get_siplay_segment   ; al = 01
    ja      .F_allocate_memory      ; al = 02
    iret

    ; Returns AX=currently used display segment
.F_get_siplay_segment:
    cs mov      ax,[SETUP.DisplaySeg - PART_TSR]
    iret

    ; Returns AX=magic, BX=version, ES=TSR segment
.F_detection_function:
    mov     bx,VERSION
    mov     ax,cs
    mov     es,ax
    mov     ax,.Magic
    iret
.F_allocate_memory:
    call    Malloc_InstStore
    iret

Init640x200:
%ifndef DOSBOX
    call    InitCGA640x200x2        ; Defined in CGA640S.INC
%endif
    cs cmp  byte [SETUP.AutoPatcher - PART_TSR],0
    jz      .NoAutoPatcher
    call    InitAutoPatcher         ; If AutoPatcher loaded, initialize it
.NoAutoPatcher:                     ; Defined in AUTOPTCH.INC
%ifndef DOSBOX
    iret
%else
    jmp     INT10.Exit
%endif

InitChar80:
%ifndef DOSBOX
    call    InitCGAChar80x25         ; Defined in CGA640S.INC
%else
    jmp     INT10.Exit
%endif
    cs cmp  byte [SETUP.AutoPatcher - PART_TSR],0
    jz      .NoAutoPatcher
    call    DestroyAutoPatcher      ; If AutoPatcher loaded, deinitialize it
.NoAutoPatcher:                     ; Defined in AUTOPTCH.INC
%ifndef DOSBOX
    iret
%else
    jmp     INT10.Exit
%endif

INT_3_original:
    dw      0,0
SETUP:
.DisplaySeg:
    dw      0xb800                  ; This defaults to B800
.AutoPatcher:
    db      0                       ; If 0 AutoPatcher in inacive

%include "GDC.INC"
%include "CGA640S.INC"

REGSTORE:   ; SS,SP temp storage during IRQ
.SS:    dw      0
.SP:    dw      0
.DS:    dw      0
.ES:    dw      0

IrqStack:
    times 0x80 db 0
.Regs:  equ $
.Top:   equ $ - 0x20

PART_AUTOPATCHER:
    ; Align CGAfont to 0E
times   (0x0e - (($ - PART_TSR) % 16)) % 16   db  'x'
CGAfont:
%include "CGAFONT.INC"
%include "AUTOPTCH.INC"

;-----------------------------------------------------------
; This is the TSR installer code
;-----------------------------------------------------------
PART_INSTALLER:
    ; Copy TSR to the beginning of CS segment + 80h
    ; We don't need command line parameters anymore
    cld                             ; Set increment direction
    mov     ax,cs
    mov     ds,ax
    mov     es,ax
    mov     si,PART_TSR
    mov     di,0x80
    mov     cx,(PART_INSTALLER - PART_TSR)/2+1
    rep movsw                       ; Copy TSR to segment start

    ; Set DS to point to TSR segment
    mov     ax,cs
    add     ax,0x08
    mov     ds,ax

    ; Get old INT 10h vector
    mov     ax,0x3510
    int     0x21
    mov     [INT10.original - PART_TSR],bx
    mov     [INT10.original - PART_TSR+2],es

    ; Set new INT 10h vector
    mov     dx,INT10 - PART_TSR     ; Note: DS=TSR segment!
    mov     ax,0x2510
    int     0x21                    ; DOS Set interrupt
    
    ; Calculate memory reservation for TSR
    ; If TSR is configured in test mode,
    ; display mem comes right after the TSR
    cmp     word [SETUP.DisplaySeg - PART_TSR],0xb800
    jz      .StandardMemory

    ; The memory size = TSR + 16k display ram
    mov     dx,(PART_INSTALLER - PART_TSR) / 0x10 + 8 + 1 + 0x400
    jmp     .MemorySizeReady
.StandardMemory:
    ; The memory size in pragraphs to reserve
    mov     dx,(PART_INSTALLER - PART_TSR) / 0x10 + 8 + 1
.MemorySizeReady:
    push    dx
    
    ; Check if autopacther activation needed
    cmp     byte [SETUP.AutoPatcher - PART_TSR],0
    jz      .AutoPatcherNotActivated
    
    ; Get old INT 3 vector
    mov     ax,0x3503
    int     0x21
    mov     [INT_3_original - PART_TSR],bx
    mov     [INT_3_original - PART_TSR+2],es
    
    ; Set up new INT 3 vector
    mov     dx,INT_3 - PART_TSR     ; Note: DS=TSR segment!
    mov     ax,0x2503
    int     0x21
    
    ; Lower current memory reservation
    pop     bx                      ; Required memory size for TSR
    push    bx
    push    cs
    pop     es                      ; ES: segment, BX: new size
    mov     ah,0x4a
    int     0x21
    
    ; Allocate memory
    mov     ax,0xff02
    int     0x10
    
.AutoPatcherNotActivated:
    ; Exit remaining resident
    mov     dx,Strings.M_Activated
    push    ds
    call    Print
    pop     ds
    
    pop     dx                      ; Required memory size for TSR
    mov     ax,0x3100
    int     0x21

;-----------------------------------------------------------
; Utility methods
;-----------------------------------------------------------
Print:      ; CS:DX points to '$' terminated string
    push    cs
    pop     ds
    mov     ah,9
    int     0x21
    ret

;-----------------------------------------------------------
; Normalizes PASCAL string @ CS:SI
; Removes additional white spaces, convert to upper case
; Out:
;   - CX = string length
;   - ES:DI = points also to the string
NormalizeArgs:
    push    si
    mov     di,si
    mov     ax,cs
    mov     ds,ax
    mov     es,ax
    xor     cx,cx               ; Load string length
    mov     bx,si               ; String length address
    lodsb
    inc     al                  ; We also need the enter key
    mov     cl,al
    xor     al,al
    stosb                       ; SI & DI points to start char
                                ; String length [DS:BX] = 0

.loop:
    lodsb
    cmp     al,' '
    ja      .normalChar
    mov     al,' '              ; Normalize char to space
    stosb
    inc     byte [bx]
    dec     cx
    jz      .Exit
.loopWhiteSpace:
    lodsb                       ; Load next to check
    cmp     al,' '
    jnb     .normalChar         ; If it's already not a white space
    loop    .loopWhiteSpace     ; If it was, just discard
    jmp     .Exit
.normalChar:
    cmp     al,'a'
    jb      .notSmall
    cmp     al,'z'
    ja      .notSmall
    sub     al,'a'-'A'
.notSmall:
    stosb
    inc     byte [bx]
    loop    .loop
.Exit:
    mov     cl,byte [bx]
    pop     si
    mov     di,si
    ret

;-----------------------------------------------------------
; Search parameter in command line arguments string
; IN:
;   - CX: command line arguments length
;   - DS:SI: parameter definition (p)
;   - ES:DI: command line arguments
; OUT:
;   - C = 1 if parameter found, 0 if not found
FindParam:  ; Searches for first commandline parameter
    push    cx
    push    di
    xor     ax,ax
    lodsb                   ; AX = [DS:SI] (p.length)
    dec     si              ; reset SI to the p.length
    cmp     cx,ax           ; If CX < p.length
    jb      .ParamNotFound  ; then parameter not found

    sub     cx,ax           ; CX = a.length - p.length
    inc     cx              ; for the counter we need +1
    inc     di              ; ES:DI point to 1st char

.loop:
    push    cx              ; Save max restart counter
    push    si
    push    di
    lodsb                   ; CX = p.length
    mov     cl,al           ;
    repz    cmpsb           ; Compare to AL till it matches
    pop     di              ; Reset ES:DI
    pop     si              ; Reset DS:SI
    or      cx,cx
    pop     cx              ; Restore restart counter

    jz      .ParamFound     ; If CX == 0, we found the parameter
    inc     di              ; Start new search from next char pos
    loop    .loop

.ParamNotFound:
    pop     di
    pop     cx
    clc                     ; Indicate not found
    ret

.ParamFound:
    pop     di
    pop     cx
    stc                     ; Indicate found
    ret

;-----------------------------------------------------------
DetectTsr:  ; ZF if present
    mov     ax,INT10.F_CGACOM<<8 + INT10.F_CGACOM_DETECT
    int     0x10
    cmp     ax,INT10.Magic
    ret

;-----------------------------------------------------------
; This is the TSR uninstaller code
; IN:
;   - ES: TSR segment register
DeactivateTsr:
    push    es                      ; Save TSR segment
    ; Get current int 10 interrupt vector
    mov     ax,0x3510
    int     0x21
    ; Check if it is pointing to CGA.COM TSR
    mov     dx,Strings.E_CantDeactivate
    cmp     bx,INT10 - PART_TSR
    pop     bx                      ; Restore TSR segment
                                    ; before we jump away
    jnz     ExitWithErrMessage      ; If not, error msg
    mov     ax,es
    cmp     ax,bx
    jnz     ExitWithErrMessage      ; If not, error msg

    ; Reset the old INT 10 vector
    es lds  dx,[INT10.original - PART_TSR]
    mov     ax,0x2510               ; DOS Set interrupt
    int     0x21
    
    ; Check if autopacther was activated
    cmp     byte [SETUP.AutoPatcher - PART_TSR],0
    jz      .AutoPatcherNotActivated
    
    ; If it was, reset the old INT 3 vector
    es lds  dx,[INT_3_original - PART_TSR]
    mov     ax,0x2503               ; DOS Set interrupt
    int     0x21
.AutoPatcherNotActivated:
    
    ; Free DOS memory of TSR
    mov     ax,es
    sub     ax,8
    mov     es,ax
    mov     ah,0x49                 ; DOS Free memory
    int     0x21
    mov     dx,Strings.E_CantDeactivate
    jc      ExitWithErrMessage      ; If there was an error, show err msg
    ret

;-----------------------------------------------------------
; Main entry point of program
;-----------------------------------------------------------
Main:
    mov     dx,Strings.Greetings
    call    Print
    mov     si,0x80         ; DS:0080 command line arguments
    call    NormalizeArgs   ; Convert params to upper case

    cmp     cx,1            ; If Strings.IsNullOrEmpty(parameters)
    jbe     TryInstall
    
    mov     si,Strings.Param3
    call    FindParam
    jnc     .NoAutoPatcher
    
    mov     byte [SETUP.AutoPatcher],1
    mov     dx,Strings.M_AutoPatcher
    call    Print
    jmp     TryInstall
    
.NoAutoPatcher:    
    mov     si,Strings.Param2
    call    FindParam       ; Find "TEST" cmd line param
    jc      TryInstallTest

    mov     si,Strings.Param1
    call    FindParam       ; Find "OFF" cmd line param
    jc      TryUninstall

    mov     dx,Strings.Help

ExitWithErrMessage:
    mov     al,1            ; Exit error code
ExitWithMessage:
    call    Print
    mov     ah,0x4c
    int     0x21

TryUninstall:
    call    DetectTsr           ; Call detect function
    mov     dx,Strings.E_NotActive
    jnz     ExitWithErrMessage  ; If not detected, error msg

    mov     dx,Strings.E_IncorrectVersion
    cmp     bx,VERSION
    jnz     ExitWithErrMessage  ; If incorrect version is detected, error msg

    call    DeactivateTsr       ; Deactivate

    call    DetectTsr           ; Check if it is unloaded
    jz      ExitWithErrMessage  ; If it's still there, error msg

    mov     dx,Strings.M_Deactivated
    jmp     ExitWithMessage     ; Else confirm with msg that it's deactivate

TryInstallTest:
    mov     ax,cs
    add     ax,(PART_INSTALLER - PART_TSR) / 0x10 + 8 + 1
    mov     word [SETUP.DisplaySeg],ax
    mov     dx,Strings.M_TestMode
    call    Print
TryInstall:
    call    DetectTsr           ; Check if it's active already
    mov     dx,Strings.E_AlreadyActive
    jz      ExitWithErrMessage  ; If active, error msg

    ; TODO: check DOS version
    ; TODO: check Rainbow HW:
    ;   - Memory size
    ;   - Graphics option
    ;   - Revision
    ; TODO: check CodeBlue is running

    jmp     PART_INSTALLER

Strings:
.Greetings:
    db "CGA Emulator v",((VERSION >> 8)+'0'),".",((VERSION & 0xff)+'0')
    db " for DEC Rainbow 100, Copyright Cyborgyn 2022",10,13,'$'
.Help:
    db "  Unrecognised command line parameter.",10,13
    db "  Usage: CGA.COM [command]",10,13
    db "  without parameters.....: Activate CGA.COM emulator",10,13
    db "  Possible commands:",10,13
    db "  PATCH..................: Enable BIOS CharMap AutoPatcher",10,13
    db "  TEST...................: Activate in test mode",10,13
    db "  OFF....................: Deactivate",10,13,10,13
    db "For additional info, visit https://github.com/cyborgyn/rbcga",10,13
    db '$'
.Param1:
    db (.Param2-$-1)," OFF "
.Param2:
    db (.Param3-$-1)," TEST "
.Param3:
    db (.Param4-$-1)," PATCH "
.Param4:

.M_AutoPatcher:
    db "  AutoPatcher enabled.",10,13,'$'
.M_TestMode:
    db "  CGA.COM in test mode is not accessible to ordinary applications.",10,13,'$'
.M_Deactivated:
    db "  CGA.COM deactivated.",10,13,'$'
.M_Activated:
    db "  CGA.COM activated.",10,13,'$'
.E_NotActive:
    db "  CGA.COM not active!",10,13,'$'
.E_AlreadyActive:
    db "  CGA.COM already activated!",10,13,'$'
.E_CantDeactivate:
    db "  Error deactivating CGA.COM!",10,13,'$'
.E_IncorrectVersion:
    db "  Incorrect version of CGA.COM is active!",10,13,'$'
