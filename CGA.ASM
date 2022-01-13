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
VERSION:            equ 0x0001
	use16
    cpu 	8086
    org 	0x100
;-----------------------------------------------------------
	jmp 	Main
;-----------------------------------------------------------
; TSR part (will be copied over to the beginning of CS)
;-----------------------------------------------------------
PART_TSR:
INT10:			; Entry point for Int10

;---------------------
; INT 10 declarations
;---------------------
.F_CGA_MODESWITCH	equ 0x00
.F_CGACOM:          equ 0xff
.F_CGACOM_DETECT:   equ 0x00
.F_CGACOM_DISPSEG:	equ 0x01
.Magic:             equ 'C'<<8|'G'
;---------------------
; Actual INT 10:
;---------------------
	cmp		ah,.F_CGA_MODESWITCH
	jz		.F_mode_switch
	cmp		ah,.F_CGACOM
	jz		.F_cga_com_functions	; ah = 'C'
	
	; for non-handled function, jump to original int handler
	db 0xEA		; JMP FAR
.original:
	dw 0x0000,0x0000

.F_mode_switch:
	cmp		al,0x06					; 640x200 B/W graphics (CGA,EGA,MCGA,VGA)
	jz		.Init640x200
	cmp		al,02					; 80x25 16 shades of gray text (CGA,EGA,MCGA,VGA)
	jz 		.InitChar80
	cmp		al,03					; 80x25 16 color text (CGA,EGA,MCGA,VGA)
	jz		.InitChar80
	stc
	iret

.F_cga_com_functions:
	cmp		al,.F_CGACOM_DISPSEG
	jb		.F_detection_function	; al = 00
	jz		.F_get_siplay_segment	; al = 01
	stc
	iret

	; Returns AX=currently used display segment
.F_get_siplay_segment:
	mov		ax,[SETUP.DisplaySeg - PART_TSR]
	iret
	
	; Returns AX=magic, BX=version, ES=TSR segment
.F_detection_function:
	mov		bx,VERSION
	mov		ax,cs
	mov		es,ax
	mov		ax,.Magic
	iret

.Init640x200:
	call	InitCGA640x200x2		; Defined in CGA640S.asm
	iret

.InitChar80:
	call	InitChar80				; Defined in CGA640S.asm
	iret

SETUP:
.DisplaySeg:
	dw		0xb800					; This defaults to B800

%include "GDC.INC"
%include "CGA640S.ASM"

;-----------------------------------------------------------
; This is the TSR installer code
;-----------------------------------------------------------
PART_INSTALLER:
	; Copy TSR to the beginning of CS segment + 80h
	; We don't need command line parameters anymore
	cld								; Set increment direction
	mov		ax,cs
	mov		ds,ax
	mov		es,ax
	mov		si,PART_TSR
	mov     di,0x80
	mov		cx,(PART_INSTALLER - PART_TSR)/2+1
	rep movsw						; Copy TSR to segment start
	
	; Set DS to point to TSR segment
	mov     ax,cs
	add     ax,0x08
	mov     ds,ax
	
	; Get old interrupt vector
	mov     ax,0x3510
	int     0x21
	mov     [INT10.original - PART_TSR],bx
	mov     [INT10.original - PART_TSR+2],es
	
	; Set new interrupt vector
	mov     dx,INT10 - PART_TSR		; Note: DS=TSR segment!
	mov     ax,0x2510
	int     0x21					; DOS Set interrupt
	
	; Calculate size of TSR memory needed,
	; and exit remaining resident
	mov		dx,Strings.M_Activated
	call    Print
	
	; If TSR is configured in test mode,
	; display mem comes right after the TSR
	cmp 	[SETUP.DisplaySeg - PART_TSR],0xb800
	jz		.StandardMemory

	; The memory size = TSR + 16k display ram
	mov		dx,(PART_INSTALLER - PART_TSR) / 0x10 + 8 + 1 + 0x400
	jmp 	.Exit
.StandardMemory:
	; The memory size in pragraphs to reserve
	mov		dx,(PART_INSTALLER - PART_TSR) / 0x10 + 8 + 1
.Exit
	mov		ax,0x3100
	int		0x21

;-----------------------------------------------------------
; Utility methods
;-----------------------------------------------------------
Print:		; CS:DX points to '$' terminated string
	push	cs
	pop		ds
	mov 	ah,9
	int		0x21
	ret

;-----------------------------------------------------------
UpCase:		; Converts string @ CS:SI, CX length
    push 	di
	push 	si
	push	cx
	mov		di,si
	mov		ax,cs
	mov		ds,ax
	mov		es,ax
.loop:
    lodsb
    cmp 	al,'a'
    jb 		.notSmall
    cmp 	al,'z'
    ja 		.notSmall
    sub 	al,'a'-'A'
.notSmall:
    stosb
    loop 	.loop
    pop 	cx
	pop 	si
	pop		di
    ret

;-----------------------------------------------------------
; In the command line argument find the first ' ' (space) char
; OUT:
; 	- DS:SI = remaining string
;	- CX = remaining length
FindFirstSpace:
	mov		ax,cs
	mov		ds,ax
	mov		es,ax
    mov 	di,0x80			; ES:DI (CS:0x0080) = command line params
    xor 	cx,cx
    es mov 	cl,[di]			; Get parameter line length
    or 		cx,cx
    jz 		.ExitFFS		; If empty, we are done
    inc 	di				; Position to first char
    mov 	al,' '
    repz scasb				; Find space
    sub 	di,2			; step back ??Why 2??
    mov 	si,di
.ExitFFS:
    ret

;-----------------------------------------------------------
; Search parameter in command line arguments string
; IN:
;	- DS:SI: command line arguments
;	- ES:DI: parameter definition (p)
;	- CX: command line length (a.length)
; OUT:
;	- C = 0 if parameter found, 1 if not found
FindParam:	; Searches for first commandline parameter
    push 	cx
	push	di
    xor 	ax,ax           
    lodsb                   ; AX = [ES:DI] (p.length)
    dec 	ax				; length--
    cmp 	cx,ax			; If CX < p.length
    jb 		.ParamNotFound  ; then parameter not found
    
	dec 	ax		;??
    sub 	cx,ax           ; CX = a.length - p.length
    dec 	si				; DS:SI needs to point to p.length

.loop:
    push 	cx				; Save max restart counter
	push 	si
	push 	di
    lodsb                   ; CX = p.length
    mov 	cl,al           ;
    repz 	cmpsb           ; Compare to AL till it matches
    pop 	di				; Reset ES:DI
	pop 	si				; Reset DS:SI
    or 		cx,cx
    pop 	cx				; Restore restart counter
    
	jz 		.ParamFound     ; If CX == 0, we found the parameter
    inc 	di              ; Start new search from next char pos
    loop 	.loop

.ParamNotFound:
    pop 	di
	pop 	cx
    stc						; Indicate not found
    ret

.ParamFound:
	pop 	di
	pop 	cx
    clc						; Indicate found
    ret
	
;-----------------------------------------------------------
DetectTsr:	; ZF if present
	mov 	ax,INT10.F_CGACOM<<8 + INT10.F_CGACOM_DETECT
	int		0x10
	cmp		ax,INT10.Magic
	ret

;-----------------------------------------------------------
; This is the TSR uninstaller code
; IN:
;	- ES: TSR segment register
DeactivateTsr:
	push 	es						; Save TSR segment
	; Get current int 10 interrupt vector
	mov     ax,0x3510
	int     0x21
	; Check if it is pointing to CGA.COM TSR
	mov 	dx,Strings.E_CantDeactivate
	cmp 	bx,INT10 - PART_TSR
	pop		bx						; Restore TSR segment
									; before we jump away
	jnz 	ExitWithErrMessage		; If not, error msg
	mov		ax,es
	cmp		ax,bx
	jnz		ExitWithErrMessage		; If not, error msg
	
	; Reset the old int 10
	es mov	ax,[INT10.original - PART_TSR+2]
	mov		ds,ax
	es mov	dx,[INT10.original - PART_TSR]
	mov		ax,0x2510				; DOS Set interrupt
	int		0x21
	
	; Free DOS memory of TSR
	mov		ax,es
	sub		ax,8
	mov		es,ax
	mov		ah,0x49					; DOS Free memory
	int		0x21
	mov		dx,Strings.E_CantDeactivate
	jc		ExitWithErrMessage		; If there was an error, show err msg
	ret

;-----------------------------------------------------------
; Main entry point of program
;-----------------------------------------------------------
Main:
	mov		dx,Strings.Greetings
	call 	Print
	call 	FindFirstSpace
    or 		cx,cx      		; If Strings.IsNullOrEmpty(parameters)
    jz 		TryInstall

	mov 	si,Strings.Param2
	call 	FindParam		; Find "TEST" cmd line param
	jc		TryInstallTest
    
	add 	cx,2
    call 	UpCase          ; Convert params to upper case
	
	mov 	si,Strings.Param1
	call 	FindParam		; Find "OFF" cmd line param
	jnc		TryUninstall
	
	mov		dx,Strings.Help

ExitWithErrMessage:
	mov		al,1			; Exit error code
ExitWithMessage:
	call	Print
	mov		ah,0x4c
	int		0x21

TryUninstall:
	call	DetectTsr			; Call detect function
	mov		dx,Strings.E_NotActive
	jnz		ExitWithErrMessage	; If not detected, error msg
	
	mov 	dx,Strings.E_IncorrectVersion
	cmp		bx,VERSION
	jnz		ExitWithErrMessage	; If incorrect version is detected, error msg
	
	call 	DeactivateTsr		; Deactivate
	
	call	DetectTsr			; Check if it is unloaded
	jz		ExitWithErrMessage	; If it's still there, error msg
	
	mov		dx,Strings.M_Deactivated
	jmp 	ExitWithMessage		; Else confirm with msg that it's deactivate

TryInstallTest:
	mov		ax,cs
	add		ax,(PART_INSTALLER - PART_TSR) >> 4 + 8 + 1
	mov		word [SETUP.DisplaySeg],ax
TryInstall:
	call	DetectTsr			; Check if it's active already
	mov		dx,Strings.E_AlreadyActive
	jz		ExitWithErrMessage	; If active, error msg
	
	; TODO: check DOS version
	; TODO: check Rainbow HW:
	;	- Memory size
	; 	- Graphics option
	; 	- Revision
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
	db "  TEST...................: Activate in test mode",10,13
	db "  OFF....................: Deactivate",10,13,10,13
	db "For additional info, visit https://github.com/cyborgyn/rbcga",10,13
	db '$'
.Param1:
	db (.Param2-.Param1-1)," OFF",0
.Param2:
	db (.Param3-.Param2-1)," TEST",0
.Param3:

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
