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
VERSION: 		equ 0x0001
IRQ10ADDRESS:	equ 0x10 * 4
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
.F_CGACOM:	equ 0xff
.F_CGACOM_DETECT:
			equ 0x00
.F_CGACOM_DEACT:
			equ 0x01
.Magic:
			equ 'C'<<8+'G'
;---------------------
; Actual INT 10:
;---------------------
	cmp		ah,.F_CGACOM
	jz		.F_cga_com_functions	; ah = ff
	
	; Non-handled function, call to original
	cs
	db 0xEA		; JMP FAR
.original:
	dw 0x0000,0x0000

.F_cga_com_functions:
	cmp		al,.F_CGACOM_DEACT
	jz 		.F_deactivate_function	; al = 01
	jb		.F_detection_function	; al = 00
	stc
	iret
	
.F_deactivate_function:
	; TODO: write unload
	; Check INT 10 pointing to us
	; Restore original int 10
	; Free memory
	iret
.F_detection_function:
	mov		bx,VERSION
	mov		ax,.Magic
	iret

;-----------------------------------------------------------
; This is the TSR installer code
;-----------------------------------------------------------
PART_INSTALLER:
	; Copy TSR to the beginning of CS segment
	; We don't need command line parameters anymore
	cld								; Set increment direction
	mov		ax,cs
	mov		ds,ax
	mov		es,ax
	mov		si,PART_TSR
	xor		di,di
	mov		cx,(PART_INSTALLER - PART_TSR)/2+1
	rep movsw						; Copy TSR to segment start
	
	; Copy over old interrupt vector
	xor		ax,ax
	mov		ds,ax					; ES=0000, int table
	mov		si,IRQ10ADDRESS
	mov		di,INT10.Original-PART_TSR
	movsw							; Copy original int 10 address
	movsw							; to TSR
	
	; Set new interrupt vector
	mov		ax,cs					; Set INT 10 to point to
	mov		[IRQ10ADDRESS],ax
	mov		ax,INT10-PART_TSR		; TSR's INT 10 handler
	mov		[IRQ10ADDRESS+2],ax
	
	; Calculate size of TSR memory needed,
	; and exit remaining resident
	mov		dx,(PART_INSTALLER-PART_TSR) / 0x10 + 1
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
FindFirstSpace:             ;Ez az eljrs az ES:DI cmen lv PASCAL
    push 	es              ;szer stringbl megkeresi az els SPACE
    pop 	ds              ;karaktert, majd erre a pozcira lltja
    xor 	cx,cx           ;DS:SI-t is. CX-ben a maradk string hossza lesz.
    mov 	di,0x80
    es mov 	cl,[di]
    or 		cx,cx
    jz 		.ExitFFS
    inc 	di
    mov 	al,' '
    repz scasb
    sub 	di,2
    mov 	si,di
.ExitFFS:
    ret

;-----------------------------------------------------------
FindParam:	; Searches for first commandline parameter
    push 	cx
	push	di              ;paramtert a stringben DS:SI=paramter,
    xor 	ax,ax           ;ES:DI=string
    lodsb                   ;AX:=paramterhossz-1
    dec 	ax
    cmp 	cx,ax			;Ha a string hossza<AX, akkor itt ez a
    jb 		.ParamNotFound  ;paramter biztos nem lesz
    dec 	ax
    sub 	cx,ax           ;Ismtls stringhossz-(AX-1)
    dec 	si
.loop:
    push 	cx
	push 	si
	push 	di
    xor 	cx,cx
    lodsb                   ;Megnzzk, hogy az aktulis helytl
    mov 	cl,al           ;nem kezddik-e egy paramterrel egyforma
    repz 	cmpsb           ;stringrszlet
    pop 	di
	pop 	si
    or 		cx,cx
    jz 		.ParamFound     ;Ha igen, megvan amit kerestnk...
    pop 	cx
    inc 	di              ;Ha nem, nveljk az aktulis pozcit,
    loop 	.loop           ;s innen is megnzzk.
.ParamNotFound:
    pop 	di
	pop 	cx
    stc						; Indicate error
    ret
.ParamFound:                ; No error
    pop 	cx
	pop 	di
	pop 	cx
    clc
    ret
	
;-----------------------------------------------------------
DetectTsr:	; ZF if present
	mov 	ax,INT10.F_CGACOM<<8 + INT10.F_CGACOM_DETECT
	int		0x10
	cmp		ax,INT10.Magic
	ret
;-----------------------------------------------------------
; Main entry point of program
;-----------------------------------------------------------
Main:
	mov		dx,Strings.Greetings
	call 	Print
	call 	FindFirstSpace
    or 		cx,cx      		; If String.IsNullOrEmpty(parameters)
    jz 		TryInstall
    
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
	
	mov		ax,INT10.F_CGACOM<<8 + INT10.F_CGACOM_DEACT
	int 	0x10				; Call deactivate
	mov 	dx,Strings.E_CantDeactivate
	or		ax,ax
	jnz		ExitWithErrMessage	; If couldn't, error msg
	
	call	DetectTsr			; Check if it is unloaded
	jz		ExitWithErrMessage	; If it's still there, error msg
	
	mov		dx,Strings.M_Deactivated
	jmp 	ExitWithMessage		; Else confirm with msg that it's deactivate
	
TryInstall:
	call	DetectTsr			; Check if it's active already
	mov		dx,Strings.E_AlreadyActive
	jz		ExitWithErrMessage	; If active, error msg
	
	; TODO: we need to check DOS version, Rainbow machine, etc...
	; TODO: do the actual install
	mov		al,0
	mov		dx,Strings.M_Activated
	jmp		ExitWithMessage


Strings:
.Greetings:
	db "CGA Emulator v",((VERSION >> 8)+'0'),".",((VERSION & 0xff)+'0')
	db " for DEC Rainbow 100, Copyright Cyborgyn 2022",10,13,'$'
.Help:
	db "  Unrecognised command line parameter. Usage: CGA.COM [command]",10,13
	db "  without parameters.....: Activate CGA.COM emulator",10,13
	db "  Possible commands:",10,13
	db "  OFF....................: Deactivate",10,13
	db '$'
.Param1:
	db (.Param2-.Param1-1)," OFF",0
.Param2:
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
