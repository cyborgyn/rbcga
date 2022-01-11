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
VERSION: equ 0x0001
	
	use16
    cpu 	8086
    org 	0x100
;-----------------------------------------------------------
	jmp 	Main

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
	mov 	ax,0xff00
	int		0x10
	cmp		ax,'C'<<8+'G'
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
	call	DetectTsr
	jz		.detected
	mov		dx,Strings.E_TsrNotPresent
	jmp 	ExitWithErrMessage
.detected:
	mov		ax,0xff01
	int 	0x10
	mov		dx,Strings.M_Removed
	or		ax,ax
	jz		ExitWithMessage
	mov 	dx,Strings.E_ErrorUninstalling
	jmp 	ExitWithErrMessage
	
TryInstall:
	call	DetectTsr
	mov		dx,Strings.E_TrsPresent
	jz		ExitWithErrMessage
	
	; TODO: we need to check DOS version, Rainbow machine, etc...
	; TODO: do the actual install
	mov		al,0
	mov		dx,Strings.M_Installed
	jmp		ExitWithMessage


Strings:
.Greetings:
	db "CGA Emulator v",((VERSION >> 8)+'0'),".",((VERSION & 0xff)+'0')," for DEC Rainbow 100, Copyright Cyborgyn 2022",10,13,'$'
.Help:
	db "  Unrecognised commandline parameter. Usage: CGA.COM [command]",10,13
	db "  without parameters.....: Install CGA.COM emulator in RAM",10,13
	db "  Possible commands:",10,13
	db "  OFF....................: Uninstall from RAM",10,13
	db '$'
.Param1:
	db (.Param2-.Param1-1)," OFF",0
.Param2:
.M_Removed:
	db "  CGA.COM deactivated.",10,13,'$'
.M_Installed:
	db "  CGA.COM activated.",10,13,'$'
.E_TsrNotPresent:
	db "  CGA.COM not active!",10,13,'$'
.E_TrsPresent:
	db "  CGA.COM already activated!",10,13,'$'
.E_ErrorUninstalling:
	db "  Error deactivating CGA.COM!",10,13,'$'
;-----------------------------------------------------------
; TSR part (will be copied over to the beginning of CS)
;-----------------------------------------------------------
PART_TSR:
INT10:
	cmp		ah,0xff
	jz		.F_cga_com_functions
	
	; Non-handled function, call to original
	cs
	db 0xEA		; JMP FAR
.origINT10:
	dw 0x0000,0x0000

.F_cga_com_functions:
	cmp		al,1
	jz 		.F_deactivate_function
	jb		.F_detection_function
	stc
	iret
	
.F_deactivate_function:
	; TODO: write unload
	iret
.F_detection_function:
	mov		bx,VERSION
	mov		ax,'C'<<8+'G'
	iret

;-----------------------------------------------------------
; This is the TSR installer code
;-----------------------------------------------------------
PART_INSTALLER:
	; TODO: do we need to adjust temporarily the SS&SP??
	mov		ax,cs
	mov		dx,ax
	mov		es,ax
	mov		si,PART_TSR
	xor		di,di
	mov		cx,(PART_INSTALLER - PART_TSR)/2+1
	rep movsw
	; TODO: exit with stay resident