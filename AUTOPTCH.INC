;----------------------------------------------------------------------------
; Functions for CGA emulator, BIOS Char map access AutoPatcher
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; Allocate memory for breakpoints
;-------------------------------
Malloc_InstStore:
    mov     bx,0x0800           ; 32k bytes in paragraphs, this is enought for 4096 break points
.retry:
    ; Save the InstStoreSize
    cs mov  word [InstStoreSize - PART_TSR],bx
    mov     ah,0x48             ; DOS reserve memory
    int     0x21
                                ; If there was an error, BX is the largest available block
    jc      .retry              ; Let's retry with that one
    ; AX= reserved memory segment address
    cs mov  word [InstStoreAddress - PART_TSR + 2],ax   ; Segment
    ; Don't use 0 for offset, as InstStoreTable entry 0 has special meaning
    cs mov  word [InstStoreAddress - PART_TSR + 0],8    ; Offset (let's start at number 1
    cs mov  ax,word [InstStoreSize - PART_TSR]
    mov     cl,4
    shl     ax,cl
    cs mov  word [InstStoreSize - PART_TSR],ax
    ret
    
;----------------------------------------------------------------------------
; InitAutoPatcher
; Initializes auto paching mechanism, by
; - Builds up breakpoint table
; - Finds MOV {DS,ES},r/m instructions in CS segment
; - Replaces MOVs with INT3 + n*NOP
; IN:
;   - Stack + 4: caller's CS:IP
;-------------------------------
InitAutoPatcher:
    push    bp
    mov     bp,sp
    add     bp,2                ; BP points to stack on function entry
    push    ax
    push    bx
    push    cx
    push    dx
    push    di
    push    ds
    push    es
    
    ; Check if InstStore already allocated (the segment part)
    cs cmp  word [InstStoreAddress - PART_TSR + 2],0    ; Segment
    jnz     .InstStore_allocated
    call    Malloc_InstStore
.InstStore_allocated:
    mov     ax,cs
    mov     ds,ax               ; DS = CS // needs the instruction length calculator
    
    mov     es,word [bp+(2+2)]  ; INT 10h caller's CS on stack
    ; Find E8 instructions on callers CS segment
    ; TODO: check less, if loaded program is smaller
    mov     cx,0xfffb           ; Check whole segment - 5 (not to roll around the segment edge)
    xor     di,di               ; Starting from the beginning + 1
    inc     di
    cld
.find_next_MOV:
    ; Top pointer of InstStore
    cs mov  ax,word [InstStoreAddress - PART_TSR]       ; Offset
    ; Compare to available
    cs cmp  ax,word [InstStoreSize - PART_TSR]
    jnb     Instrumented        ; If no more room for breakpoints, exit
    
    mov     al,0x8e             ; MOV sreg,xxx first byte
    repne scasb                 ; ~1M cycles = 0.2s for 64k
    jcxz    Instrumented
    
    ; sreg3: ES=000, DS=011
    ; reg  : 000-111=AX,CX,DX,BX,SP,BP,SI,DI
    ; mod  : 00=disp0, 01=disp8, 10=disp16, 11=reg
    ; 10001110 : 11 sreg3 reg -> 1 bytes
    ; 10001110 : mod sreg3 r/m
    mov     al,byte [di]        ; Load Mod/sreg/rm byte
    and     al,0b_00_111_000
    jz      MovConfirmed.ds_or_es   ; it's a MOV ES,something since ES=000
    cmp     al,0b_00_011_000
    jz      MovConfirmed        ; it's a MOV DS,something
    inc     di                  ; We don't have to recheck Mod/r/m byte
    dec     cx
    jmp     .find_next_MOV      ; If it's not dealing with DS or ES
                                ; we leave it alone
MovConfirmed:
    mov     al,0x80             ; InstStore.Status flag: DS
                                ; If it was ES, AL already = 00
.ds_or_es:                      ; It is MOV ES,xxx or MOV DS,xxx
    mov     dl,al               ; Save state byte for later use
    push    cx
    dec     di                  ; ES:DI points to instruction
    
    ; Let's calculate instruction length with prefix included
    ; DS = CS, ES:DI: current instruction, AL,AH: instruction, mode r/m byte
    mov     ax,word [di]        ; Load instruction + 1 byte (probably mode reg r/m)
    call    GetInstructionLength
    ; CX = instruction length
    jcxz    .invalid            ; Skip creating breakpoint, if it's invalid
    mov     al,dl               ; restore state byte
    or      al,cl               ; Set length (bit 0-2) in status byte
    ; Create a breakpoint at ES:DI
    call    CreateBreakPoint
.invalid:
    pop     cx
    or      cx,cx
    jnz     InitAutoPatcher.find_next_MOV

Instrumented:
    ; TODO: also implement POP DS,ES search and instrumentation (1F & 07)
    ; TODO: also implement LES reg and LDS reg search and instrumentation (C4 & C5)
    pop     es
    pop     ds
    pop     di
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    pop     bp
    ret


;----------------------------------------------------------------------------
; DestroyAutoPatcher
;-------------------------------
DestroyAutoPatcher:
    ; TODO: Remove left over application instrumentation
    ; TODO: Free reserved memory
    ret

;----------------------------------------------------------------------------
; Create a new break point for ES:DI
; IN:
;   - AL: InstStore.Status
;   - ES:DI: break point place
;   - CX: instruction length
; OUT:
;   - ES:DI= next instruction after breakpoint
;   - CX= 0
;   - AL= 90h
;-------------------------------
CreateBreakPoint:
    push    bx
    push    dx
    push    si
    push    di
    push    es
    push    ds
    push    cx

    mov     si,di               ; Move ES:DI into DS:SI
    mov     bx,es
    mov     ds,bx
    mov     dx,si               ; Save SI for later use
    ; We load the TOP pointer of InstStore
    cs les  di,[InstStoreAddress - PART_TSR]        ; Offset + Segment
    inc     di
    inc     di                  ; Skip address for now
    stosb                       ; Write Status
    mov     ax,0x9090           ; Prefill area with NOPs
    stosw                       ; 5xNOP
    stosw
    stosb
    
    ; We take the opportunity, that DI points right after the last
    ; InstStore value, so we save it back for next time
    cs mov  word [InstStoreAddress - PART_TSR],di   ; Offset
    
    ; Calculate physical address of CS:IP
    mov     ax,dx               ; AX=IP
    
    mov     cl,12
    mov     bx,ds               ; BX=CS
    shr     bx,cl               ; BX=CS upper 4 bits
    
    mov     cx,ds
    shl     cx,1
    shl     cx,1
    shl     cx,1
    shl     cx,1
    add     ax,cx               ; AX=Phisical Address lower 16 bits
    
    adc     bx,0                ; BX=Phisical Address upper 4 bits
    shl     bx,1                ; BX=InstStoreTable index
    
    ; Update TOP pointer
    cs mov  word [InstStoreAddress - PART_TSR],di
    ; Update InstStoreTable[bx + 1] to new end value index
    mov     cx,di
    shr     cx,1
    shr     cx,1
    shr     cx,1
    cs mov  word [bx + (InstStoreTable - PART_TSR + 2)],cx
    sub     di,8                ; DI points to the InstStore.Address
    
    cs cmp  word [bx + (InstStoreTable - PART_TSR)],0
    jnz     .entryStarted
    dec     cx
    cs mov  word [bx + (InstStoreTable - PART_TSR)],cx
.entryStarted:
    stosw                       ; Write the phisical address to InstStore.Address
    shl     bx,1
    shl     bx,1
    es or   [di],bl             ; Save phys. addr. upper 4 bits into
                                ; Status byte's bits 3-6
    inc     di                  ; Skip the status byte in InstStore
    
    pop     cx
    push    cx
    rep movsb                   ; Copy instruction
    
    pop     cx
    pop     ds
    pop     es
    pop     di
    pop     si
    pop     dx
    pop     bx
    ; Now place INT 3 + NOPs
    mov     al,0xcc             ; INT 3 instruction
    stosb
    mov     al,0x90             ; NOP instruction
    dec     cx                  ; CX = instruction length - 1
    rep stosb
    ret

;----------------------------------------------------------------------------
; Calculates instruction length according to mod/rm byte
; If there is segment prefix, it counts as well, and decrements DI
; IN:
;   - DS: CS
;   - AL: instruction
;   - AH: mod/rm byte (if any)
;   - ES:DI: points to instruction
; OUT:
;   - CX= length of instruction
;   - ES:DI= pointer to instruction with prefix (if exisits)
;-------------------------------
GetInstructionLength:
    ; Setup DS:BX for xlat
    xor     cx,cx
    mov     bx,(Instructions - PART_TSR)
    xlat
    mov     cl,al
    jcxz    .Exit               ; If AL = 0, it's an invalid instruction
    mov     bh,ah               ; Save AH for later reuse
    and     cl,0b_00000111      ; From instruction description we use length
    and     al,0b_00_111_000    ; Mask out mode r/m type
    jz      .Exit               ; If not have any kind of mode r/m byte, we have the length already
    cmp     al,MOD_INS2_RM
    jb      .mod_reg_rm         ; It has regular mode reg r/m byte
    jz      .mod_ins2_rm        ; It's a mode instruction2 r/m byte
    cmp     al,MOD_INS4_RM
    jb      .mod_ins3_rm        ; It's a mode instruction3 r/m byte
    jz      .mod_ins4_rm        ; It's a mode instruction4 r/m byte
.mod_ins5_rm:                   ; Else it's a mode instruction5 r/m byte
    and     ah,0b_00_111_000    ; Get the extended instruction part
    cmp     ah,0b_00_111_000    ; Only instruction 111 is not defined
    jz      .ExitInvalid
    mov     ah,bh
    and     ah,0b_11_000_000    ; Check mode part
    cmp     ah,0b_11_000_000    ; JMP or CALL with register is invalid
    jz      .ExitInvalid
    jmp     .mod_reg_rm
    
.mod_ins4_rm:
    and     ah,0b_00_111_000    ; Get the extended instruction part
    cmp     ah,0b_00_001_000    ; only INC (000), DEC (001) is valid
    ja      .ExitInvalid
    jmp     .mod_reg_rm         ; Otherwise the lengths are the same

.mod_ins3_rm:
    and     ah,0b_00_111_000    ; Get the extended instruction part
    jz      .inst_test_rm_imm
    cmp     ah,0b_00_001_000    ; Extended instruction 001 is invalid
    jz      .ExitInvalid
    jmp     .mod_reg_rm         ; Else it's need to be decoded just list normal mode rm byte

.inst_test_rm_imm:
    test    byte [di],1         ; Instruction byte's 0 bit has the info of the length of
    jz      .inst_test_rm_imm8  ; immediate parameter
    inc     cx
.inst_test_rm_imm8:
    inc     cx
    jmp     .mod_reg_rm

.mod_ins2_rm:                   ; It's almost the same, just 110 inst is not defined
    and     ah,0b_00_111_000    ; Get the extended instruction part
    cmp     ah,0b_00_110_000    ; Check for invalid instruction
    jz      .ExitInvalid        ; Handle invalid extended instruction
.mod_reg_rm:
    mov     ah,bh               ; Restore AH, and resolve as normal mode reg r/m byte
    and     ah,0b_11_000_111    ; Mask out reg part
    cmp     ah,0b_00_000_110    ; Special case: no index, just disp16
    jz      .IncLen2
    and     ah,0b_11_000_000    ; Mask out r/m part also
    jz      .ModRmSizeReady     ; Mod 00 -> just index, no more bytes (special case already handled)
    cmp     ah,0b_10_000_000
    ja      .haveNoSegPrefix    ; Mod 11 -> just registers, no more bytes
                                ; In this case, segment prefix is also non-sense
    jb      .IncLen             ; Mod 01 -> disp8, 1 more byte
.IncLen2:
    inc     cx                  ; Mod 10 -> disp16, 2 more bytes
.IncLen:
    inc     cx
.ModRmSizeReady:
    ; TODO: need to rethink prefix addition
    mov     ah,byte [di-1]      ; Check previous byte
    and     ah,0b_11100111
    cmp     ah,0x26             ; Is it a segment prefix?
    jnz     .haveNoSegPrefix
    dec     di                  ; We have prefix, ES:DI points to prefix
    inc     cx                  ; Instruction length +1
.haveNoSegPrefix:
.Exit:
    ret
.ExitInvalid:
    xor     cx,cx
    ret

;----------------------------------------------------------------------------
; Evaluates loaded segment if it is pointing to the right place
; If not, corrects it
; IN:
;   - AX: segment
; OUT:
;   - AX= corrected segment, or same
;-------------------------------
EvalSegment:
    cmp     ax,0xf000
    jb      .needNoCorrection
    
    mov     bx,0xffa6 - ((CGAfont - PART_TSR) / 0x10)
    sub     bx,ax
    mov     ax,cs
    sub     ax,bx               ; AX = CS - (FFA6 - AX - CGAfont.seg)
                                ; Needs: CS > 1000 !!!!
    ret

.needNoCorrection:
    ; TODO: check if it's pointing into the InstStore
    
;-------------------------------
; Remove breakpoint
;-------------------------------
    push    ax                  ; We don't alter loaded segment
    cs mov  ds,word [InstStoreAddress - PART_TSR + 2]
    cs mov  si,word [BreakPointChecked - PART_TSR]
    
    lodsw
    mov     di,ax               ; AX = DI = breakpoint phys address lower 16 bits
    lodsb                       ; Status byte
    xor     ah,ah
    ; Indicate that this slot is not used anymore
    mov     byte [si-1],ah      ; Clear the status byte/length
    mov     dx,ax
    and     al,0b_1111_000
    mov     cl,9
    shl     ax,cl
    mov     es,ax               ; ES:DI points to original instruction's place
    and     dl,7
    mov     cx,dx               ; Length of instruction to restore
    ; Restore original instruction
    rep movsb
    
    ; We rerun to the next instruction after IRET
    cs lds  bx,[REGSTORE3.SP - PART_TSR]
    dec     word [bx]
    add     word [bx],dx
    
    ; TODO: Mark InstStoreTable entry having empty slots
    
    pop     ax                  ; Return simulated segment load without change
    ret

;----------------------------------------------------------------------------
; INT 3 handler (debug trap)
; - Finds caller address in InstStore
; - Emulates original instruction
; - Checks result if DS or ES >= F000
; -   Sets return DS or ES to point to fake char map
; -   and returns
; - Else
; TODO: If the result pointing into InstStore
; TODO:   Disables all breakpoints above the new DS or ES
; TODO:     How to later reenable them???
; -   Removes breakpoint
; - Returns to next instruction
; IN:
;   - Stack: caller's CS:IP
; OUT:
;   - ES or DS set as it would be with original instruction
;       Except if it's bigger than F000, then it points fake BIOS Char Map
;-------------------------------
Break:
.IP:        dw 0
.CS:        dw 0

INT_3:
    ; Save CPU state
    cs mov  word [REGSTORE3.SP - PART_TSR],sp
    cs mov  word [REGSTORE3.SS - PART_TSR],ss
    ; Setup new stack
    mov     sp,cs
    mov     ss,sp
    mov     sp,(Int3Stack.Top - PART_TSR)
    
    push    ax
    push    bx
    push    cx
    push    dx
    push    si
    push    di
    push    bp
    push    ds
    push    es
.SP_TOP:    equ (Int3Stack.Top - PART_TSR) - 9*2

    cld
    mov     ax,cs
    mov     es,ax               ; ES:DI = CS:Break.IP
    mov     di,(Break.IP - PART_TSR)
                                ; DS:SI = original SS:SP
    cs lds  si,[REGSTORE3.SP - PART_TSR]
    ; Copy breakpoint data
    lodsw                       ; Copy breakpoint IP
    dec     ax                  ; pointing to INT3 instead after it
    stosw
    movsw                       ; Copy breakpoint CS
    
                                ; DS=InstStore segment
    cs mov  ds,word [InstStoreAddress - PART_TSR + 2]   ; Segment
    
    ; Get InstStore of the original instruction
    cs mov  ax,word [Break.CS - PART_TSR]  ; Caller's CS
    mov     bx,ax
    mov     dx,ax
    mov     cl,12
    shr     bx,cl               ; BX=CS upper 4 bits
    shl     ax,1
    shl     ax,1
    shl     ax,1
    shl     ax,1
    cs add  ax,word [Break.IP - PART_TSR]  ; AX=Phisical Address lower 16 bits
    adc     bx,0                ; BX=Phisical Address upper 4 bits
    shl     bl,1                ; BX=InstStoreTable index
    ; Load table lower & upper bound indexes
    cs mov  si,word [bx + (InstStoreTable - PART_TSR)]
    cs mov  di,word [bx + (InstStoreTable - PART_TSR + 2)]
    cmp     si,di
    jnb     Search.NotFound
    
    ; Do a quick search for the Address
    ; Max 13 iterations, if the max number of 8191 breakpoints are installed
    ; in the same phisical 64k block, typically it will be under 10 (estimated)
Search:
    ; BX=((SI+DI)/2)*8, DX=((SI+DI)/2)
    mov     bx,si
    add     bx,di
    shr     bx,1
    mov     dx,bx               ; DX=InstStore index
    shl     bx,1
    shl     bx,1
    shl     bx,1                ; BX=InstStore pointer of index
    cmp     ax,word [bx]
    jb      .smaller
    ja      .bigger
    jmp     BreakPointFound     ; If AX = InstStore.Address, we found it
    
.smaller:
    cmp     di,dx
    jz      .NotFound
    mov     di,dx               ; If AX < InstStore.Address, DI=current index
    jmp     Search
.bigger:
    cmp     si,dx
    jz      .NotFound
    mov     si,dx               ; If AX > InstStore.Address, SI=current index
    jmp     Search
.NotFound:
    pop     es
    pop     ds
    pop     bp
    pop     di
    pop     si
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    cs mov  sp,word [REGSTORE3.SP - PART_TSR]
    cs mov  ss,word [REGSTORE3.SS - PART_TSR]
    ; Call original INT 3
    cs jmp far [INT_3_original - PART_TSR]

BreakPointFound:
    cs mov  word [BreakPointChecked - PART_TSR],bx
    mov     si,bx
    add     si,2
    mov     di,.simulationArea - PART_TSR
    lodsb                       ; Load status into AL
    mov     cx,5
    rep movsb                   ; Copy original instruction to simulation area
    
    test    al,0x80             ; Is it ES or DS modification?
    mov     ax,0x9090           ; 2xNOP instruction, for the case it's ES
    jnz     .restorePrepared    ; We need to evaluate DS
                                ; Else we need to eval ES, by jumping to alternate evaluation
    mov     ax,0xeb + (Evaluation.evalES - (Evaluation+2))*0x100    ; JMP instruction
.restorePrepared:
    mov     di,Evaluation - PART_TSR
    stosw                       ; Write the JMP or NOP instructions to help evaluation
    
    es cmp  byte [.simulationArea - PART_TSR],0x2e  ; CS segment override prefix
    jz      CSoverridden
    
    ; By default we restore DS & ES. However, if CS prefix is used, we substitute it with ES
    pop     es
.prepareSimulation:
    pop     ds
    pop     bp                  ; Restore register values of the running program
    pop     di                  ; 'cause we don't know which one will be used
    pop     si                  ; during simulation (could be any)
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    cs mov  sp,word [REGSTORE3.SP - PART_TSR]
    cs mov  ss,word [REGSTORE3.SS - PART_TSR]
.simulationArea:
    nop                         ; This will be a MOV {DS,ES}, something
    nop
    nop
    nop
    nop
    
    mov     sp,cs               ; Restore INT3 stack with saved registers
    mov     ss,sp
    mov     sp,INT_3.SP_TOP

Evaluation:
    ; JMP .evalES instruction
    dw      0xeb + (Evaluation.evalES - (Evaluation+2))*0x100

.evalDS:
    ; We evaluate DS here, and restore ES in the end
    
    mov     ax,ds
    call    EvalSegment
    mov     ds,ax
    
    pop     es
    inc     sp                  ; We simply discard DS loading
    inc     sp                  ; ..and it's faster
.ExitInt3:
    pop     bp                  ; Restore all other CPU state
    pop     di
    pop     si
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    cs mov  sp,word [REGSTORE3.SP - PART_TSR]
    cs mov  ss,word [REGSTORE3.SS - PART_TSR]
    iret

.evalES:
    ; We evaluate ES here, and restore DS in the end
    mov     ax,es
    call    EvalSegment
    mov     es,ax

    inc     sp                  ; We simply discard ES loading
    inc     sp                  ; ..and it's faster
    pop     ds
    jmp     .ExitInt3

CSoverridden:
    ; Let's override for simulation with ES instead
    es mov  byte [BreakPointFound.simulationArea - PART_TSR],0x26
    mov     es,word [Break.CS - PART_TSR]
    inc     sp                  ; We skip restoring ES from stack
    inc     sp
    jmp     BreakPointFound.prepareSimulation

;----------------------------------------------------------------------------
; InstStoreTable stores 16 pointers to InstStore's first entries of
; that high 4 bit addresses of the 20 bit address space
; Phisical address bits: |19|18|17|16| |15|14|13|12|...| 3| 2| 1| 0|
;                        +--+--+--+--+ +--+--+--+--+..+--+--+--+--+
;                        IST[0..4]     | r| r| r|IS[?].Address / 8
InstStoreTable:
    times 17 dw 0
InstStoreAddress:
    dw      0,0         ; Offset, Segment order
InstStoreSize:
    dw      0
BreakPointChecked:
    dw      0           ; pointer to breakpoint which was recently checked

;    times 16-(($-PART_TSR) % 16) db 0xff   ; Padding to align InstStore to segment start
;----------------------------------------------------------------------------
; InstStore contains an ordered list of 8 bytes per breakpoint
; The break point address needs to be binary searched
; The .Address contains the lower 16 bits of phisical address
; The bounds can be found in the InstStoreTable:
; 4 high phisical address bits indexes into InstStoreTable, which
; points to start of the range. The next index pointer is the
; upper bound.

; This is dynamically allocated from DOS
;InstStore:
;.Address:       equ $-InstStore
;    dw      0
;.Status:        equ $-InstStore
;    db      0                   ; bit 0-2: Instruction length
;                                ; bit 3-6: Reserved, must be 0
;                                ; bit 7: 0 if ES, 1 if DS is modified
;.Instruction:   equ $-InstStore
;    times 5 nop                 ; Max length = 5: Prefix, 8E, Mod, disp16_L, disp16_H
