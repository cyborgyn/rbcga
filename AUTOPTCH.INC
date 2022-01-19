;----------------------------------------------------------------------------
; Functions for CGA emulator, BIOS Char map access AutoPatcher
;----------------------------------------------------------------------------


;----------------------------------------------------------------------------
; InitAutoPatcher
; Initializes auto paching mechanism, by
; - Builds up breakpoint table
; - Finds MOV {DS,ES},r/m instructions in CS segment
; - Replaces MOVs with INT3 + n*NOP
; IN:
;   - Stack + 4: caller's CS:IP
;-------------------------------
malloc_InstStore:
    mov     bx,0x0fff           ; 64k - 16 bytes in paragraphs
.retry:
    ; Save the InstStoreSize
    cs mov  word [InstStoreSize - PART_TSR],bx
    mov     ah,0x48             ; DOS reserve memory
    int     0x21
                                ; If there was an error, BX is the lagerst available block
    jc      .retry              ; Let's retry with that one
    ; AX= reserved memory segment address
    cs mov  word [InstStoreAddress - PART_TSR],ax
    cs mov  word [InstStoreAddress - PART_TSR + 2],0
    cs mov  ax,word [InstStoreSize - PART_TSR]
    mov     cl,4
    shl     ax,cl
    cs mov  word [InstStoreSize - PART_TSR],ax
    jmp     InitAutoPatcher.InstStore_allocated
    
InitAutoPatcher:
    push    bp
    mov     bp,sp
    add     bp,2                ; BP points to stack on function entry
    push    ax
    push    cx
    push    di
    push    ds
    push    es
    
    ; Check if InstStore already allocated
    cs cmp  word [InstStoreAddress - PART_TSR],0
    jz      malloc_InstStore
.InstStore_allocated:
    
    mov     es,word [bp+(2+2)]  ; INT 10h caller's CS on stack
    ; Find E8 instructions on callers CS segment
    ; TODO: check less, if loaded program is smaller
    mov     cx,0xfffb           ; Check whole segment - 5 (not to roll around the segment edge)
    xor     di,di               ; Starting from the beginning + 1
    inc     di
    cld
.find_next_MOV:
    ; Top pointer of InstStore
    cs mov  ax,word [InstStoreAddress - PART_TSR + 2]
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
    es mov  al,byte [di]        ; Load Mod/sreg/rm byte
    mov     ah,al               ; Save for later use
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
    push    cx
    dec     di                  ; ES:DI points to instruction
    
    ; Let's calculate instruction length with prefix included
    call    GetInstructionLength
    ; CX = instruction length
    or      al,cl               ; Set length (bit 0-2) in status byte
    ; Create a breakpoint at ES:DI
    call    CreateBreakPoint
    
    pop     cx
    or      cx,cx
    jnz     InitAutoPatcher.find_next_MOV

Instrumented:
    ; TODO: also implement POP DS,ES search and instrumentation (1F & 07)
    ; TODO: also implement LES reg and LDS reg search and instrumentation (C4 & C5)
    pop     es
    pop     ds
    pop     di
    pop     cx
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
    cs les  di,[InstStoreAddress - PART_TSR]
    inc     di
    inc     di                  ; Skip address for now
    stosb                       ; Write Status
    mov     ax,0x9090           ; Prefill area with NOPs
    stosw                       ; 5xNOP
    stosw
    stosb
    
    ; We take the opportunity, that DI points right after the last
    ; InstStore value, so we save it back for next time
    cs mov  word [InstStoreAddress - PART_TSR + 2],di
    
    ; Calculate physical address of CS:IP
    mov     ax,dx               ; AX=IP
    mov     bx,ds               ; BX=CS
    mov     cl,12
    shr     bx,cl               ; BX=CS upper 4 bits
    mov     cx,ds
    add     ax,cx               ; AX=Phisical Address lower 16 bits
    adc     bx,0                ; BX=Phisical Address upper 4 bits
    shl     bl,1                ; BX=InstStoreTable index
    
    ; Update InstStoreTable[bx + 1] to new end value
    cs mov  word [bx + (InstStoreTable - PART_TSR + 2)],di
    
    sub     di,8                ; DI points to the InstStore.Address
    
    cs cmp  word [bx + (InstStoreTable - PART_TSR)],0
    jnz     .entryStarted
    cs mov  word [bx + (InstStoreTable - PART_TSR)],di
.entryStarted:
    stosw                       ; Write the phisical address to InstStore.Address
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
;   - AH: mod/rm byte
;   - ES:DI: points to instruction
; OUT:
;   - CX= length of instruction
;   - ES:DI= pointer to instruction with prefix (if exisits)
;-------------------------------
GetInstructionLength:
    mov     cx,2                ; Since we surely have instruction + mod/rm byte
    and     ah,0b_11_000_111    ; Mask out reg part
    cmp     ah,0b_00_000_110    ; Special case: no index, just disp16
    jz      .IncLen2
    and     ah,0b_11_000_000    ; Mask out r/m part also
    jz      .ModRmSizeReady     ; Mod 00 -> just index, no more bytes (special case already handled)
    cmp     ah,0b_10_000_000
    ja      .ModRmSizeReady     ; Mod 11 -> just registers, no more bytes
    jb      .IncLen             ; Mod 01 -> disp8, 1 more bytes
.IncLen2:
    inc     cx                  ; Mod 10 -> disp16, 2 more bytes
.IncLen:
    inc     cx
.ModRmSizeReady:
    es mov  ah,byte [di-1]      ; Check previous byte
    and     ah,0b_11100111
    cmp     ah,0x26             ; Is it a segment prefix?
    jnz     .haveNoSegPrefix
    dec     di                  ; We have prefix, ES:DI points to prefix
    inc     cx                  ; Instruction length +1
.haveNoSegPrefix:
    ret

;----------------------------------------------------------------------------
; Evaluates loaded segment if it is pointing to the right place
; If not, corrects it
; IN:
;   - AX: segment
; OUT:
;   - AX= corrected segment
;-------------------------------
EvalSegment:
    cmp     ax,0xf000
    jb      .needNoCorrection
    
    push    bx                  ; Needs correction
    mov     bx,0xffa6 - ((CGAfont - PART_TSR) / 0x10)
    sub     bx,ax
    mov     ax,cs
    sub     ax,bx               ; AX = CS - (FFA6 - AX - CGAfont.seg)
    pop     bx                  ; Needs: CS > 1000 !!!!
    ret
.needNoCorrection:
    ; TODO: check if it's pointing into the InstStore
    ; TODO: remove breakpoint
    ret

;----------------------------------------------------------------------------
; INT 3 handler (debug trap)
; - Finds caller address in InstStore
; - Emulates original instruction
; - Checks result if DS or ES >= F000
; -   Sets return DS or ES to point to fake char map
; -   and returns
; TODO: Else checks the result pointing into InstStore
; TODO:   undoes all breakpoints above the new DS or ES
; TODO: undoes breakpoint, removes from InstStore
; - end returns
; IN:
;   - Stack: caller's CS:IP
; OUT:
;   - ES or DS set as it would be with original instruction
;       Except if it's bigger than F000, then it points fake BIOS Char Map
;-------------------------------
INT_3:
    cs mov  word [REGSTORE.DS - PART_TSR],ds
    cs mov  word [REGSTORE.ES - PART_TSR],es
    push    bp
    mov     bp,sp
    add     bp,2                ; BP points to stack on function entry
    push    ax
    push    bx
    push    cx
    push    dx
    push    si
    push    di
    
    mov     ax,cs
    mov     es,ax               ; ES=CS
                                ; DS=InstStore segment
    mov     ds,word [InstStoreAddress - PART_TSR]
    
    ; Get InstStore of the original instruction
    mov     ax,word [bp+2]      ; Caller's CS
    mov     bx,ax
    mov     dx,ax
    mov     cl,12
    shr     bx,cl               ; BX=CS upper 4 bits
    add     ax,word [bp]        ; AX=Phisical Address lower 16 bits
    adc     bx,0                ; BX=Phisical Address upper 4 bits
    ; Actually the address points to next instruction, so we need to decrease
    dec     ax
    sbb     bx,0
    shl     bl,1                ; BX=InstStoreTable index
    cs mov  si,word [bx + (InstStoreTable - PART_TSR)]
    cs mov  di,word [bx + (InstStoreTable - PART_TSR + 2)]
    ; TODO: need some sanity checks
    
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
    
    ; TODO: handle not found case!
.smaller:
    mov     di,dx               ; If AX < InstStore.Address, DI=current index
    jmp     Search
.bigger:
    mov     si,dx               ; If AX > InstStore.Address, SI=current index
    jmp     Search

BreakPointFound:
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
    stosw                       ; Write the JMP or NOP instructions to help evaluation
    
    es cmp  byte [.simulationArea - PART_TSR],0x2e  ; CS segment override prefix
    jz      CSoverridden
    
    ; By default we restore DS & ES. However, if CS prefix is used, we substitute it with ES
    cs mov  es,word [REGSTORE.ES - PART_TSR]
.prepareSimulation:
    cs mov  ds,word [REGSTORE.DS - PART_TSR]
    pop     di                  ; Restore register values of the running program
    pop     si                  ; 'cause we don't know which one will be used
    pop     dx                  ; during simulation (could be any)
    pop     cx
    pop     bx
    pop     ax
    pop     bp                  ; Note: even SS:BP indexing should work
.simulationArea:
    nop                         ; This will be a MOV {DS,ES}, something
    nop
    nop
    nop
    nop
Evaluation:
    ; JMP .evalES instruction
    dw      0xeb + (Evaluation.evalES - (Evaluation+2))*0x100

.evalDS:
    ; We evaluate DS here, and restore ES in the end
    push    ax
    mov     ax,ds
    call    EvalSegment
    mov     ds,ax
    pop     ax
    cs mov  es,word [REGSTORE.ES - PART_TSR]
    iret

.evalES:
    ; We evaluate ES here, and restore DS in the end
    push    ax
    mov     ax,es
    call    EvalSegment
    mov     es,ax
    pop     ax
    cs mov  ds,word [REGSTORE.DS - PART_TSR]
    iret

CSoverridden:
    ; Let's override for simulation with ES instead
    es mov  byte [BreakPointFound.simulationArea - PART_TSR],0x26
    mov     es,word [bp+2]      ; ES= original CS
    jmp     BreakPointFound.prepareSimulation

;----------------------------------------------------------------------------
; InstStoreTable stores 16 pointers to InstStore's first entries of
; that high 4 bit addresses of the 20 bit address space
; Phisical address bits: |19|18|17|16| |15|14|13|...| 3| 2| 1| 0|
;                        +--+--+--+--+ +--+--+--+...+--+--+--+--+
;                        IST[0..4]     IS[?].Address   | Free slot cnt
InstStoreTable:
    times 17 dw 0
InstStoreAddress:
    dw      0,0
InstStoreSize:
    dw      0
    
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
