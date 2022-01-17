;----------------------------------------------------------------------------
; Functions for CGA emulator, BIOS Char map access AutoPatcher
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; InitAutoPatcher
; Initializes auto paching mechanism, by
; - Hooks to int3 interrupt
; - Beuilds up breakpoint table
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
    push    cx
    push    di
    push    ds
    push    es
    
    ; TODO: Register INT3 interrupt handler, if it wasn't yet
    ; TODO: Reserve memory area to work with, if needed
    
    mov     es,word [bp+(2+2)]  ; INT 10h caller's CS on stack
    ; Find E8 instructions on callers CS segment
    ; TODO: check less, if loaded program is smaller
    mov     cx,0xfffc           ; Check whole segment - 4 (not to roll around the segment edge)
    xor     di,di               ; Starting from the beginning
    cld
.find_next_MOV:
    mov     al,0x8e             ; MOV sreg,xxx first byte
    repne scasb                 ; ~1M cycles = 0.2s for 64k
    je      .MOV_found

.instrumented:
    ; TODO: also implement POP DS,ES search and instrumentation
    pop     es
    pop     ds
    pop     di
    pop     cx
    pop     ax
    pop     bp
    ret

.MOV_found:
    ; sreg3: ES=000, DS=011
    ; reg  : 000-111=AX,CX,DX,BX,SP,BP,SI,DI
    ; mod  : 00=disp0, 01=disp8, 10=disp16, 11=reg
    ; 10001110 : 11 sreg3 reg -> 1 bytes
    ; 10001110 : mod sreg3 r/m
    es mov  al,byte [di]        ; Load Mod/sreg/rm byte
    and     al,0b_00_111_000
    jz      .ds_or_es           ; since ES=000
    cmp     al,0b_00_011_000
    jz      .ds_or_es
    inc     di                  ; We don't have to recheck Mod/r/m byte
    dec     cx
    jmp     .find_next_MOV      ; If it's not dealing with DS or ES
                                ; we leave it alone
.ds_or_es:                      ; It is MOV ES,xxx or MOV DS,xxx
    ; TODO: check if it has Segment register prefix
    dec     di                  ; ES:DI points to instruction
    ; TODO: check if it has SS Segment reference (default, or prefixed) and cancel instrumentation
    
    ; TODO: do instrumentation
    or      cx,cx
    jnz     .find_next_MOV
    jmp     .instrumented

DestroyAutoPatcher:
    ; TODO: Remove left over application instrumentation
    ; TODO: Free reserved memory
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
    sub     ax,bx               ; AX = CS - (FFA6 - CGAfont.seg - AX)
    pop     bx
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
    add     ax,InstStore - PART_TSR
    mov     ds,ax               ; DS=InstStore segment
    
    ; Get InstStore of the original instruction
    mov     ax,word [bp+2]      ; Caller's CS
    mov     bx,ax
    mov     dx,ax
    mov     cl,12
    shr     bx,cl               ; BX=CS upper 4 bits
    add     ax,word [bp]        ; AX=Phisical Address lower 16 bits
    adc     bx,0                ; BX=Phisical Address upper 4 bits
    shl     bl,1                ; BX=InstStoreTable index
    cs mov  si,word [InstStoreTable - PART_TSR + bx]
    cs mov  di,word [InstStoreTable - PART_TSR + 2 + bx]
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
    cmp     ax,word [bx + (InstStore.Address - InstStore)]
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
    mov     al,0x9090           ; 2xNOP instruction, for the case it's ES
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
    es mov  byte [.simulationArea - PART_TSR],0x26
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

    times 16-(($-PART_TSR) % 16) db 0xff   ; Padding to align InstStore to segment start
;----------------------------------------------------------------------------
; InstStore contains an ordered list of 8 bytes per breakpoint
; The break point address needs to be binary searched
; The .Address contains the lower 16 bits of phisical address
; The bounds can be found in the InstStoreTable:
; 4 high phisical address bits indexes into InstStoreTable, which
; points to start of the range. The next index pointer is the
; upper bound.
InstStore:
.Address:       equ $-InstStore
    dw      0
.Status:        equ $-InstStore
    db      0                   ; bit 0-2: Instruction length
                                ; bit 3-6: Reserved, must be 0
                                ; bit 7: 0 if ES, 1 if DS is modified
.Instruction:   equ $-InstStore
    times 5 nop                 ; Max length = 5: Prefix, 8E, Mod, disp16_L, disp16_H
