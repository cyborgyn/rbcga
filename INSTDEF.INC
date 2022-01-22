;----------------------------------------------------------------------------
; This table defines 256 instruction descriptors, as following:
; bits  0-2 : base length of instriction
;             if 0, then this instruction is invalid
; bit   3   : it has mod/rm byte (it's defined length added to bits 0-2)
;-------------------------------
MOD_REG_RM:     equ 0b_00_001_000
MOD_INS1_RM:    equ 0b_00_001_000    ; From length & validity decode perspective == MOD_REG_RM
MOD_INS2_RM:    equ 0b_00_010_000
MOD_INS3_RM:    equ 0b_00_011_000
MOD_INS4_RM:    equ 0b_00_100_000
MOD_INS5_RM:    equ 0b_00_101_000
MOD_REG_MEM:    equ 0b_00_110_000
STR_PREFIX:     equ 0b_01_000_000
SEG_PREFIX:     equ 0b_10_000_000
IMM8:           equ 0               ; Not using this info now
IMM16:          equ 0
IMM32:          equ 0
REL8:           equ 0
REL16:          equ 0

Instructions:
;0x00
	db  2 + MOD_REG_RM      ; Add BYTE
	db  2 + MOD_REG_RM      ; Add WORD
	db  2 + MOD_REG_RM      ; Add BYTE (reg & rm iverted)
	db  2 + MOD_REG_RM      ; Add WORD
	db  2 + IMM8            ; Add AL,imm8
	db  3 + IMM16           ; Add AX,imm16
	db  1                   ; Push ES
	db  1                   ; Pop ES
	db  2 + MOD_REG_RM      ; Or BYTE
	db  2 + MOD_REG_RM      ; Or WORD
	db  2 + MOD_REG_RM      ; Or BYTE (reg & rm inverted)
	db  2 + MOD_REG_RM      ; Or WORD
	db  2 + IMM8            ; Or AL,imm8
	db  3 + IMM16           ; Or AX,imm16
	db  1                   ; Push CS
	db  0   ; Not valid

;0x10
	db  2 + MOD_REG_RM      ; Adc BYTE
	db  2 + MOD_REG_RM      ; Add WORD
	db  2 + MOD_REG_RM      ; Adc BYTE (reg & rm iverted)
	db  2 + MOD_REG_RM      ; Adc WORD
	db  2 + IMM8            ; Adc AL,imm8
	db  3 + IMM16           ; Adc AX,imm16
	db  1                   ; Push SS
	db  1                   ; Pop SS
	db  2 + MOD_REG_RM      ; Sbb BYTE
	db  2 + MOD_REG_RM      ; Sbb WORD
	db  2 + MOD_REG_RM      ; Sbb BYTE (reg & rm inverted)
	db  2 + MOD_REG_RM      ; Sbb WORD
	db  2 + IMM8            ; Sbb AL,imm8
	db  3 + IMM16           ; Sbb AX,imm16
	db  1                   ; Push DS
	db  1                   ; Pop DS

;0x20
	db  2 + MOD_REG_RM      ; And BYTE
	db  2 + MOD_REG_RM      ; And WORD
	db  2 + MOD_REG_RM      ; And BYTE (reg & rm inverted)
	db  2 + MOD_REG_RM      ; And WORD
	db  2 + IMM8            ; And AL,imm8
	db  3 + IMM16           ; And AX,imm16
	db  1 + SEG_PREFIX      ; ES prefix
	db  1                   ; Daa
	db  2 + MOD_REG_RM      ; Sub BYTE
	db  2 + MOD_REG_RM      ; Sub WORD
	db  2 + MOD_REG_RM      ; Sub BYTE (reg & rm inverted)
	db  2 + MOD_REG_RM      ; Sub WORD
	db  2 + IMM8            ; Sub AL,imm8
	db  3 + IMM16           ; Sub AX,imm16
	db  1 + SEG_PREFIX      ; CS prefix
	db  1                   ; Das

;0x30
	db  2 + MOD_REG_RM      ; Xor BYTE
	db  2 + MOD_REG_RM      ; Xor WORD
	db  2 + MOD_REG_RM      ; Xor BYTE (reg & rm inverted)
	db  2 + MOD_REG_RM      ; Xor WORD
	db  2 + IMM8            ; Xor AL,imm8
	db  3 + IMM16           ; Xor AX,imm16
	db  1 + SEG_PREFIX      ; SS prefix
	db  1                   ; Aaa
	db  2 + MOD_REG_RM      ; Cmp BYTE
	db  2 + MOD_REG_RM      ; Cmp WORD
	db  2 + MOD_REG_RM      ; Cmp BYTE (reg & rm inverted)
	db  2 + MOD_REG_RM      ; Cmp WORD
	db  2 + IMM8            ; Cmp AL,imm8
	db  3 + IMM16           ; Cmp AX,imm16
	db  1 + SEG_PREFIX      ; DS prefix
	db  1                   ; Aas

;0x40
	db  1                   ; Inc AX
	db  1                   ; Inc CX
	db  1                   ; Inc DX
	db  1                   ; Inc BX
	db  1                   ; Inc SP
	db  1                   ; Inc BP
	db  1                   ; Inc SI
	db  1                   ; Inc DI
	db  1                   ; Dec AX
	db  1                   ; Dec CX
	db  1                   ; Dec DX
	db  1                   ; Dec BX
	db  1                   ; Dec SP
	db  1                   ; Dec BP
	db  1                   ; Dec SI
	db  1                   ; Dec DI

;0x50
	db  1                   ; Push AX
	db  1                   ; Push CX
	db  1                   ; Push DX
	db  1                   ; Push BX
	db  1                   ; Push SP
	db  1                   ; Push BP
	db  1                   ; Push SI
	db  1                   ; Push DI
	db  1                   ; Pop AX
	db  1                   ; Pop CX
	db  1                   ; Pop DX
	db  1                   ; Pop BX
	db  1                   ; Pop SP
	db  1                   ; Pop BP
	db  1                   ; Pop SI
	db  1                   ; Pop DI

;0x60
    db  0   ; Not valid -v
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0   ; Not valid -A

;0x70
	db  2 + REL8            ; Jo rel8
	db  2 + REL8            ; Jno rel8
	db  2 + REL8            ; Jb rel8
	db  2 + REL8            ; Jnb rel8
	db  2 + REL8            ; Jz rel8
	db  2 + REL8            ; Jnz rel8
	db  2 + REL8            ; Jbe rel8
	db  2 + REL8            ; Ja rel8
	db  2 + REL8            ; Js rel8
	db  2 + REL8            ; Jns rel8
	db  2 + REL8            ; Jpe rel8
	db  2 + REL8            ; Jpo rel8
	db  2 + REL8            ; Jl rel8
	db  2 + REL8            ; Jge rel8
	db  2 + REL8            ; Jle rel8
	db  2 + REL8            ; Jg rel8

;0x80
	db  3 + MOD_INS1_RM + IMM8  ; {Add|Or|Adc|Sbb|And|Sub|Xor|Cmp} BYTE imm8
	db  4 + MOD_INS1_RM + IMM16 ; {Add|Or|Adc|Sbb|And|Sub|Xor|Cmp} WORD imm16
	db  3 + MOD_INS1_RM + IMM8  ; {Add|Or|Adc|Sbb|And|Sub|Xor|Cmp} BYTE imm8
	db  3 + MOD_INS1_RM + IMM8  ; {Add|Or|Adc|Sbb|And|Sub|Xor|Cmp} WORD imm8
	db  2 + MOD_REG_RM      ; Test BYTE
	db  2 + MOD_REG_RM      ; Test WORD
	db  2 + MOD_REG_RM      ; Xchg BYTE
	db  2 + MOD_REG_RM      ; Xchg WORD
	db  2 + MOD_REG_RM      ; Mov BYTE
	db  2 + MOD_REG_RM      ; Mov WORD
	db  2 + MOD_REG_RM      ; Mov BYTE (reg & rm inverted)
	db  2 + MOD_REG_RM      ; Mov WORD
	db  2 + MOD_REG_RM      ; Mov WORD from sreg
	db  2 + MOD_REG_RM      ; Lea
	db  2 + MOD_REG_RM      ; Mov WORD to sreg
	db  2 + MOD_REG_RM      ; Pop WORD

;0x90
	db  1                   ; Nop (Xchg AX,AX)
	db  1                   ; Xchg CX,AX
	db  1                   ; Xchg DX,AX
	db  1                   ; Xchg BX,AX
	db  1                   ; Xchg SP,AX
	db  1                   ; Xchg BP,AX
	db  1                   ; Xchg SI,AX
	db  1                   ; Xchg DI,AX
	db  1                   ; Cbw
	db  1                   ; Cwd
	db  5                   ; Jmp far seg:offset
	db  1                   ; Wait
	db  1                   ; Pushf
	db  1                   ; Popf
	db  1                   ; Sahf
	db  1                   ; Lahf

;0xA0
	db  3 + REL16           ; Mov BYTE AL,offset
	db  3 + REL16           ; Mov WORD AX,offset
	db  3 + REL16           ; Mov BYTE offset,AL
	db  3 + REL16           ; Mov WORD offset,AX
	db  1                   ; Movsb
	db  1                   ; Movsw
	db  1                   ; Cmpsb
	db  1                   ; Cmpsw
	db  2 + IMM8            ; Test AL,imm8
	db  3 + IMM16           ; Test AX,imm16
	db  1                   ; Stosb
	db  1                   ; Stosw
	db  1                   ; Lodsb
	db  1                   ; Lodsw
	db  1                   ; Scasb
	db  1                   ; Scasw

;0xB0
	db  2 + IMM8            ; Mov BYTE AL,imm8
	db  2 + IMM8            ; Mov BYTE CL,imm8
	db  2 + IMM8            ; Mov BYTE DL,imm8
	db  2 + IMM8            ; Mov BYTE BL,imm8
	db  2 + IMM8            ; Mov BYTE AH,imm8
	db  2 + IMM8            ; Mov BYTE CH,imm8
	db  2 + IMM8            ; Mov BYTE DH,imm8
	db  2 + IMM8            ; Mov BYTE CL,imm8
	db  3 + IMM16           ; Mov BYTE AX,imm16
	db  3 + IMM16           ; Mov BYTE CX,imm16
	db  3 + IMM16           ; Mov BYTE DX,imm16
	db  3 + IMM16           ; Mov BYTE BX,imm16
	db  3 + IMM16           ; Mov BYTE SP,imm16
	db  3 + IMM16           ; Mov BYTE BP,imm16
	db  3 + IMM16           ; Mov BYTE SI,imm16
	db  3 + IMM16           ; Mov BYTE DI,imm16

;0xC0
	db  0   ; Not valid
	db  0   ; Not valid
	db  3 + IMM16           ; Ret WORD
	db  1                   ; Ret
	db  2 + MOD_REG_MEM     ; LES reg,mem
	db  2 + MOD_REG_MEM     ; LDS reg,mem
	db  3 + MOD_REG_RM      ; Mov BYTE R/M,imm8
	db  4 + MOD_REG_RM      ; Mov WORD R/M,imm16
	db  0   ; Not valid
	db  0   ; Not valid
	db  3 + IMM16           ; Retf WORD
	db  1                   ; Retf
	db  1                   ; Int 3
	db  2 + IMM8            ; Int imm8
	db  1                   ; Into
	db  1                   ; Iret

;0xD0
	db  2 + MOD_INS2_RM     ; {Rol|Ror|Rcl|Rcr|Shl|Shr|*|Sar} BYTE R/M,1
	db  2 + MOD_INS2_RM     ; {Rol|Ror|Rcl|Rcr|Shl|Shr|*|Sar} WORD R/M,1
	db  2 + MOD_INS2_RM     ; {Rol|Ror|Rcl|Rcr|Shl|Shr|*|Sar} BYTE R/M,CL
	db  2 + MOD_INS2_RM     ; {Rol|Ror|Rcl|Rcr|Shl|Shr|*|Sar} WORD R/M,CL
	db  2 + IMM8            ; Aam imm8
	db  2 + IMM8            ; Aad imm8
	db  0   ; Not valid
	db  1                   ; Xlat
	db  0   ; Not valid -v
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0
	db  0   ; Not valid -A

;0xE0
	db  2 + REL8            ; Loopnz
	db  2 + REL8            ; Loopz
	db  2 + REL8            ; Loop
	db  2 + REL8            ; Jcxz
	db  2 + IMM8            ; In AL,imm8
	db  2 + IMM8            ; In AX,imm8
	db  2 + IMM8            ; Out imm8,AL
	db  2 + IMM8            ; Out imm8,AX
	db  3 + REL16           ; Call rel16
	db  3 + REL16           ; Jmp rel16
	db  5 + IMM32           ; Jmp far seg:offset
	db  2 + REL8            ; Jmp short rel8
	db  1                   ; In AL,DX
	db  1                   ; In AX,DX
	db  1                   ; Out DX,AL
	db  1                   ; Out DX,AX

;0xF0
	db  1                   ; Lock
	db  0   ; Not valid
	db  1 + STR_PREFIX      ; Repnz
	db  1 + STR_PREFIX      ; Repz
	db  1                   ; Hlt
	db  1                   ; CMC
	db  2 + MOD_INS3_RM     ; BYTE {Test reg,imm8|*|Not|Neg|Mul|Imul|Div|Idiv}
	db  2 + MOD_INS3_RM     ; WORD {Test reg,imm16|*|Not|Neg|Mul|Imul|Div|Idiv}
	db  1                   ; Clc
	db  1                   ; Stc
	db  1                   ; Cli
	db  1                   ; Sti
	db  1                   ; Cld
	db  1                   ; Std
	db  2 + MOD_INS4_RM     ; BYTE {Inc|Dec|*|*|*|*|*|*}
	db  2 + MOD_INS5_RM     ; WORD {Inc|Dec|Call ind|Call ind|Jmp ind|Jmp ind|Push|*}
