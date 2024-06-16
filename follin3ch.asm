format zx81
;labelusenumeric
;LISTOFF

	// hardware options to be set and change defaults in ZX81DEF.INC
	MEMAVL	   =	   MEM_16K	   // can be MEM_1K, MEM_2K, MEM_4K, MEM_8K, MEM_16K, MEM_32K, MEM_48K
					   // default value is MEM_16K
	STARTMODE  EQU	   SLOW_MODE	   // SLOW or FAST
	DFILETYPE  EQU	   COLLAPSED	   // COLLAPSED or EXPANDED or AUTO
	STARTUPMSG EQU	  'CREATED WITH ZX81-IDE' // any message will be shown on screen after loading, max. 32 chars

	include 'SINCL-ZX\ZX81.INC'	   // definitions of constants

;LISTON



	REM _hide _noedit  _asm

; Star Tip 2 (C) 1987 Tim Follin/Your Sinclair.
; disassembled and commented by Matt B


	;test code
	
start:
	ld hl,musicData1
	call	$0f23		;FAST
	call play
	call	$0f2b		;SLOW
	ret

	;engine code

play:
;       DI                              ; Disable interrupts.
	push hl
	pop ix				; IX points to the start of the music.
nextb:
	LD    A,(IX+0)			; Look at the next byte of music.
	INC   A
	JP    NZ,bbreak 		; If A is 0FFh read a new envelope.
	INC   IX
	LD    H,(IX+1)			; Load the note length.
	LD    L,(IX+0)
	LD    (noteLength),HL
	INC   IX
	INC   IX
	LD    A,(IX+0)			; Load the attack rate.
	LD    (attackRate),A
	LD    A,(IX+1)			; Load the decay rate.
	LD    (decayRate),A
	LD    A,(IX+2)			; Decay target volume.
	LD    (decayTargetVolume),A
	INC   IX			; Move IX to next music data.
	INC   IX
	INC   IX
	JP    nextb
bbreak:
	LD    A,(decayRate)		; Copy decay rate to decay count.
	LD    (decayCount),A
	LD    A,(attackRate)		; Copy attack rate to attack count.
	LD    (attackCount),A
	LD    BC,(noteLength)		; BC contains the note length.
	LD    H,(IX+0)			; H, L and D contain the note pitches.
	LD    L,(IX+1)
	LD    D,(IX+2)
	LD    E,10			; Only control volume every ten cycles.
	LD    A,1
	LD    (volumeControl),A 	; Set volume to 1.
	LD    (attackDecay),A		; Set attack phase.
	CALL  subr			; Call subroutine that drives the beeper.
	XOR   A 			; Zero accumulator.
;       IN    A,(254)                   ; Read keyboard.
;       CPL                             ; Complement result.
;       AND   31                        ; Mask keyboard bits.
;       JP    NZ, keyp                  ; Jump if a key is pressed.
	INC   IX			; Move IX 3 bytes along.
	INC   IX
	INC   IX
	LD    A,(IX+0)			; Check for a zero.
	AND   A
	JP    NZ, nextb 		; Finished?
keyp:
;       EI                              ; Re-enable interrupts.
	RET				; Return from music program.

subr:
	PUSH  BC			; Start of subroutine. Save the note length.
	LD    A,(volumeControl) 	; Get the volume.
	LD    C,A
	DEC   H 			; Decrement counter for first channel.
	JR    NZ,labl1			; Do we play the first note yet?
	XOR   A 			; Zero A.
;       OUT   (254),A                   ; Set beeper low.
	in	a,($fe) 		; Cassette Output becomes Low

	LD    B,C			; B holds a delay.
wait1:
	DJNZ  wait1			; Wait for the first half of the duty cycle.
	LD    A,16			; Set beeper bit.
;       OUT   (254),A                   ; Set beeper high.
	out	($ff),a 		; Cassette Output becomes High

	SUB   C 			; Subtract delay from 16.
	LD    B,A
wait2:
	DJNZ  wait2			; Wait for the second half of the duty cycle.
	LD    H,(IX+0)			; Re-load H with pitch for channel 1.
labl1:
	DEC   L
	JR    NZ,labl2			; Do we play the second note yet?
	XOR   A 			; Zero A.
;       OUT   (254),A                   ; Set beeper low.
	in	a,($fe) 		; Cassette Output becomes Low

	LD    B,C
wait3:
	DJNZ  wait3			; Wait for the first half of the duty cycle.
	LD    A,16			; Set beeper bit.
;       OUT   (254),A                   ; Set beeper high.
	out	($ff),a 		; Cassette Output becomes High

	SUB   C 			; Subtract delay from 16.
	LD    B,A
wait4:
	DJNZ  wait4			; Wait for the second half of the duty cycle.
	LD    L,(IX+1)			; Re-load L with pitch for channel 2.
labl2:
	DEC   D
	JR    NZ,labl3			; Do we play the third note yet?
	XOR   A 			; Zero A.
;       OUT   (254),A                   ; Set beeper low.
	in	a,($fe) 		; Cassette Output becomes Low

	LD    B,C
wait5:
	DJNZ  wait5			; Wait for the first half of the duty cycle.
	LD    A,16			; Set beeper bit.
;       OUT   (254),A                   ; Set beeper high.
	out	($ff),a 		; Cassette Output becomes High

	SUB   C 			; Subtract delay from 16.
	LD    B,A
wait6:
	DJNZ  wait6			; Wait for the second half of the duty cycle.
	LD    D,(IX+2)			; Re-load D with pitch for channel 3.
labl3:
	DEC   E 			; Volume control loop.
	JP    NZ,labl5			; Only use every ten cycles.
	LD    E,10
	LD    A,(attackDecay)		; Attack (1) or Decay (0)?
	AND   A
	JP    Z,labl4
	LD    A,(attackCount)		; Load the current attack count.
	DEC   A 			; Subtract 1.
	LD    (attackCount),A		; Save it.
	JP    NZ,labl5			; We're done if count is not zero.
	LD    A,(attackRate)		; Loat the attack rate.
	LD    (attackCount),A		; Save it in the attack count.
	LD    A,(volumeControl) 	; Load the volume.
	INC   A 			; Increase it.
	LD    (volumeControl),A 	; Save it.
	CP    15			; Is it maxed out?
	JP    NZ,labl5			; If not, skip this next bit.
	DEC   A
	LD    (volumeControl),A 	; Decrease volume.
	XOR   A
	LD    (attackDecay),A		; Switch to decay.
	JP    labl5			; Skip to the end of the loop.
labl4:
	LD    A,(decayCount)		; Load the decay count.
	DEC   A
	LD    (decayCount),A
	JP    NZ,labl5			; Is it zero yet?
	LD    A,(decayRate)		; Load decay rate.
	LD    (decayCount),A		; Store it in count.
	LD    A,(volumeControl) 	; Load volume.
	DEC   A 			; Decrease it.
	LD    B,A			; Store it in B.
	LD    A,(decayTargetVolume)	; Load decay target.
	CP    B 			; Is volume on target?
	JP    Z,labl5
	LD    A,B			; Store new volume.
	LD    (volumeControl),A
labl5:
	POP   BC			; Restore BC
	DEC   BC			; Decrement BC
	LD    A,B			; Is the note finished?
	OR    C
	JP    NZ,subr			; If BC is not zero loop again.
	RET				; return from subroutine

; Workspace starts here.
; Initial values

noteLength:		dw $9600	; Note length counter.
volumeControl:		db $0C		; Volume control.
decayRate:		db $80		; Decay rate.
decayCount:		db $80		; Current decay count.
attackRate:		db $00		; Attack rate.
attackCount:		db $00		; Current attack count.
attackDecay:		db $00		; Attack (1) or decay (0) phase.
decayTargetVolume:	db $01		; Decay target volume.


musicData1:
	db $ff,$00,$0f,$01,$19,$01
	db $57,$56,$57
	db $44,$43,$44
	db $40,$3f,$40
	db $44,$43,$44
	db $57,$56,$57
	db $44,$43,$44
	db $40,$3f,$40
	db $44,$43,$44
	db $57,$56,$57
	db $44,$43,$44
	db $40,$3f,$40
	db $44,$43,$44
	db $57,$56,$57
	db $44,$43,$44
	db $40,$3f,$40
	db $44,$43,$44
	db $69,$68,$57
	db $69,$68,$57
	db $6f,$6e,$5c
	db $ff,$00,$1e,$01,$19,$01
	db $69,$68,$57
	db $ff,$00,$3c,$01,$19,$01
	db $57,$56,$44
	db $ff,$00,$0f,$01,$19,$01
	db $69,$68,$57
	db $69,$68,$57
	db $6f,$6e,$5c
	db $ff,$00,$1e,$01,$19,$01
	db $69,$68,$57
	db $ff,$00,$3c,$01,$19,$01
	db $69,$68,$4d
	db $ff,$00,$05,$01,$01,$0c
	db $29,$28,$29
	db $2b,$2a,$2b
	db $38,$37,$38
	db $44,$43,$44
	db $57,$56,$57
	db $5c,$5b,$5c
	db $ff,$00,$05,$01,$01,$08
	db $29,$28,$29
	db $2b,$2a,$2b
	db $38,$37,$38
	db $44,$43,$44
	db $57,$56,$57
	db $5c,$5b,$5c
	db $ff,$00,$05,$01,$01,$04
	db $29,$28,$29
	db $2b,$2a,$2b
	db $38,$37,$38
	db $44,$43,$44
	db $57,$56,$57
	db $5c,$5b,$5c
	db $ff,$00,$05,$01,$01,$02
	db $29,$28,$29
	db $2b,$2a,$2b
	db $38,$37,$38
	db $44,$43,$44
	db $57,$56,$57
	db $5c,$5b,$5c
	db $ff,$00,$78,$c8,$19,$0c
	db $57,$56,$57
	db $ff,$00,$78,$01,$c8,$01
	db $57,$56,$57
	db $00


musicData2:
	db  $ff,$60,$09,$02,$01,$0a,$41,$52,$6d,$3d,$52,$6d,$41,$52,$6d,$49
	db  $52,$6d,$ff,$00,$96,$01,$96,$01,$57,$62,$83,$ff,$00,$96,$fa,$00
	db  $0f,$57,$62,$83,$ff,$60,$09,$04,$01,$0a,$53,$5d,$7c,$46,$5d,$7c
	db  $3e,$5d,$7c,$46,$5d,$7c,$5d,$5d,$7c,$63,$5d,$7c,$5d,$53,$7c,$63
	db  $53,$7c,$6e,$53,$7c,$7c,$53,$7c,$8c,$53,$7c,$7c,$53,$7c,$6f,$53
	db  $7c,$53,$53,$7c,$ff,$60,$09,$04,$01,$0a,$64,$85,$c8,$59,$85,$c8
	db  $54,$85,$c8,$42,$84,$c7,$54,$85,$c8,$59,$85,$c8,$64,$85,$c8,$70
	db  $86,$c8,$4b,$96,$e1,$54,$96,$e1,$5f,$96,$e1,$64,$96,$e1,$71,$96
	db  $e1,$7f,$97,$e1,$71,$96,$e1,$64,$96,$e1,$4e,$9d,$eb,$58,$9d,$eb
	db  $4e,$9d,$eb,$42,$9c,$ea,$46,$9c,$eb,$58,$9c,$eb,$4e,$9d,$eb,$58
	db  $9d,$eb,$4e,$9d,$eb,$63,$9d,$eb,$69,$9d,$eb,$84,$9d,$eb,$76,$9d
	db  $eb,$76,$9d,$eb,$76,$9d,$eb,$76,$9d,$eb,$58,$63,$c7,$58,$53,$c6
	db  $57,$41,$c5,$57,$37,$c3,$58,$63,$c7,$58,$53,$c6,$57,$41,$c5,$57
	db  $37,$c3,$53,$63,$c7,$53,$53,$c6,$53,$41,$c5,$53,$37,$c3,$53,$63
	db  $c7,$53,$53,$c6,$53,$41,$c5,$53,$37,$c3,$63,$63,$df,$63,$5e,$df
	db  $63,$4a,$df,$63,$3e,$df,$63,$63,$df,$63,$5e,$df,$63,$4a,$df,$63
	db  $3e,$df,$5d,$63,$df,$5d,$5e,$df,$5d,$4a,$df,$5d,$3e,$df,$5d,$63
	db  $df,$5d,$5e,$df,$5d,$4a,$df,$5d,$3e,$df,$6f,$63,$c7,$6f,$53,$c6
	db  $6f,$41,$c5,$6f,$37,$c3,$84,$63,$c7,$84,$53,$c6,$84,$41,$c5,$84
	db  $37,$c3,$7d,$63,$df,$7d,$5e,$df,$7d,$4a,$df,$7d,$3e,$df,$94,$63
	db  $df,$94,$5e,$df,$94,$4a,$df,$94,$3e,$df,$84,$63,$c7,$84,$53,$c6
	db  $84,$41,$c5,$84,$37,$c3,$6f,$63,$c7,$6f,$53,$c6,$6f,$41,$c5,$6f
	db  $37,$c3,$63,$63,$c7,$63,$53,$c6,$63,$41,$c5,$63,$37,$c3,$63,$63
	db  $c7,$63,$53,$c6,$63,$41,$c5,$63,$37,$c3,$63,$63,$c7,$5e,$53,$c6
	db  $63,$41,$c5,$5e,$37,$c3,$63,$63,$c7,$5e,$53,$c6,$63,$41,$c5,$5e
	db  $37,$c3,$5d,$5d,$d2,$75,$58,$d2,$5c,$45,$cf,$58,$3a,$d0,$5d,$5d
	db  $d2,$75,$58,$d2,$5c,$45,$cf,$58,$3a,$d0,$5d,$5d,$d2,$75,$58,$d2
	db  $5c,$45,$cf,$58,$3a,$d0,$5c,$5c,$8b,$75,$58,$8b,$5c,$45,$8b,$58
	db  $3a,$8b,$63,$63,$de,$63,$5e,$de,$63,$4a,$dd,$62,$3e,$dc,$63,$63
	db  $de,$63,$5e,$de,$63,$4a,$6f,$62,$3e,$dc,$63,$63,$94,$63,$5e,$f8
	db  $63,$4a,$94,$62,$3e,$f8,$63,$63,$f8,$63,$5e,$f8,$63,$4a,$f8,$62
	db  $3e,$f8,$ff,$60,$09,$01,$01,$0d,$63,$63,$f8,$63,$5e,$f8,$63,$4a
	db  $f8,$62,$3e,$f8,$63,$63,$f8,$63,$5e,$f8,$63,$4a,$f8,$62,$3e,$f8
	db  $6f,$63,$f8,$6f,$5e,$f8,$6f,$4a,$f8,$6f,$3e,$f8,$6f,$63,$f8,$6f
	db  $5e,$f8,$6f,$4a,$f8,$6f,$3e,$f8,$ff,$c0,$12,$01,$01,$0d,$4a,$59
	db  $de,$53,$63,$dc,$59,$6f,$de,$53,$63,$dc,$63,$7c,$f9,$58,$6f,$f9
	db  $4a,$58,$f9,$58,$6f,$f9,$ff,$60,$09,$01,$01,$0d,$57,$68,$83,$68
	db  $68,$83,$83,$68,$83,$62,$68,$83,$68,$68,$83,$83,$68,$83,$62,$6f
	db  $94,$6f,$6f,$94,$94,$6f,$94,$58,$6f,$94,$6f,$6f,$94,$94,$6f,$94
	db  $57,$68,$83,$68,$68,$83,$83,$68,$83,$62,$68,$83,$68,$68,$83,$83
	db  $68,$83,$62,$6f,$94,$6f,$6f,$94,$94,$6f,$94,$76,$6f,$94,$6f,$6f
	db  $94,$94,$6f,$94,$ff,$60,$09,$01,$1e,$01,$6f,$94,$de,$6f,$94,$de
	db  $6f,$94,$de,$6f,$94,$de,$7d,$a6,$de,$6f,$94,$de,$7d,$a6,$de,$6f
	db  $94,$de,$5d,$8c,$de,$6f,$8c,$de,$6f,$8c,$de,$6f,$8c,$de,$7d,$8c
	db  $de,$6f,$8c,$de,$5d,$8c,$de,$6f,$8c,$de,$53,$7c,$de,$63,$7c,$de
	db  $7c,$7c,$de,$95,$7c,$de,$7c,$7c,$de,$63,$7c,$de,$53,$7c,$de,$5d
	db  $7c,$de,$63,$7c,$de,$7c,$7c,$de,$6f,$6f,$de,$6f,$6f,$de,$6f,$6f
	db  $de,$6f,$6f,$de,$6f,$6f,$de,$6f,$6f,$de,$6f,$6f,$de,$6f,$6f,$de
	db  $6f,$6f,$de,$ff,$c0,$12,$01,$00,$00,$e0,$e1,$e2,$e0,$e1,$e2,$ff
	db  $60,$09,$01,$00,$28,$5d,$7c,$93,$e0,$e1,$e2,$e0,$e1,$e2,$5d,$7c
	db  $93,$e0,$e1,$e2,$e0,$e1,$e2,$5d,$7c,$93,$e0,$e1,$e2,$ff,$80,$25
	db  $01,$00,$1e,$62,$7c,$a5,$ff,$60,$09,$01,$00,$02,$3d,$7a,$b8,$45
	db  $6e,$b8,$49,$7a,$b8,$36,$6d,$a3,$3d,$61,$a3,$41,$6d,$a3,$3d,$7a
	db  $b8,$45,$6e,$b8,$49,$7a,$b8,$36,$6d,$a3,$3d,$61,$a3,$41,$6d,$a3
	db  $ff,$c0,$12,$01,$00,$28,$e0,$e1,$e2,$e0,$e1,$e2,$ff,$60,$09,$01
	db  $00,$28,$5d,$7c,$93,$e0,$e1,$e2,$e0,$e1,$e2,$5d,$7c,$93,$e0,$e1
	db  $e2,$e0,$e1,$e2,$5d,$7c,$93,$e0,$e1,$e2,$ff,$80,$25,$01,$00,$1e
	db  $52,$6d,$82,$ff,$60,$09,$01,$00,$02,$3d,$7a,$b8,$45,$6e,$b8,$49
	db  $7a,$b8,$36,$6d,$a3,$3d,$61,$a3,$41,$6d,$a3,$45,$8a,$cf,$4e,$7c
	db  $cf,$53,$8b,$d0,$3d,$7a,$b8,$45,$6e,$b8,$49,$7a,$b8,$4e,$9c,$ea
	db  $58,$8c,$ea,$5e,$9d,$eb,$45,$8a,$cf,$4e,$7c,$cf,$53,$8b,$d0,$3d
	db  $7a,$b8,$45,$6e,$b8,$49,$7a,$b8,$36,$6d,$a3,$3d,$61,$a3,$41,$6d
	db  $a3,$30,$60,$90,$36,$56,$90,$39,$60,$90,$36,$56,$90,$30,$60,$90
	db  $36,$56,$90,$39,$60,$90,$36,$56,$90,$30,$60,$90,$36,$56,$90,$39
	db  $60,$90,$36,$56,$90,$30,$60,$90,$36,$56,$90,$39,$60,$90,$36,$56
	db  $90,$ff,$00,$96,$00,$80,$01,$39,$60,$90,$00,$00,$00,$00		


	END _asm

AUTORUN:

	REM 'Ï±ìæííû±áÓ±î÷óõáÐ×Ô××±Ï'
	CLS
	PRINT AT 3,0;"ÏÏÏÏÏÏÏÏ±ýú±ÕÎâêõ±î÷ôêã±ÏÏÏÏÏÏÏÏ"
	PRINT AT 6,0;"æïèêïæ TIM FOLLIN 3CH","ãðåæó± TIM FOLLIN (1987)";TAB 7;"(YOUR SINCLAIR NR.20)"
	PRINT AT 11,0;"ãéððôæ±áï±ðñõêðïÆ"
	PRINT AT 13,0;"Õ  MUSIC: UNTITLED";TAB 3;"AUTHOR: TIM FOLLIN"
	PRINT AT 16,0;"×  MUSIC: UNTITLED";TAB 3;"AUTHOR: UNCREDITED"
	PRINT AT 20,0;"ÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏÏ"
.choose:
	LET A$=INKEY$
	IF A$="" OR A$<"1" OR A$>"2" THEN GOTO #.choose#
	POKE #start+1,(A$="1")*(#musicData2 MOD 256)+(A$="2")*(#musicData1 MOD 256)
	POKE #start+2,(A$="1")*(#musicData2/256)+(A$="2")*(#musicData1/256)

	RAND USR #start
	GOTO #.choose#


	include 'SINCL-ZX\ZX81DISP.INC' 	 ; include D_FILE and needed memory areas

VARS_ADDR:
	db 80h

WORKSPACE:

assert ($-MEMST)<MEMAVL
// end of program
