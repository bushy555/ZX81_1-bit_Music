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
	ld hl,musicData
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
	JP    NZ,bbreak 		 ; If A is 0FFh read a new envelope.
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


musicData:
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

	END _asm

.credits:
	REM '3-CHANNEL SOUND                 BY: TIM FOLLIN         FROM YOUR SINCLAIR N20(AUG.1987)MUSIC: UNTITLED / AUTHOR: UNKNOW   EXTRACTED FROM 1TRACKER BY        KELLY A. MURTA (2022)'
	RAND USR #start
	LIST #.credits#

AUTORUN:

	include 'SINCL-ZX\ZX81DISP.INC' 	 ; include D_FILE and needed memory areas

VARS_ADDR:
	db 80h

WORKSPACE:

assert ($-MEMST)<MEMAVL
// end of program
