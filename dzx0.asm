; -----------------------------------------------------------------------------
; ZX0 8080 decoder by Ivan Gorodetsky
; Based on ZX0 z80 decoder by Einar Saukas
; v1 (2021-02-15) - 103 bytes forward / 100 bytes backward
; v2 (2021-02-17) - 101 bytes forward / 100 bytes backward
; v3 (2021-02-22) - 99 bytes forward / 98 bytes backward
; v4 (2021-02-23) - 98 bytes forward / 97 bytes backward
; v5 (2021-08-16) - 94 bytes forward and backward
; v6 (2021-08-17) - 92 bytes forward / 94 bytes backward
; v7 (2022-04-30) - 92 bytes forward / 94 bytes backward (source address now in DE, slightly faster)
; v8 (2025-08-25) - 90 bytes forward V1 / 92 bytes forward V2 / 92 bytes backward (source address in HL again now)
; -----------------------------------------------------------------------------
; Parameters (forward):
;   HL: source address (compressed data)
;   BC: destination address (decompressing)
;
; Parameters (backward):
;   HL: last source address (compressed data)
;   BC: last destination address (decompressing)
; -----------------------------------------------------------------------------
; compress forward to old (classic) file format V1 with -c option (better choice for 8080)
; you can compress forward to new file format V2 with no options
;
; compress backward with -b option
;
; Compile with The Telemark Assembler (TASM) 3.2
; ----------------------------------------------------------------------------- 

;#define BACKWARD
;#define V2

#ifdef BACKWARD
#define NEXT_HL dcx h
#define NEXT_BC dcx b
#else
#define NEXT_HL inx h
#define NEXT_BC inx b
#endif


dzx0:
#ifdef BACKWARD
        lxi d,0
		push d
#else
        lxi d,0FFFFh
		push d
		inx d
#endif
		mvi a,080h
dzx0_literals:
		call dzx0_elias
		call dzx0_ldir
		jc dzx0_new_offset
		call dzx0_elias
dzx0_copy:
		xthl
		push h
#ifdef BACKWARD
		inx h
#endif
		dad b
		call dzx0_ldir
		pop h
		xthl
		jnc dzx0_literals
dzx0_new_offset:
#ifdef BACKWARD
		call dzx0_elias
		inx sp
		inx sp
		dcr d
		rz
		dcr e
		mov d,a
		mov a,e
#else
#ifdef V2
		pop d
		mvi e,0FEh
		call dzx0_elias_loop
		inr e
		rz
		mov d,a
		mov a,e
#else
		call dzx0_elias
		mov d,a
		pop psw
		xra a
		sub e
		rz
#endif
#endif
		rar
		push psw
		mov a,m
		rar
		inx sp
		push psw
		inx sp
		NEXT_HL
		mov a,d
		lxi d,1
#ifdef BACKWARD
		cc dzx0_elias_backtrack
#else
		cnc dzx0_elias_backtrack
#endif
		inx d
		jmp dzx0_copy
dzx0_elias:
		inr e
dzx0_elias_loop:	
		add a
		jnz dzx0_elias_skip
		mov a,m
		NEXT_HL
		ral
dzx0_elias_skip:
#ifdef BACKWARD
		rnc
#else
		rc
#endif
dzx0_elias_backtrack:
		xchg\ dad h\ xchg
		add a
		jnc dzx0_elias_loop
		jmp dzx0_elias
		
dzx0_ldir:
		push psw						
dzx0_ldir1:
		mov a,m
		stax b
		NEXT_HL
		NEXT_BC
		dcx d
		mov a,d
		ora e
		jnz dzx0_ldir1
		pop psw
		add a
		ret

		.end