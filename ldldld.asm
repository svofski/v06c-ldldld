        ; LD LD, (LD)
        .project ldldld.com
        .tape v06c-rom

; todo:
; [T] ld rp, (a16)
; [x] ld (a16), rp      
; [T] bit/res/set (tested)
; [x] implement rlc/rrc/rl/rr/srl etc from CB prefix (untested)

; [x] DD prefix (IX):  push/pop; ld ix, im16; inc/dec; ld sp,ix, ld l,(ix+nn), cp (ix+nn)
; [x] DD CB prefix set 7,(ix+46); bit n, (ix+im8); res n, (ix+im8)
; [x] ED: LDI/LDIR, 
; [ ] FD prefix (IY).. should be same as ix?
; [ ] LDD/LDDR, CPD/CPDR, IND/INDR, OUTD/OTDR seem to be not used in rogue
;
; FIXME
; 
; [ ] srl, rr, rl   set/reset Z flag
; [x] rl,rr,rlc,rrc ignores carry in
; [ ] add ix, rp    set/reset Z flag like adc hl,rp
;
; jnz $ etc -- these things never execute because bpt is inserted at jump
;
; implement:
;   emu_dd_ldrixim8:
;   emu_dd_ldixim8r
;   ed_neg


traptab_org     .equ $9000
host_org        .equ traptab_org + $100
bpt_stack       .equ traptab_org 
guest_stack     .equ traptab_org - 64

guest_bc        .equ bpt_stack - 2
guest_de        .equ bpt_stack - 4

OPC_RST0        .equ $c7        
OPC_RST1        .equ $cf
OPC_RST2        .equ $d7
OPC_RST3        .equ $df                ; emulated insn trap
OPC_RST4        .equ $e7                ; bpt after single-ended branch
OPC_RST5        .equ $ef                ; bpt after forked branch
OPC_RST6        .equ $f7                ; bpt at ret/rst/pchl

OPC_RNZ         .equ $c0 	        ; ret nz
OPC_RZ          .equ $c8 	        ; ret z
OPC_RET         .equ $c9 	        ; ret
OPC_RNC         .equ $d0 	        ; ret nc
OPC_RC          .equ $d8 	        ; ret c
OPC_RPO         .equ $e0 	        ; ret po
OPC_RPE         .equ $e8 	        ; ret pe
OPC_RP          .equ $f0 	        ; ret p
OPC_RM          .equ $f8 	        ; ret m

OPC_PCHL        .equ $e9

OPC_JMP         .equ $c3

FLAGBIT_C       .equ $01

fcb1            .equ $5c                ; default fcb
        ; BDOS functions
C_WRITE         .equ 2
C_WRITESTR      .equ 9        
F_OPEN          .equ 15          ; open file
F_CLOSE         .equ 16
F_READ          .equ 20          ; read next record
F_READRAND      .equ 33          ; read random record
F_WRITERAND     .equ 34          ; write random record

FCB_SIZE        .equ 36


        .org $100
        jmp host_org
        ; test program
test_guest:        
        mvi a, $55
        sta $1000
        call test_1
        ora a
        cnz test_2
        call test_3
        ;
        .db $ed, $7b, $06, $00   ; ld sp, (6)
        ;
        mvi e, 0
        
        ; set 7, e      e <- $80
        .db $CB, $FB
        ; set 1, e      e <- $82
        .db $CB, $CB
        ; set 2, e      e <- $86
        .db $CB, $D3
        ; res 2, e      e <- $82
        .db $CB, $93
        ; bit 2, e:  expect z
        .db $CB, $53
        jz $+6
        jmp $
        ; bit 1, e: expect nz
        .db $CB, $4B
        jnz $+6
        jmp $
        ; rlc e 
        .db $CB, $03
        ; expect carry bit set
        jc $+6
        jmp $
        

        ;
        ;     jr t1
        ; t2: 
        ;     jr t3
        ; t1: 
        ;     jr t2
        ; t3:
        .db $18, $02, $18, $02, $18, $FC
        ;     ld bc, $240
        ; lup:    
        ;     inc c
        ;     djnz lup
        .db $01, $40, $02, $0C, $10, $FD
        
        mov e, c 
        mvi c, 2
        call 5
        mvi c, 9
        lxi d, testmsg
        call 5
        rst 0
testmsg: .db 'ob, I want all my garmonbozia', 13, 10, '$'
test_1:
        ret
test_2:
        mvi c, 2
        dcr c
        rz
        jmp $-2
test_3:
        mvi c, 2
        dcr c
        jnz $-1
        ret
        
        .org host_org
        ; restrict TPA
        ; load guest program to TPA
        di
        lxi sp, bpt_stack

load_guest:
        lxi h, $100
        push h
        
          lxi d, fcb1
          mvi c, F_OPEN
          call 5
          ora a
          jm err_loadguest
_lg_loop:
          lxi d, fcb1
          mvi c, F_READ
          call 5
          ora a
          jz _lg_read_ok
          dcr a
          jz _lg_read_eof
          jmp err_loadguest
_lg_read_ok:
          lxi d, $80
          mvi c, 128
        pop h
_lg_memcpy_l:        
        ldax d \ inx d \ mov m, a \ inx h
        dcr c
        jnz _lg_memcpy_l
        push h
          jmp _lg_loop          
_lg_read_eof:
        pop h
        ; wipe out dma and fcb so that the guest doesn't confuse itself
        lxi h, fcb1
        mvi m, 0
        inx h
        mvi m, ' '
        lxi h, $80
        xra a
        mov m, a \ inr l
        jnz $-2
        jmp load_guest_exit

err_loadguest:
        pop h
        rst 0
        ;...
load_guest_exit:        
        ;
        
        ; create a fake cp/m vectors table
install_hooks:
        lda 2
        dcr a
        sta 2
        lhld 6          ; cp/m start
        dcr h           ; 256 bytes lower
        shld 6
        
        ; fill vectors with fake instructions EDED+RET
        mvi c, 16       ; how many vectors?
        mvi b, $ed      ; ed-ed for cp/m hyperswitch
_ih_lup:        
        mov m, b \ inx h \ mov m, b \ inx h \ mvi m, $c9 \ inx h
        dcr c
        jnz _ih_lup


        
install_handlers:
        mvi a, OPC_JMP
        sta 3*8
        sta 4*8
        sta 5*8
        sta 6*8
        sta 7*8
        lxi h, rst3_hand
        shld 3*8+1
        lxi h, rst4_hand
        shld 4*8+1
        lxi h, rst5_hand
        shld 5*8+1
        lxi h, rst6_hand
        shld 6*8+1
        ;lxi h, rst7_hand
        ;shld 7*8+1
run_guest:
        ;lxi h, test_guest ;$100
        lxi h, $100
        shld guest_pc
        lxi h, guest_stack
        shld guest_sp
        jmp rst5_scan
        
;rst7_hand:
;        ret
        
        ; emulated instructions
rst3_hand:
        di
        shld guest_hl                   ; save guest hl
        pop h
        push psw                        ; save guest psw
        dcx h
        shld guest_pc                   ; return addr (guest pc - 1)
        pop h
        shld guest_psw
        lhld bptsave_t_ptr              ; restore bpt insn
        lda bptsave_t
        mov m, a                        

        lxi h, 0                        ; save guest sp
        dad sp
        shld guest_sp
        lxi sp, bpt_stack               ; set host sp
        push b
        push d
        ;
        lhld guest_pc
        call emu_ld                     ; emulate ld ld, (ld)
        jmp rst5_scan_ext               ; and scan from the new pc onward

rst4_hand:
        ; bpt after single-ended branch
        di
        shld guest_hl                   ; save guest hl
        pop h
        push psw                        ; save guest psw
        dcx h
        shld guest_pc                   ; return addr (guest pc - 1)
        pop h
        shld guest_psw
        lhld bptsave_t_ptr              ; restore bpt insn
        lda bptsave_t
        mov m, a 

        lxi h, 0                        ; save guest sp
        dad sp
        shld guest_sp
        jmp rst5_scan        
        
        ; bpt at either branch end, (sp) = orig insn addr
rst5_hand:
        di
        shld guest_hl                   ; save guest hl
        pop h                           ; h <- guest pc
        push psw                        ; guest psw on stack
        dcx h                           ; h <- guest pc - 1
        shld guest_pc                   ; return addr (guest pc - 1)
        pop h                           ; save guest psw
        shld guest_psw
        lhld bptsave_t_ptr              ; restore bpt t insn
        lda bptsave_t
        mov m, a 
        lhld bptsave_f_ptr              ; restore bpt f insn
        lda bptsave_f
        mov m, a

        lxi h, 0                        ; save guest sp
        dad sp
        shld guest_sp
rst5_scan:        
        lxi sp, bpt_stack               ; set host sp
        push b                          ; save guest bc
        push d                          ; save guest de
rst5_scan_ext:
        lhld guest_pc
        call scan_until_br
        ; return control to guest
        pop d
        pop b
guest_psw .equ $+1
        lxi h, 0
        push h
        pop psw
guest_sp .equ $+1
        lxi sp, 0
guest_hl .equ $+1
        lxi h, 0
        ei
guest_pc .equ $+1
        jmp 0
        
        ; bpt at ret/ret cond/rst/pchl
rst6_hand:
        di
        shld guest_hl                   ; save guest hl
        pop h
        push psw                        ; save guest psw
        dcx h
        shld guest_pc                   ; return addr (guest pc - 1)
        pop h
        shld guest_psw
        lhld bptsave_t_ptr              ; restore bpt insn
        lda bptsave_t
        mov m, a                        

        lxi h, 0                        ; save guest sp
        dad sp
        shld guest_sp
        lxi sp, bpt_stack               ; set host sp
        push b
        push d
        ;
        lhld guest_pc
        call emu_br1                    ; emulate ret
        jmp rst5_scan_ext               ; and scan from the new pc onward

        ; singlestep a br1 insn in
        ; mem[hl] = opc
emu_br1:
        mov a, m
        cpi OPC_RET
        jz ss_ret
        cpi OPC_RZ
        jz ss_rz
        cpi OPC_RNZ
        jz ss_rnz
        cpi OPC_RC
        jz ss_rc
        cpi OPC_RNC
        jz ss_rnc
        cpi OPC_RPO
        jz ss_rpo
        cpi OPC_RPE
        jz ss_rpe
        cpi OPC_RM
        jz ss_rm
        cpi OPC_RP
        jz ss_rp
        cpi OPC_PCHL
        jz ss_pchl
        jmp $
        ret

        ; normal: user pc <- mem[guest_sp], guest_sp += 2
ss_ret:
        lhld guest_sp
        mov e, m                        ; de <- mem[guest_sp]
        inx h
        mov d, m
        inx h
        shld guest_sp                   ; guest_sp <- guest_sp + 2
        xchg
        shld guest_pc                   ; guest_pc <- return addr
        ret
        
ss_nop:        
        lhld guest_pc
        inx h
        shld guest_pc
        ret
        
ss_rz:
        lhld guest_psw
        push h
        pop psw
        jz ss_ret
        jmp ss_nop
ss_rnz:
        lhld guest_psw
        push h
        pop psw
        jnz ss_ret
        jmp ss_nop
ss_rc:
        lhld guest_psw
        push h
        pop psw
        jc ss_ret
        jmp ss_nop
ss_rnc:
        lhld guest_psw
        push h
        pop psw
        jnc ss_ret
        jmp ss_nop
ss_rpo:
        lhld guest_psw
        push h
        pop psw
        jpo ss_ret
        jmp ss_nop
ss_rpe:
        lhld guest_psw
        push h
        pop psw
        jpe ss_ret
        jmp ss_nop
ss_rm:
        lhld guest_psw
        push h
        pop psw
        jm ss_ret
        jmp ss_nop
ss_rp:        
        lhld guest_psw
        push h
        pop psw
        jp ss_ret
        jmp ss_nop

ss_pchl:
        lhld guest_hl
        shld guest_pc
        ret
        
bptsave_t_ptr:  .dw 0                   ; bpt true insn addr
bptsave_t:      .db 0                   ; bpt branch if condition true, insn addr mem[sp]
bptsave_f_ptr:  .dw 0                   ; bpt false insn addr
bptsave_f:      .db 0                   ; bpt branch if condition false, insn addr?

guest_ix:       .dw 0
guest_iy:       .dw 0
;guest_pc:       .dw 0
;guest_sp:       .dw 0
;guest_hl:       .dw 0
;guest_psw:      .dw 0

        ; hl = guest pc
        ; if first insn = br3, insert 2 bpts --- todo: emulate forking jump?
        ; if br3 follows, insert bpt at br3
scan_until_br:
        mvi d, traptab >> 8
        ; first insn
        mov e, m
        ldax d
        ora a
        jm scubr_fork
        jz scubr_emulate
        add l
        mov l, a
        mvi a, 0
        adc h
        mov h, a
        ; next insn
scubr_1:
        mov e, m                        ; de = &traptab[mem[pc]]
        ldax d                          ; a = traptab[mem[pc]]
        ora a
        jm scubr_branch                 ; found a branch
        jz scubr_emulate                ; found emulated insn
        add l                           ; normal insn, advance ptr
        mov l, a
        mvi a, 0
        adc h
        mov h, a                        ; next pc
        jmp scubr_1

        ; regular branch found at the end of a run
scubr_branch:
        mov a, m                        ; opcode
        sta bptsave_t
        shld bptsave_t_ptr
        mvi m, OPC_RST4
        ret

scubr_emulate:
        mov a, m                        ; opcode
        sta bptsave_t
        shld bptsave_t_ptr
        mvi m, OPC_RST3
        ret
        
        
;         ; unconditional jump/call at the start of a run, set break at the far end
; scubr_uncond:        
;         inx h                           ; de <- branch dst
;         mov e, m
;         inx h
;         mov d, m
;         inx h
;         xchg                            ; hl <- branch dst, de <- pc + 3
;         mov a, m                        ; bptsave_t = mem[br dst]
;         sta bptsave_t
;         mvi m, OPC_RST4                 ; mem[br dst] = rst5
;         shld bptsave_t_ptr              ; bptsave_t_ptr = br dst
;         ret
        
        ; branch/fork at the start of a run, singlestep
scubr_fork:
        cpi $81                         ; 1-byte insn: ret/ret cond/rst --- pchl must be emulated
        jz scubr_1bbr
        ; insn length 3, save branch dst and insert bpt
scubr_3bbr:
        ; smart? then it's slower!
        ;mov a, m
        ;cpi $c3                         ; jmp -- no fork
        ;jz scubr_uncond
        ;cpi $cd                         ; call -- no fork
        ;jz scubr_uncond
        
        inx h                           ; de <- branch dst
        mov e, m
        inx h
        mov d, m
        inx h
        xchg                            ; hl <- branch dst, de <- pc + 3
        mov a, m                        ; bptsave_t = mem[br dst]
        sta bptsave_t
        mvi m, OPC_RST5                 ; mem[br dst] = rst5
        shld bptsave_t_ptr              ; bptsave_t_ptr = br dst
        ; insn length 3, save nobranch dst and insert bpt (insn following current br)
        xchg                            ; hl <- pc + 3
        mov a, m
        sta bptsave_f
        mvi m, OPC_RST5                 ; mem[br dst] = rst5
        shld bptsave_f_ptr
        ret
        ; br length 1: rst, ret cond, pchl --> singlestep/emulated
        ; insert rst6 at insn
scubr_1bbr:        
        mov a, m                        ; opcode
        sta bptsave_t
        shld bptsave_t_ptr
        sta bptsave_f
        shld bptsave_f_ptr
        mvi m, OPC_RST6
        ret
        

        ; hl = guest pc
emu_ld:
        mov a, m
        cpi $10
        jz emu_djnz
        cpi $20
        jz emu_jr_nz
        cpi $30
        jz emu_jr_nc
        cpi $18
        jz emu_jr
        cpi $28
        jz emu_jr_z
        cpi $38
        jz emu_jr_c
        cpi $ed                         ; ED xx..
        jz emu_ed
        cpi $cb
        jz emu_cb
        cpi $dd                         ; ix...
        jz emu_dd
        cpi $fd
        jz emu_fd
        jmp $
       
        ; CB prefix: bit, res, set...
emu_cb:
        inx h
        ; rlc    0000 0_rrr
        ; rrc    0000 1_rrr
        ; rl     0001 0_rrr
        ; rr     0001 1_rrr
        ; sla    0010 0_rrr
        ; sra    0010 1_rrr
        ; sll    0011 0_rrr
        ; srl    0011 1_rrr
        ; 
        ; bit:   01nn n_rrr
        ; res:   10nn n_rrr
        ; set:   11nn n_rrr
        mvi a, 0b11000000
        ana m
        cpi 0b01000000
        jz emu_bit
        cpi 0b10000000
        jz emu_res
        cpi 0b11000000
        jz emu_set
        mvi a, 0b11111000
        ana m
        cpi $00
        jz emu_rlc_r
        cpi $08
        jz emu_rrc_r
        cpi $10
        jz emu_rl_r
        cpi $18
        jz emu_rr_r
        cpi $20
        jz emu_sla_r
        cpi $28
        jz emu_sra_r
        cpi $30
        jz emu_sll_r
        cpi $38
        jz emu_srl_r
        jmp $
       
       ; IY instructions -- attempt to fake it by switching IX<->IY
emu_fd:       
       push h
         lhld guest_ix  
         xchg
         lhld guest_iy          ; de <- ix, hl <- iy
         shld guest_ix          ; ix <- old iy
         xchg
         shld guest_iy          ; iy <- old ix
        pop h
        call emu_dd
        
        ; swap back
        lhld guest_ix
        xchg
        lhld guest_iy
        shld guest_ix
        xchg
        shld guest_iy
        
        ret

       
        ; IX instructions
emu_dd:
        inx h
        mov a, m
        cpi $e1 \ jz emu_dd_popix
        cpi $e5 \ jz emu_dd_pushix
        cpi $f9 \ jz emu_dd_ldspix
        cpi $cb \ jz emu_ddcb
        cpi $21 \ jz emu_dd_ldixim16
        cpi $22 \ jz emu_dd_lda16ix
        cpi $23 \ jz emu_dd_incix
        cpi $2a \ jz emu_dd_ldixa16
        cpi $2b \ jz emu_dd_decix
        cpi $09 \ jz emu_dd_addixbc
        cpi $19 \ jz emu_dd_addixde
        cpi $29 \ jz emu_dd_addixix
        cpi $39 \ jz emu_dd_addixsp
        cpi $be \ jz emu_dd_cpixim8
        cpi $ae \ jz emu_dd_xorixim8
        cpi $9e \ jz emu_dd_sbcixim8
        cpi $8e \ jz emu_dd_adcixim8
        cpi $b6 \ jz emu_dd_orixim8
        cpi $a6 \ jz emu_dd_andixim8
        cpi $96 \ jz emu_dd_subixim8
        cpi $86 \ jz emu_dd_addixim8
        cpi $e3 \ jz emu_dd_exspix
        ani 0b11000111  ; 
        cpi 0b01000110  ; LD r,(IX+im8)
        jz emu_dd_ldrixim8
        mov a, m
        ani 0b11111000
        cpi 0b01110000  ; LD (IX+im8),r
        jz emu_dd_ldixim8r
        jmp $
       
        ; ED prefix 
        ; 4B    01_00_1011 ld bc, (a16)
        ; 5B    01_01_1011 ld de, (a16)
        ; 6B    01_10_1011 ld hl, (a16)
        ; 7B    01_11_1011 ld sp, (a16)

        ; LD (NN),BC              ; ED 43 XX XX
        ; LD (NN),DE              ; ED 53 XX XX
        ; LD (NN),HL*             ; ED 63 XX XX
        ; LD (NN),SP              ; ED 73 XX XX
emu_ed:
        inx h
        ; 48    01_00_1011 ld bc, (a16)
        ; 58    01_01_1011 ld de, (a16)
        ; 68    01_10_1011 ld hl, (a16)
        ; 78    01_11_1011 ld sp, (a16)
        mvi a, 0b11001111
        ana m
        cpi 0b01001011
        jz em_ed_ldrp_a16
        cpi 0b01000011
        jz em_ed_lda16_rp
        cpi 0b01001010   ; adc hl,ss
        jz em_ed_adchlss
        cpi 0b01000010   ; sbc hl,ss
        jz em_ed_sbchlss
        mov a, m
        cpi $a0 \        jz ed_ldi
        cpi $b0 \        jz ed_ldir
        cpi $a1 \        jz ed_cpi
        cpi $b1 \        jz ed_cpir
        cpi $a8 \        jz ed_ldd
        cpi $b8 \        jz ed_lddr
        cpi $a9 \        jz ed_cpd
        cpi $b9 \        jz ed_cpdr
        cpi $5f \        jz ed_ldar
        cpi $44 \        jz ed_neg
        cpi $ed \        jz eded_cpmhook
        jmp $
        
        ; todo: find some entropy source in necrodos
ed_ldar:
        inx h
        shld guest_pc
        mvi a, 4 ; chosen by fair dice roll
                 ; guarranteed to be random
        sta guest_psw+1
        ret
ed_neg:
        jmp $
        
        ; find cp/m vector substitute (e.g. BF00 -> C000)
        ; forward call to cp/m and return back to caller code
eded_cpmhook:
        inx h
        shld guest_pc
        dcx h
        dcx h           ; eded address = cp/m vector address - 256
        inr h
        shld _cpmhook_vec

        lhld guest_psw
        push h
          lhld guest_bc
          mov b, h
          mov c, l
          lhld guest_de
          xchg
          lhld guest_hl
        pop psw
_cpmhook_vec .equ $+1        
        call 0
        push psw
          shld guest_hl
          xchg
          shld guest_de
          mov h, b
          mov l, c
          shld guest_bc
        pop h
        shld guest_psw
        ret
        
        ; ld rp, (a16)
em_ed_ldrp_a16:
        mov b, m        ; b = 4B/5B/6B/7B
        inx h
        mov e, m
        inx h
        mov d, m
        inx h
        shld guest_pc   ; update guest pc
        xchg
        mov e, m
        inx h
        mov d, m
        xchg            ; hl = mem[a16]
        mov a, b
        cpi $4B
        jnz $+7
        shld guest_bc   ; ed 48 xx xx
        ret
        cpi $5B
        jnz $+7
        shld guest_de   ; ed 58 xx xx
        ret
        cpi $6B
        jnz $+7
        shld guest_hl   ; ed 68 xx xx
        ret
        shld guest_sp   ; ed 78 xx xx
        ret
        
em_ed_lda16_rp:  
        mov b, m        ; b = 43/53/63/73
        inx h
        mov e, m
        inx h
        mov d, m        ; de = a16
        inx h
        shld guest_pc   ; update guest pc
        mov a, b
        cpi $43         ; ld (a16), bc
        jnz $+9
        lhld guest_bc
        jmp _ed_lda16_rps
        cpi $53         ; ld (a16), de
        jnz $+9
        lhld guest_de
        jmp _ed_lda16_rps
        cpi $63
        jnz $+9
        lhld guest_hl
        jmp _ed_lda16_rps
                        ; ld (a16), sp
        lhld guest_sp
_ed_lda16_rps:
        xchg            ; de <- value, hl <- a16
        mov m, e
        inx h
        mov m, d
        ret
        
        ; adc hl, bc/de/hl/sp
        ; hl <- hl + rp + CY
        ; 
        ; S is set if result is negative; otherwise, it is reset.
        ; Z is set if result is 0; otherwise, it is reset.
        ; H is set if carry from bit 11; otherwise, it is reset.
        ; P/V is set if overflow; otherwise, it is reset
        ; N is reset.
        ; C is set if carry from bit 15; otherwise, it is reset
em_ed_adchlss:
        mov a, m ; 4a:bc,5a:de,6a:hl,7a:sp
        inx h
        shld guest_pc
        cpi $4a
        jnz $+9
        lhld guest_bc \ jmp _adchlss2
        cpi $5a
        jnz $+9
        lhld guest_de \ jmp _adchlss2
        cpi $6a
        jnz $+9
        lhld guest_hl \ jmp _adchlss2
        lhld guest_sp
_adchlss2:
        xchg            ; de <- ss
        
        lda guest_psw
        rar
        jnc $+4
        inx d
        ;
        lhld guest_hl
        dad d           ; hl <- guest_hl + ss
        shld guest_hl
        ; 8080 dad only sets C, we also want Z and S
        push psw
        pop d           ; e = flags
        mvi a, $3f      ; clear S Z
        ana e
        mov e, a
        xra a
        ora h
        jp _adchlss3
        mvi a, $80
        ora e           ; set S sign bit
_adchlss4:
        sta guest_psw
        ret
_adchlss3:
        ora l
        jnz _adchlss4
        mvi a, $40
        ora e           ; set Z zero bit
        sta guest_psw
        ret
        
        ; sbc hl, ss
        ; hl <- hl - ss - CY
em_ed_sbchlss:
        mov a, m        ; 42:bc 52:de 62:hl 72:sp
        inx h
        shld guest_pc
        cpi $42
        jnz $+9
        lhld guest_bc \ jmp _sbchlss2
        cpi $52
        jnz $+9
        lhld guest_de \ jmp _sbchlss2
        cpi $62
        jnz $+9
        lhld guest_hl \ jmp _sbchlss2
        lhld guest_sp
_sbchlss2:
        xchg
        lhld guest_hl
        ; sbc hl, de
        lda guest_psw
        rar                     ; test c
        jnc $+4
        dcx h                   ; subtract carry in 

        mov a, l
        sub e
        mov l, a
        mov a, h
        sbb d
        mov h, a
        shld guest_hl
        
        ; flags
        push psw
        pop b
        mov a, c
        sta guest_psw
        ret

emu_djnz:
        ; b = b - 1
        ; if b != 0, pc += im8 (signed)
        lda guest_bc+1
        dcr a
        sta guest_bc+1
        jnz emu_jr
        inx h
        inx h
        shld guest_pc
        ret
emu_jr:
        inx h
        xra a
        ora m
        inx h
        jp emu_jr_positive
emu_jr_negative:        
        add l
        mov l, a
        mov a, h
        aci -1
        mov h, a
        shld guest_pc
        ret
emu_jr_positive:
        add l
        mov l, a
        mov a, h
        aci 0
        mov h, a
        shld guest_pc
        ret
emu_jr_nz:
        xchg                            ; de <- guest pc
        lhld guest_psw
        push h
        pop psw                         ; psw <- guest psw
        xchg                            ; hl <- guest pc
        jnz emu_jr                      ; ?cond -> jr
        inx h                           ; else guest pc += 2
        inx h
        shld guest_pc
        ret
emu_jr_nc:
        xchg                            ; de <- guest pc
        lhld guest_psw
        push h
        pop psw                         ; psw <- guest psw
        xchg                            ; hl <- guest pc
        jnc emu_jr                      ; ?cond -> jr
        inx h                           ; else guest pc += 2
        inx h
        shld guest_pc
        ret
emu_jr_z:
        xchg                            ; de <- guest pc
        lhld guest_psw
        push h
        pop psw                         ; psw <- guest psw
        xchg                            ; hl <- guest pc
        jz emu_jr                       ; ?cond -> jr
        inx h                           ; else guest pc += 2
        inx h
        shld guest_pc
        ret
emu_jr_c:
        xchg                            ; de <- guest pc
        lhld guest_psw
        push h
        pop psw                         ; psw <- guest psw
        xchg                            ; hl <- guest pc
        jc emu_jr                       ; ?cond -> jr
        inx h                           ; else guest pc += 2
        inx h
        shld guest_pc
        ret
       
        ; LDI – Load (DE) <- (HL), ++DE, ++HL, --BC 
ed_ldi:
        inx h
        shld guest_pc
        
        lhld guest_de
        xchg
        lhld guest_hl
        ;ldax d
        ;mov m, a
        mov a, m
        stax d
        inx d
        inx h
        shld guest_hl
        xchg
        shld guest_de
        lhld guest_bc
        dcx h
        shld guest_bc
        ret
ed_ldir:
        inx h
        shld guest_pc
        
        lhld guest_bc
        mov b, h
        mov c, l

        lhld guest_de
        xchg
        lhld guest_hl
        
_ldir_loop:        
        ;ldax d \ inx d
        ;mov m, a \ inx h
        mov a, m \ inx h
        stax d \ inx d
        dcx b
        mov a, b
        ora c
        jnz _ldir_loop
        
        shld guest_hl
        xchg
        shld guest_de
        lxi h, 0
        shld guest_bc
        ; flags? N=0, H=0, others unaffected
        ret
ed_cpi:
        jmp $
ed_cpir:
        jmp $
ed_ldd:
        jmp $
ed_lddr:
        jmp $
ed_cpd:
        jmp $
ed_cpdr:
        jmp $

        ; align 256
        .org 0100h + $ & $ff00
bit_value:
        .db 0b00000001
        .db 0b00000010
        .db 0b00000100
        .db 0b00001000
        .db 0b00010000
        .db 0b00100000
        .db 0b01000000
        .db 0b10000000

        ; hl -> opcode
        ; bit number 00nnn000 -> a
getbit_a: 
        mvi a, 0b00111000
        ana m
        rar \ rar \ rar
        mov l, a
        mvi h, bit_value >> 8
        mov a, m
        ret
        
        ; register 00000rrr: 0=b, 1=c, 2=d, 3=e, 4=h, 5=l, 6=m, 7=a
getreg_a:
        mvi a, 0b00000111
        ana m
        ral \ ral       ; ofs = reg * 4 
        mov e, a
        mvi d, 0
        lxi h, getreg_switch
        dad d
        pchl

getreg_switch:
_grsw_0_b:      lda guest_bc+1 \ ret
_grsw_1_c:      lda guest_bc   \ ret
_grsw_2_d:      lda guest_de+1 \ ret
_grsw_3_e:      lda guest_de   \ ret
_grsw_4_h:      lda guest_hl+1 \ ret
_grsw_5_l:      lda guest_hl   \ ret
_grsw_6_m:      jmp _grsw_7_m_real \ nop
_grsw_7_a:      lda guest_psw+1 \ ret

_grsw_7_m_real:
        lhld guest_hl
        mov a, m
        ret
        
setreg_a:
        mov b, a
        mvi a, 0b00000111
        ana m
        ral \ ral       ; ofs = reg * 4
        mov e, a
        mvi d, 0
        lxi h, setreg_switch
        dad d
        mov a, b
        pchl

setreg_switch:
_srsw_0_b:      sta guest_bc+1 \ ret       
_srsw_1_c:      sta guest_bc   \ ret
_srsw_2_d:      sta guest_de+1 \ ret
_srsw_3_e:      sta guest_de   \ ret
_srsw_4_h:      sta guest_hl+1 \ ret
_srsw_5_l:      sta guest_hl   \ ret
_srsw_6_m:      jmp _srsw_7_m_real \ nop
_srsw_7_a:      sta guest_psw+1 \ ret

_srsw_7_m_real:
        lhld guest_hl
        mov m, a
        ret

        ; Z is set if bit = 0
        ; H is set ?
        ; N is reset
emu_bit:
        push h
        push h
        call getreg_a
        pop h
        mov b, a
        call getbit_a
        ana b
        jz _ebt_setz
_ebt_clrz:
        lda guest_psw
        ani ~0x40       ; clear Z
        ori 0x10        ; set H
        sta guest_psw
        pop h
        inx h
        shld guest_pc
        ret
_ebt_setz:
        lda guest_psw
        ori 0x40|0x10   ; set Z+H
        sta guest_psw
        pop h
        inx h
        shld guest_pc
        ret
        
        ; 10bbbrrr 
emu_res:
        push h
        push h
        push h
        call getreg_a
        pop h
        mov b, a
        call getbit_a
        cma
        ana b
        pop h
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
        
        ; 11bbbrrr 
emu_set:
        push h
        push h
        push h
        call getreg_a
        pop h
        mov b, a
        call getbit_a
        ora b
        pop h
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
emu_rlc_r:
        push h
        push h
        call getreg_a
        mov b, a
        lda guest_psw           ; 
        rar                     ; guest carry -> c
        mov a, b
        
        rlc
        mov b, a                ; save result in b
        pop h
        
        lda guest_psw           ; update carry bit in guest psw
        jc $+8
        ani ~(FLAGBIT_C)
        jmp $+5
        ori FLAGBIT_C
        sta guest_psw
        
        mov a, b                ; result
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
emu_rrc_r:
        push h
        push h
        call getreg_a

        mov b, a
        lda guest_psw           ; 
        rar                     ; guest carry -> c
        mov a, b
        
        rrc
        mov b, a                ; save result in b
        pop h
        
        lda guest_psw           ; update carry bit in guest psw
        jc $+8
        ani ~(FLAGBIT_C)
        jmp $+5
        ori FLAGBIT_C
        sta guest_psw
        
        mov a, b                ; result
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
emu_rl_r:
        push h
        push h
        call getreg_a
        mov b, a
        lda guest_psw           ; 
        rar                     ; guest carry -> c
        mov a, b
        ral
        mov b, a                ; save result in b
        pop h
        
        lda guest_psw           ; update carry bit in guest psw
        jc $+8
        ani ~(FLAGBIT_C)
        jmp $+5
        ori FLAGBIT_C
        sta guest_psw
        
        mov a, b                ; result
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
emu_rr_r:
        push h
        push h
        call getreg_a
        mov b, a
        lda guest_psw           ; 
        rar                     ; guest carry -> c
        mov a, b
        rar
        mov b, a                ; save result in b
        pop h
        
        lda guest_psw           ; update carry bit in guest psw
        jc $+8
        ani ~(FLAGBIT_C)
        jmp $+5
        ori FLAGBIT_C
        sta guest_psw
        
        mov a, b                ; result
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
emu_sla_r:
        push h
        push h
        call getreg_a
        ora a
        ral
        mov b, a                ; save result in b
        pop h
        
        lda guest_psw           ; update carry bit in guest psw
        jc $+8
        ani ~(FLAGBIT_C)
        jmp $+5
        ori FLAGBIT_C
        sta guest_psw
        
        mov a, b                ; result
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
emu_sra_r:
        push h
        push h
        call getreg_a
        
        ora a
        jp $+4
        stc
        rar
        
        mov b, a                ; save result in b
        pop h
        
        lda guest_psw           ; update carry bit in guest psw
        jc $+8
        ani ~(FLAGBIT_C)
        jmp $+5
        ori FLAGBIT_C
        sta guest_psw
        
        mov a, b                ; result
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
emu_sll_r:
        jmp $                   ; undocumented
emu_srl_r:
        push h
        push h
        call getreg_a
        
        ora a                   ; C <- 0
        rar
        
        mov b, a                ; save result in b
        pop h
        
        lda guest_psw           ; update carry bit in guest psw
        jc $+8
        ani ~(FLAGBIT_C)
        jmp $+5
        ori FLAGBIT_C
        sta guest_psw
        
        mov a, b                ; result
        call setreg_a
        pop h
        inx h
        shld guest_pc
        ret
        
        
        ;;
        ;; DD PREFIX / IX INSNS
        ;; 
emu_dd_popix:
        inx h
        shld guest_pc
        lhld guest_sp
        mov e, m
        inx h
        mov d, m
        inx h
        shld guest_sp
        xchg
        shld guest_ix
        ret
emu_dd_pushix:
        inx h
        shld guest_pc
        lhld guest_ix
        xchg
        
        lhld guest_sp
        dcx h
        mov m, d
        dcx h
        mov m, e
        shld guest_sp
        ret
        
        ; ex (sp), ix
emu_dd_exspix:
        inx h
        shld guest_pc
        
        lhld guest_ix
        xchg            
        lhld guest_sp   ; de <- ix, hl <- sp
        mov c, m
        mov m, e
        inx h
        mov b, m        ; bc <- (sp), (sp) <- ix
        mov m, d
        mov h, b
        mov l, c        ; hl <- old (sp)
        shld guest_ix   ; ix <- old (sp)
        ret
        
emu_dd_ldspix:
        inx h
        shld guest_pc
        lhld guest_ix
        shld guest_sp
        ret
        
emu_dd_ldixim16:
        inx h
        mov e, m
        inx h
        mov d, m
        inx h
        shld guest_pc
        xchg
        shld guest_ix
        ret
emu_dd_lda16ix:
        inx h
        mov e, m
        inx h
        mov d, m
        inx h
        shld guest_pc
        lhld guest_ix
        xchg
        mov m, e
        inx h
        mov m, d
        ret
emu_dd_incix:
        inx h
        shld guest_pc
        lhld guest_ix
        inx h
        shld guest_ix
        ret
emu_dd_ldixa16:
        inx h
        mov e, m
        inx h
        mov d, m
        inx h
        shld guest_pc
        xchg
        mov e, m
        inx h
        mov d, m
        xchg
        shld guest_ix
        ret
emu_dd_decix:
        inx h
        shld guest_pc
        lhld guest_ix
        dcx h
        shld guest_ix
        ret
emu_dd_addixbc:
        inx h
        shld guest_pc
        lhld guest_bc
        xchg
        lhld guest_ix
        dad d
        shld guest_ix
        push psw
        pop b
        mov a, c
        sta guest_psw
        ret
emu_dd_addixde:
        inx h
        shld guest_pc
        lhld guest_de
        xchg
        lhld guest_ix
        dad d
        shld guest_ix
        push psw
        pop b
        mov a, c
        sta guest_psw
        ret
emu_dd_addixix:
        inx h
        shld guest_pc
        lhld guest_ix
        mov d, h
        mov e, l
        dad d
        shld guest_ix
        push psw
        pop b
        mov a, c
        sta guest_psw
        ret
emu_dd_addixsp:
        inx h
        shld guest_pc
        lhld guest_sp
        xchg
        lhld guest_ix
        dad d
        shld guest_ix
        push psw
        pop b
        mov a, c
        sta guest_psw
        ret


        ; hl = ix + e
        ; clobbers: de
        ; bc untouched
ix_plus_e_hl:
        mvi d, 0
        xra a
        ora e
        jp $+4
        dcr d
        lhld guest_ix
        dad d
        ret
        
        ; DD BE xx  CP (IX+r8) ###
emu_dd_cpixim8:
        inx h
        mov e, m
        inx h
        shld guest_pc 
        call ix_plus_e_hl       ; hl = ix + int8_t(e)
        mov e, m
        lda guest_psw+1         ; a = guest a
        cmp e
        push psw
        pop d                   ; d <- a  e <- psw/f
        mov a, e
        sta guest_psw
        ret        
emu_dd_xorixim8:
        jmp $           ; todo
emu_dd_sbcixim8:
        jmp $           ; todo
emu_dd_adcixim8:
        jmp $           ; todo
emu_dd_orixim8:
        jmp $           ; todo
emu_dd_andixim8:
        jmp $           ; todo
emu_dd_subixim8:
        jmp $           ; todo
emu_dd_addixim8:
        jmp $           ; todo

emu_dd_ldrixim8:
        jmp $
emu_dd_ldixim8r
        jmp $


        ; branch out to the unholy set of bit/set/res (ix + im8)
emu_ddcb:
        inx h
        mvi a, 0b11000111       ; bit n, (ix+im8) : 01nnn110
        ana m
        cpi 0b01000110
        jz ddcb_bitixim8
        cpi 0b10000110          ; res n, (ix+im8): 10nnn110
        jz ddcb_resixim8
        cpi 0b11000110          ; set n, (ix+im8): 11nnn110
        jz ddcb_setixim8
        jmp $                   ; rlc/rl/sla/rrc/rr/sra/srl - sorry

        ; bit n, (ix+im8)
ddcb_bitixim8:
        push h
        inx h
        mov e, m
        inx h
        shld guest_pc 
        call ix_plus_e_hl       ; hl = ix + int8_t(e)
        mov b, m                ; e <- mem[ix+im8]

        pop h
        call getbit_a
        ana b
        push psw
        pop b                   ; b <- a, c <- f
        mov a, c
        sta guest_psw
        ret
        
        ; res n, (ix+im8)
ddcb_resixim8:
        push h
          inx h
          mov e, m
          inx h
          shld guest_pc 
        pop h
        call getbit_a
        cma
        mov b, a
        
        call ix_plus_e_hl       ; hl = ix + int8_t(e)
        mov a, m                ; a <- mem[ix+im8]

        ana b                   ; a = a & ~bitval
        mov m, a
        ret
        
        ; set n, (ix+im8)
ddcb_setixim8:
        push h
          inx h
          mov e, m
          inx h
          shld guest_pc 
        pop h
        call getbit_a
        mov b, a
        
        call ix_plus_e_hl       ; hl = ix + int8_t(e)
        mov a, m                ; a <- mem[ix+im8]

        ora b                   ; a = a & ~bitval
        mov m, a
        ret
        
        ; opcode/break table
        ; 0 = through
        ; 1 = break -> single step
        ; 2 = break -> emulate
        ; align 256
        .org traptab_org
traptab:        
        ; instruction size, 0 = emt
        ; sign bit = branch
        .db 1 ; $00 	nop	        nop
        .db 3 ; $01 	ld bc,00000	lxi	b,X0000
        .db 1 ; $02 	ld (bc),a       stax	b
        .db 1 ; $03 	inc bc		inx	b
        .db 1 ; $04 	inc b		inr	b
        .db 1 ; $05 	dec b		dcr	b
        .db 2 ; $06 	ld b,000h	mvi	b,0
        .db 1 ; $07 	rlca		rlc
        .db 0 ; $08 	ex af,af'       ####
        .db 1 ; $09 	add hl,bc	dad	b
        .db 1 ; $0a 	ld a,(bc)	ldax	b
        .db 1 ; $0b 	dec bc		dcx	b
        .db 1 ; $0c 	inc c		inr	c
        .db 1 ; $0d 	dec c		dcr	c
        .db 2 ; $0e 	ld c,000h	mvi	c,0
        .db 1 ; $0f 	rrca		rrc
        .db 0 ; $10 	djnz $+2	#######################
        .db 3 ; $11 	ld de,00000	lxi	d,X0000
        .db 1 ; $12 	ld (de),a	stax	d
        .db 1 ; $13 	inc de		inx	d
        .db 1 ; $14 	inc d		inr	d
        .db 1 ; $15 	dec d		dcr	d
        .db 2 ; $16 	ld d,000h	mvi	d,0
        .db 1 ; $17 	rla		ral
        .db 0 ; $18 	jr $+2	        #######################
        .db 1 ; $19 	add hl,de	dad	d
        .db 1 ; $1a 	ld a,(de)	ldax	d
        .db 1 ; $1b 	dec de		dcx	d
        .db 1 ; $1c 	inc e		inr	e
        .db 1 ; $1d 	dec e		dcr	e
        .db 2 ; $1e 	ld e,000h	mvi	e,0
        .db 1 ; $1f 	rra		rar
        .db 0 ; $20 	jr nz,$+2	#######################
        .db 3 ; $21 	ld hl,00000	lxi	h,X0000
        .db 3 ; $22 	ld (00000h),hl	shld	X0000
        .db 1 ; $23 	inc hl		inx	h
        .db 1 ; $24 	inc h		inr	h
        .db 1 ; $25 	dec h		dcr	h
        .db 2 ; $26 	ld h,000h	mvi	h,0
        .db 1 ; $27 	daa		daa
        .db 0 ; $28 	jr z,$+2	#######################
        .db 1 ; $29 	add hl,hl	dad	h
        .db 3 ; $2a 	ld hl,(00000h)	lhld	X0000
        .db 1 ; $2b 	dec hl		dcx	h
        .db 1 ; $2c 	inc l		inr	l
        .db 1 ; $2d 	dec l		dcr	l
        .db 2 ; $2e 	ld l,000h	mvi	l,0
        .db 1 ; $2f 	cpl		cma
        .db 0 ; $30 	jr nc,$+2	#######################
        .db 3 ; $31 	ld sp,0000	lxi	sp,X0000
        .db 3 ; $32 	ld (00000h),a   sta	X0000
        .db 1 ; $33 	inc sp		inx	sp
        .db 1 ; $34 	inc (hl)	inr	m
        .db 1 ; $35 	dec (hl)	dcr	m
        .db 2 ; $36 	ld (hl),000h	mvi	m,0
        .db 1 ; $37 	scf		stc
        .db 0 ; $38 	jr c,$+2	#######################
        .db 1 ; $39 	add hl,sp	dad	sp
        .db 3 ; $3a 	ld a,(00000h)	lda	X0000
        .db 1 ; $3b 	dec sp		dcx	sp
        .db 1 ; $3c 	inc a		inr	a
        .db 1 ; $3d 	dec a		dcr	a
        .db 2 ; $3e 	ld a,000h	mvi	a,0
        .db 1 ; $3f 	ccf		cmc
        .db 1 ; $40 	ld b,b		mov	b,b
        .db 1 ; $41 	ld b,c		mov	b,c
        .db 1 ; $42 	ld b,d		mov	b,d
        .db 1 ; $43 	ld b,e		mov	b,e
        .db 1 ; $44 	ld b,h		mov	b,h
        .db 1 ; $45 	ld b,l		mov	b,l
        .db 1 ; $46 	ld b,(hl)	mov	b,m
        .db 1 ; $47 	ld b,a		mov	b,a
        .db 1 ; $48 	ld c,b		mov	c,b
        .db 1 ; $49 	ld c,c		mov	c,c
        .db 1 ; $4a 	ld c,d		mov	c,d
        .db 1 ; $4b 	ld c,e		mov	c,e
        .db 1 ; $4c 	ld c,h		mov	c,h
        .db 1 ; $4d 	ld c,l		mov	c,l
        .db 1 ; $4e 	ld c,(hl)	mov	c,m
        .db 1 ; $4f 	ld c,a		mov	c,a
        .db 1 ; $50 	ld d,b		mov	d,b
        .db 1 ; $51 	ld d,c		mov	d,c
        .db 1 ; $52 	ld d,d		mov	d,d
        .db 1 ; $53 	ld d,e		mov	d,e
        .db 1 ; $54 	ld d,h		mov	d,h
        .db 1 ; $55 	ld d,l		mov	d,l
        .db 1 ; $56 	ld d,(hl)	mov	d,m
        .db 1 ; $57 	ld d,a		mov	d,a
        .db 1 ; $58 	ld e,b		mov	e,b
        .db 1 ; $59 	ld e,c		mov	e,c
        .db 1 ; $5a 	ld e,d		mov	e,d
        .db 1 ; $5b 	ld e,e		mov	e,e
        .db 1 ; $5c 	ld e,h		mov	e,h
        .db 1 ; $5d 	ld e,l		mov	e,l
        .db 1 ; $5e 	ld e,(hl)	mov	e,m
        .db 1 ; $5f 	ld e,a		mov	e,a
        .db 1 ; $60 	ld h,b		mov	h,b
        .db 1 ; $61 	ld h,c		mov	h,c
        .db 1 ; $62 	ld h,d		mov	h,d
        .db 1 ; $63 	ld h,e		mov	h,e
        .db 1 ; $64 	ld h,h		mov	h,h
        .db 1 ; $65 	ld h,l		mov	h,l
        .db 1 ; $66 	ld h,(hl)	mov	h,m
        .db 1 ; $67 	ld h,a		mov	h,a
        .db 1 ; $68 	ld l,b		mov	l,b
        .db 1 ; $69 	ld l,c		mov	l,c
        .db 1 ; $6a 	ld l,d		mov	l,d
        .db 1 ; $6b 	ld l,e		mov	l,e
        .db 1 ; $6c 	ld l,h		mov	l,h
        .db 1 ; $6d 	ld l,l		mov	l,l
        .db 1 ; $6e 	ld l,(hl)	mov	l,m
        .db 1 ; $6f 	ld l,a		mov	l,a
        .db 1 ; $70 	ld (hl),b	mov	m,b
        .db 1 ; $71 	ld (hl),c	mov	m,c
        .db 1 ; $72 	ld (hl),d	mov	m,d
        .db 1 ; $73 	ld (hl),e	mov	m,e
        .db 1 ; $74 	ld (hl),h	mov	m,h
        .db 1 ; $75 	ld (hl),l	mov	m,l
        .db 1 ; $76 	halt		hlt
        .db 1 ; $77 	ld (hl),a	mov	m,a
        .db 1 ; $78 	ld a,b		mov	a,b
        .db 1 ; $79 	ld a,c		mov	a,c
        .db 1 ; $7a 	ld a,d		mov	a,d
        .db 1 ; $7b 	ld a,e		mov	a,e
        .db 1 ; $7c 	ld a,h		mov	a,h
        .db 1 ; $7d 	ld a,l		mov	a,l
        .db 1 ; $7e 	ld a,(hl)	mov	a,m
        .db 1 ; $7f 	ld a,a		mov	a,a
        .db 1 ; $80 	add a,b		add	b
        .db 1 ; $81 	add a,c		add	c
        .db 1 ; $82 	add a,d		add	d
        .db 1 ; $83 	add a,e		add	e
        .db 1 ; $84 	add a,h		add	h
        .db 1 ; $85 	add a,l		add	l
        .db 1 ; $86 	add a,(hl)	add	m
        .db 1 ; $87 	add a,a		add	a
        .db 1 ; $88 	adc a,b		adc	b
        .db 1 ; $89 	adc a,c		adc	c
        .db 1 ; $8a 	adc a,d		adc	d
        .db 1 ; $8b 	adc a,e		adc	e
        .db 1 ; $8c 	adc a,h		adc	h
        .db 1 ; $8d 	adc a,l		adc	l
        .db 1 ; $8e 	adc a,(hl)	adc	m
        .db 1 ; $8f 	adc a,a		adc	a
        .db 1 ; $90 	sub b		sub	b
        .db 1 ; $91 	sub c		sub	c
        .db 1 ; $92 	sub d		sub	d
        .db 1 ; $93 	sub e		sub	e
        .db 1 ; $94 	sub h		sub	h
        .db 1 ; $95 	sub l		sub	l
        .db 1 ; $96 	sub (hl)	sub	m
        .db 1 ; $97 	sub a		sub	a
        .db 1 ; $98 	sbc a,b		sbb	b
        .db 1 ; $99 	sbc a,c		sbb	c
        .db 1 ; $9a 	sbc a,d		sbb	d
        .db 1 ; $9b 	sbc a,e		sbb	e
        .db 1 ; $9c 	sbc a,h		sbb	h
        .db 1 ; $9d 	sbc a,l		sbb	l
        .db 1 ; $9e 	sbc a,(hl)	sbb	m
        .db 1 ; $9f 	sbc a,a		sbb	a
        .db 1 ; $a0 	and b		ana	b
        .db 1 ; $a1 	and c		ana	c
        .db 1 ; $a2 	and d		ana	d
        .db 1 ; $a3 	and e		ana	e
        .db 1 ; $a4 	and h		ana	h
        .db 1 ; $a5 	and l		ana	l
        .db 1 ; $a6 	and (hl)	ana	m
        .db 1 ; $a7 	and a		ana	a
        .db 1 ; $a8 	xor b		xra	b
        .db 1 ; $a9 	xor c		xra	c
        .db 1 ; $aa 	xor d		xra	d
        .db 1 ; $ab 	xor e		xra	e
        .db 1 ; $ac 	xor h		xra	h
        .db 1 ; $ad 	xor l		xra	l
        .db 1 ; $ae 	xor (hl)	xra	m
        .db 1 ; $af 	xor a		xra	a
        .db 1 ; $b0 	or b		ora	b
        .db 1 ; $b1 	or c		ora	c
        .db 1 ; $b2 	or d		ora	d
        .db 1 ; $b3 	or e		ora	e
        .db 1 ; $b4 	or h		ora	h
        .db 1 ; $b5 	or l		ora	l
        .db 1 ; $b6 	or (hl)		ora	m
        .db 1 ; $b7 	or a		ora	a
        .db 1 ; $b8 	cp b		cmp	b
        .db 1 ; $b9 	cp c		cmp	c
        .db 1 ; $ba 	cp d		cmp	d
        .db 1 ; $bb 	cp e		cmp	e
        .db 1 ; $bc 	cp h		cmp	h
        .db 1 ; $bd 	cp l		cmp	l
        .db 1 ; $be 	cp (hl)		cmp	m
        .db 1 ; $bf 	cp a		cmp	a
        .db $81 ; $c0 	ret nz		rnz
        .db 1 ; $c1 	pop bc		pop	b
        .db $83 ; $c2 	jp nz,00000h	jnz	X0000
        .db $83 ; $c3 	jp 00000h       jmp	X0000
        .db $83 ; $c4 	call nz,00000h	cnz	X0000
        .db 1 ; $c5 	push bc		push	b
        .db 2 ; $c6 	add a,000h	adi	0
        .db $81 ; $c7 	rst 0		rst	0
        .db $81 ; $c8 	ret z		rz
        .db $81 ; $c9 	ret		ret
        .db $83 ; $ca 	jp z,00000h	jz	X0000
        .db 0 ; $cb 	rlc b	        #######################
        .db $83 ; $cc 	call z,00000h   cz	X0000
        .db $83 ; $cd 	call 00000h	call	X0000
        .db 2 ; $ce 	adc a,000h	aci	0
        .db $81 ; $cf 	rst 8		rst	1
        .db $81 ; $d0 	ret nc		rnc
        .db 1 ; $d1 	pop de		pop	d
        .db $83 ; $d2 	jp nc,00000h	jnc	X0000
        .db 2 ; $d3 	out (000h),a	out	0
        .db $83 ; $d4 	call nc,00000h	cnc	X0000
        .db 1 ; $d5 	push de		push	d
        .db 2 ; $d6 	sub 000h	sui	0
        .db $81 ; $d7 	rst 10h		rst	2
        .db $81 ; $d8 	ret c		rc
        .db 0 ; $d9 	exx		#######################
        .db $83 ; $da 	jp c,00000h     jc	X0000
        .db 2 ; $db 	in a,(000h)	in	0
        .db $83 ; $dc 	call c,00000h   cc	X0000
        .db 0 ; $dd 	defb 0ddh	#######################
        .db 2 ; $de 	sbc a,000h	sbi	0
        .db $81 ; $df 	rst 18h		rst	3
        .db $81 ; $e0 	ret po		rpo
        .db 1 ; $e1 	pop hl		pop	h
        .db $83 ; $e2 	jp po,00000     jpo	X0000
        .db 1 ; $e3 	ex (sp),hl	xthl
        .db $83 ; $e4 	call po,00000h  cpo	X0000
        .db 1 ; $e5 	push hl		push	h
        .db 2 ; $e6 	and 000h	ani	0
        .db $81 ; $e7 	rst 20h		rst	4
        .db $81 ; $e8 	ret pe		rpe
        .db $81 ; $e9 	jp (hl)		pchl #### emulated because outcomes unknown beforehand
        .db $83 ; $ea 	jp pe,00000h	jpe	X0000
        .db 1 ; $eb 	ex de,hl        xchg
        .db $83 ; $ec 	call pe,00000h	cpe	X0000
        .db 0 ; $ed 	defb 0edh       #######################
        .db 2 ; $ee 	xor 000h	xri	0
        .db $81 ; $ef 	rst 28h		rst	5
        .db $81 ; $f0 	ret p		rp
        .db 1 ; $f1 	pop af		pop	psw
        .db $83 ; $f2 	jp p,00000	jp	X0000
        .db 1 ; $f3 	di		di
        .db $83 ; $f4 	call p,00000h	cp	X0000
        .db 1 ; $f5 	push af		push	psw
        .db 2 ; $f6 	or 000h	        ori	0
        .db $81 ; $f7 	rst 30h		rst	6
        .db $81 ; $f8 	ret m		rm
        .db $1 ; $f9 	ld sp,hl	sphl
        .db $83 ; $fa 	jp m,00000      jm	X0000
        .db 1 ; $fb 	ei		ei
        .db $83 ; $fc 	call m,00000h   cm	X0000
        .db 0 ; $fd 	defb 0fdh       #######################
        .db 2 ; $fe 	cp 000h	        cpi	0
        .db $81 ; $ff 	rst 38h	
