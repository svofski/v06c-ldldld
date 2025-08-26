      .org $100
      
      mvi c, 9
      lxi d, gamarjoba
      call 5
      lxi h, data_start
      lxi b, BASE
      call dzx0
      jmp BASE
gamarjoba:
      .db "LD LD,(LD) for Vector-06C by svofski 2025", $0d, $0a
      .db "Z80 compatibility layer for 8080 CPU", $0d, $0a, $0d, $0a, "$"
      .include "dzx0.asm"
data_start:
