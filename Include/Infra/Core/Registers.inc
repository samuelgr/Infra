;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Infra
;   Common infrastructure used by other projects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Authored by Samuel Grossman
; Copyright (c) 2016-2026
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Registers.inc
;   Size-dependent name mappings for registers. Helps with 32/64-bit portability.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IFNDEF __INFRA_REGISTERS_INC
__INFRA_REGISTERS_INC EQU 1


; The mappings that follow create register names with 's' as a prefix. Each mapping expands to
; the corresponding register at the largest-available size. Size depends on whether assembling
; for a 32-bit or a 64-bit target binary. Only registers supported in both modes are included.

IFDEF _WIN32
; 32-bit mappings
sax TEXTEQU <eax>
sbx TEXTEQU <ebx>
scx TEXTEQU <ecx>
sdx TEXTEQU <edx>
ssi TEXTEQU <esi>
sdi TEXTEQU <edi>
sbp TEXTEQU <ebp>
ssp TEXTEQU <esp>
ENDIF

IFDEF _WIN64
; 64-bit mappings
sax TEXTEQU <rax>
sbx TEXTEQU <rbx>
scx TEXTEQU <rcx>
sdx TEXTEQU <rdx>
ssi TEXTEQU <rsi>
sdi TEXTEQU <rdi>
sbp TEXTEQU <rbp>
ssp TEXTEQU <rsp>
ENDIF


ENDIF ; __INFRA_REGISTERS_INC
