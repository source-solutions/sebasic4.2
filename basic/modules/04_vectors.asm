;	// SE Basic IV 4.2 Cordelia
;	// Copyright (c) 1999-2022 Source Solutions, Inc.

;	// SE Basic IV is free software: you can redistribute it and/or modify
;	// it under the terms of the GNU General Public License as published by
;	// the Free Software Foundation, either version 3 of the License, or
;	// (at your option) any later version.
;	// 
;	// SE Basic IV is distributed in the hope that it will be useful,
;	// but WITHOUT ANY WARRANTY; without even the implied warranty o;
;	// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;	// GNU General Public License for more details.
;	// 
;	// You should have received a copy of the GNU General Public License
;	// along with SE Basic IV. If not, see <http://www.gnu.org/licenses/>.

;;
;	// --- VECTOR TABLE ---------------------------------------------------------
;;
:

;	// PC must not hit $04c6 (load trap) or $0562 (save trap)

;	org $04c6;
;load_trap:
;	ret;								// must never call this address

;	org $0562;
;save_trap:
;	ret;								// must never call this address

	org $04c4;

;;
; open a file for appending if it exists
; @param IX - pointer to ASCIIZ file path
; @throws sets carry flag on error
;;
SEFileAppend:
	jp v_open_w_append;					// $00

;;
; close a file
; @param A - file handle
; @throws sets carry flag on error
;;
SEFileClose:
	jp v_close;							// $01

;;
; create a file for writing or open a file for writing if it exists
; @param IX - pointer to ASCIIZ file path
; @throws sets carry flag on error
;;
SEFileCreate:
	jp v_open_w_create;					// $02

;;
; load a file from disk to memory
; @param HL - destination address
; @param IX - pointer to ASCIIZ file path
; @throws sets carry flag on error
;;
SEFileLoad:
	jp v_load;							// $03

;;
; open a file from disk
; @param IX - pointer to ASCIIZ file path
; @return file handle in <code>A</code>
; @throws sets carry flag on error
;;
SEFileOpen:
	jp v_open;							// $04

;;
; open a file from disk for reading if it exists
; @param IX - pointer to ASCIIZ file path
; @return file handle in <code>A</code>
; @throws sets carry flag on error
;;
SEFileOpenExists:
	jp v_open_r_exists;					// $05

;;
; read bytes from a file to memory
; @param A - file handle
; @param BC - byte count
; @param IX - destination in memory
; @throws sets carry flag on error
;;
SEFileRead:
	jp v_read;							// $06

;;
; read one byte from a file to memory
; @param A - file handle
; @param IX - destination in memory
; @throws sets carry flag on error
;;
SEFileReadOne:
	jp v_read_one;						// $07

;;
; remove a file
; @param IX - pointer to ASCIIZ file path
; @throws sets carry flag on error
;;
SEFileRemove:
	jp v_delete;						// $08

;;
; rename a file or folder
; @param DE - pointer to new ASCIIZ path
; @param IX - pointer to old ASCIIZ path
; @throws sets carry flag on error
;;
SEFileRename:
	jp v_rename;						// $09

;;
; save a file from memory to disk
; @param BC - byte count
; @param HL - source address
; @param IX - pointer to ASCIIZ file path
; @throws sets carry flag on error
;;
SEFileSave:
	jp v_save;							// $0a

;;
; write bytes from memory to a file
; @param A - file handle
; @param BC - byte count
; @param IX - source in memory
; @throws sets carry flag on error
;;
SEFileWrite:
	jp v_write;							// $0b

;;
; write one byte from memory to a file
; @param A - file handle
; @param IX - source in memory
; @throws sets carry flag on error
;;
SEFileWriteOne:
	jp v_write_one;						// $0c

;;
; create a folder
; @param IX - pointer to ASCIIZ folder path
; @throws sets carry flag on error
;;
SEFolderCreate:
	jp v_mkdir;							// $0d

;;
; open a folder for reading
; @param IX - pointer to ASCIIZ folder path
; @return folder handle in <code>A</code>
; @throws sets carry flag on error
;;
SEFolderOpen:
	jp v_opendir;						// $0e

;;
; read a folder entry
; @param A - folder handle
; @param IX - destination memory address
; @throws sets carry flag on error
;;
SEFolderRead:
	jp v_readdir;						// $0f

;;
; remove a folder
; @param IX - pointer to ASCIIZ folder path
; @throws sets carry flag on error
;;
SEFolderRemove:
	jp v_rmdir;							// $10

;;
; set current working folder
; @param IX - pointer to ASCIIZ folder path
; @throws sets carry flag on error
;;
SEFolderSet:
	jp v_chdir;							// $11


; $12
	jp $ffff;							// 

; $13
	jp $ffff;							// 

; $14
	jp $ffff;							// 

; $15
	jp $ffff;							// 

; $16
	jp $ffff;							// 

; $17
	jp $ffff;							// 

; $18
	jp $ffff;							//

; $19
	jp $ffff;							// 

; $1a
	jp $ffff;							// 

; $1b
	jp $ffff;							// 

; $1c
	jp $ffff;							// 

; $1d
	jp $ffff;							// 

; $1e
	jp $ffff;							//

; $1f
	jp $ffff;							// 

; $20
	jp $ffff;							// 

; $21
	jp $ffff;							// 

; $22
	jp $ffff;							// 

; $23
	jp $ffff;							// 

; $24
	jp $ffff;							// 

; $25
	jp $ffff;							//

; $26
	jp $ffff;							// 

; $27
	jp $ffff;							// 

; $28
	jp $ffff;							// 

; $29
	jp $ffff;							// 

; $2a
	jp $ffff;							// 

; $2b
	jp $ffff;							// 

; $2c
	jp $ffff;							// 

; $2d
	jp $ffff;							// 

; $2e
	jp $ffff;							// 

; $2f
	jp $ffff;							// 

; $30
	jp $ffff;							// 

; $31
	jp $ffff;							// 

; $32
	jp $ffff;							// 

; $33
	jp $ffff;							// 

; $34
	jp $c900;							// ($0562 must never be called)

;	// permits extension of vector table if required

; ;;
; ; set RAM in top 16K
; ; @param A - bank (0 to 8)
; ;;
; SEMemorySet:
; 	jp v_mem;							// 

; ;;
; ; clear the screen
; ;;
; SEScreenClear:
; 	jp c_cls;							// 

; ;;
; ; set the screen mode
; ; @param A - mode
; ;;
; SEScreenModeSet:
; 	jp v_scr_mode;						//

; ;;
; ; set the palette
; ; @param IX - pointer to 64 bytes of palette data
; ;;
; SEScreenPaletteSet:
; 	jp v_pal_set;						//

; ;;
; ; print an ASCIIZ string to the main display
; ; @param IX - pointer to ASCIIZ string
; ;;
; SEScreenPrintString:
; 	jp v_pr_string;						// 

; ;;
; ; print an ASCIIZ string to the lower display
; ; @param IX - pointer to ASCIIZ string
; ;;
; SEScreenPrintStringLower:
; 	jp v_pr_string_lo;					// 
