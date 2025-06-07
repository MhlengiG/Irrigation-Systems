;-------------------------------------------------------------------------------
;			CODE BY: MHLENGI GIGABA
;			DATE: 16/10/2024
;  DESCRIPTION: FINAL CODE
;-------------------------------------------------------------------------------
    
;-------------------------------------------------------------------------------
; LIST OF SUBROUTINES:
;   
; CONFIGURATION: 
;	AN0 AS ANALOGUE INPUT PIN FOR ADC
;	AN4 AS OUTPUT PIN FOR E LCD
;	AN5 AS OUTPUT PIN FOR RS LCD
;	PORTC AS AN OUTPUT FOR LCD DATA
;	INITIALIZE LCD
; MAIN:
;	
;	UPDATE LCD: IF NEEDED
;
; LCD RELATED SUBROUTINES:
;	INIT_LCD
;	LCD_CMD
;	LCD_DATA
;
; ADC RELATED SUBROUTINES:
;	ADC_PROCESS
;	COMPARE
;
; GENERAL SUBROUTINES:
;	BIN2BCD
;	DELAY100ms
;	DELAY1ms
;	DELAY_ACQUISTION
;-------------------------------------------------------------------------------
      
#include "p16F690.inc"
 __CONFIG _CP_OFF & _CPD_OFF & _BOR_OFF & _MCLRE_ON & _WDT_OFF & _PWRTE_ON & _INTRC_OSC_NOCLKOUT & _FCMEN_OFF & _IESO_OFF 

 errorlevel -302 ; SUPRESS BANK CHANGE WARNINGS
 
;-----------------------------VARIABLES-----------------------------------------
D0             EQU 0x30  
D1             EQU 0x31
D2             EQU 0x32
D3             EQU 0x33
DIVIDEND_H     EQU 0x34  
DIVIDEND_L     EQU 0x35
DIVISOR_H      EQU 0x36
DIVISOR_L      EQU 0x37
MULTIPLIER     EQU 0x38
R0             EQU 0x39
R1             EQU 0x3A
R2             EQU 0x3B
TEMP1          EQU 0x3C
TEMP2          EQU 0x3D
REM0           EQU 0x3E
REM1           EQU 0x3F
LOOPCOUNT      EQU 0x40
LCDTEMP        EQU 0x41  
TEMP1_CONVERT  EQU 0x42
TEMP2_CONVERT  EQU 0x43
DELAYTEMPVAR   EQU 0x44
COUNTER_CONVERT EQU 0x45
TEMPVALUE      EQU 0x46
HUNDREDS       EQU 0x47
TENS           EQU 0x48
COUNT          EQU 0x49
UNITS          EQU 0x4A
THOUSANDS      EQU 0x4B
ADC_H          EQU 0x4C
ADC_L          EQU 0x4D
UPDATE_FLAG    EQU 0x4E
W_SAVE	       EQU 0x4F
STATUS_SAVE    EQU 0x50
RELAY_FLAG     EQU 0x51
;-------------------------------RESET VECTOR------------------------------------
    ORG 0x00
    GOTO INIT
;--------------------------------INTERRUPTS-------------------------------------
    ORG 0x04

ADC_PROCESS:
    
    MOVWF W_SAVE		; CONTEXT SAVING OF WREG AND STATUS REG
    MOVFW STATUS
    MOVWF STATUS_SAVE
    
    MOVFW ADRESH
    ANDLW 0x02
    MOVWF ADC_H

    BANKSEL ADRESL
    MOVFW ADRESL
    BANKSEL ADCON0
    MOVWF ADC_L
    
    CALL COMPARE		; COMPARE ADC WITH THRESHOLD
    BSF UPDATE_FLAG, 0		; SET UPDATE FLAG
    
    ; CONTEXT RESTORE OF WREG AND STATUS REG
    MOVFW STATUS_SAVE
    MOVWF STATUS
    MOVWF W_SAVE
    
    RETFIE
     
;--------------------------CONFIGURATION SECTION--------------------------------

INIT:
    BANKSEL TRISA	    ; BANK1
    BSF TRISA, 0	    ; AN0 PIN AS INPUT
    BCF TRISA, 4
    BCF TRISA, 5
    BCF TRISB, 4	    ; RELAY CONNECTION
    BANKSEL PORTC
    CLRF PORTC
    CLRF PORTA
    
ADC_INIT:
    CLRF ANSEL		    ; ALL PINS DIGITAL
    BSF ANSEL, 0	    ; RA0 AS ANALOGUE PIN
    CLRF ADCON1		    ; FOSC/ 2
    BANKSEL ANSELH	    ; BANK0
    CLRF ANSELH		    ; ALL PINS DIGITAL
    BANKSEL ADCON0	    ; BANK0
    MOVLW B'10000001'	    ; RIGHT, VDD AS VREF, AN0 AS ANALOGUE
    MOVWF ADCON0
    
ADC_INT_INIT:
    BCF PIR1, ADIF	    ; CLEAR INTERRUPT FLAG
    BANKSEL PIE1	    ; BANK1
    BSF PIE1, ADIE	    ; ENABLE ADC INTERRUPT
    BANKSEL ADCON0	    ; BANK0
    CALL DELAY1ms	    ; AQUISITION DELAY
    BSF INTCON, GIE	    ; GLOBAL INTERRUPT ENABLE
    
    
; ALL IN BANK 0
    CLRF ADC_H		    ; CLEAR ADC RESULTS
    CLRF ADC_L
    CLRF UPDATE_FLAG
    CLRF RELAY_FLAG
    CALL INIT_LCD
    GOTO MAIN
;-------------------------------MAIN--------------------------------------------   
MAIN:
    BTFSC UPDATE_FLAG, 0	; UPDATE LCD IF ADC DONE
    CALL UPDATE
    
    GOTO MAIN

;------------------------------LCD RELATED SUBROUTINES--------------------------
    
INIT_LCD:
    BCF INTCON, GIE	; CLEAR TO ALLOW FOR SUBROUTINE
    CALL DELAY100ms	;LCD START UP DELAY
    
    MOVLW 0x03		;Special Sequence
    CALL LCD_CMD
    CALL DELAY100ms
    
    MOVLW 0x03		;Special Sequence
    CALL LCD_CMD
    CALL DELAY100ms
    
    MOVLW 0x03		;Special Sequence
    CALL LCD_CMD
    
    MOVLW 0x02		;Special Sequence 4-BIT MODE
    CALL LCD_CMD
    
    MOVLW 0x28
    CALL LCD_CMD
    
    MOVLW 0x0C
    CALL LCD_CMD
    
    MOVLW 0x01
    CALL LCD_CMD
    CALL DELAY1ms
    CALL DELAY1ms
    
    MOVLW 0x06		;SETING ENTRY MODE
    CALL LCD_CMD 
    
    MOVLW 0x80		;SET CURSOR AT BEGINNING
    CALL LCD_CMD 
    
    BSF INTCON, GIE	; RE- ENABLE ADC INTERRUPTS
    CALL START_MSG	;CALL START MSG
    
    RETURN
;------------------------------------------- 
LCD_CMD:
    ;High Nibble
    MOVWF LCDTEMP   
    ANDLW 0XF0
    MOVWF PORTC
    BCF PORTA,5		;Clear RS (Set Command Register)
    BSF PORTA,4		;Toggle Enable
    CALL DELAY1ms	;Enable Delay
    BCF PORTA,4
    CALL DELAY1ms	;Del
    
    ;Low Nibble
    SWAPF LCDTEMP,1
    MOVF LCDTEMP,0
    ANDLW 0xF0
    MOVWF PORTC
    BCF PORTA,5		;Clear RS (Set Command Register)
    BSF PORTA,4		;Toggle Enable
    CALL DELAY1ms	;Enable Delay
    BCF PORTA,4
    CALL DELAY1ms
    RETURN
;--------------------------------------------    
LCD_DATA:
    ;High Nibble
    MOVWF LCDTEMP   
    ANDLW 0XF0
    MOVWF PORTC
    BSF PORTA,5		;Set RS (Set Data Register)
    BSF PORTA,4		;Toggle Enable
    CALL DELAY1ms	;Enable Delay
    BCF PORTA,4
    CALL DELAY1ms	;Delay for 4 bit mode
    
    ;Low Nibble
    SWAPF LCDTEMP,1
    MOVF LCDTEMP,0
    ANDLW 0xF0
    MOVWF PORTC
    BSF PORTA,5		;Set RS (Set Data Register)
    BSF PORTA,4		;Toggle Enable
    CALL DELAY1ms	;Enable Delay
    BCF PORTA,4
    CALL DELAY1ms
    RETURN
;-----------------------------------------    
START_MSG:
    MOVLW 0x01        ; Clear Screen
    CALL LCD_CMD 
    MOVLW 0x80        ; Set Cursor to beginning of first line
    CALL LCD_CMD 
    
    MOVLW 'M'
    CALL LCD_DATA
    MOVLW 'O'
    CALL LCD_DATA
    MOVLW 'I'
    CALL LCD_DATA
    MOVLW 'S'
    CALL LCD_DATA
    MOVLW 'T'
    CALL LCD_DATA
    MOVLW 'U'
    CALL LCD_DATA
    MOVLW 'R'
    CALL LCD_DATA
    MOVLW 'E'
    CALL LCD_DATA  
    MOVLW ':'
    CALL LCD_DATA
    MOVLW ' '
    CALL LCD_DATA
    
    MOVLW 0xC0		; MOVE CURSOR TO ROW 2
    CALL LCD_CMD
    
    MOVLW 'R'
    CALL LCD_DATA
    MOVLW 'E'
    CALL LCD_DATA
    MOVLW 'L'
    CALL LCD_DATA
    MOVLW 'A'
    CALL LCD_DATA
    MOVLW 'Y'
    CALL LCD_DATA
    MOVLW ':'
    CALL LCD_DATA
    
    MOVLW ' '
    CALL LCD_DATA
    MOVLW ' '
    CALL LCD_DATA
    MOVLW ' '
    CALL LCD_DATA
    
    MOVLW 'A'
    CALL LCD_DATA  
    MOVLW 'D'
    CALL LCD_DATA
    MOVLW 'C'
    CALL LCD_DATA
    MOVLW ':'
    CALL LCD_DATA
    
    RETURN   
;------------------------------------------ 
UPDATE:
    BCF INTCON, GIE
; UPDATE ADC:  
    MOVLW 0xCC        ; Set Cursor to beginning of first line
    CALL LCD_CMD
    CALL BIN2BCD
    
    MOVFW THOUSANDS     ; Display hundreds digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW HUNDREDS     ; Display hundreds digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW TENS         ; Display tens digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW UNITS        ; Display units digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA      
    
;   UPDATE RELAY
    MOVLW 0xC6
    CALL LCD_CMD
    
    MOVLW '0'
    BTFSC RELAY_FLAG, 0
    MOVLW '1'
    CALL LCD_DATA
    
;   UPDATE MOISTURE
    MOVLW 0x8A
    CALL LCD_CMD
    
    CALL SUBTRACTADC
    CALL MULTIPLY100
    CALL DIV580
    
    CALL BIN2BCD
    
    MOVFW HUNDREDS
    ADDLW 0x30
    CALL LCD_DATA
    MOVFW TENS         ; Display tens digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW UNITS        ; Display units digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    
    MOVLW '%'
    CALL LCD_DATA
    
    BSF INTCON, GIE	; RE-ENABLE INTERRUPTS
    RETURN

;----------------------------Delays-------------------------------------------
; Delay functions remain unchanged (DELAY1ms and DELAY100ms)
DELAY1ms 
    MOVLW 0xFA ; 250 is the initial value
Loop1ms 
    ADDLW 0xFF ; Dec WREG
    BTFSS STATUS,Z ; Checks if zero flag set
    GOTO Loop1ms ; If false, keep looking
    RETURN

DELAY100ms
    MOVLW 0x64
    MOVWF DELAYTEMPVAR
loopDELAY100ms
    CALL DELAY1ms
    DECFSZ DELAYTEMPVAR,1
    GOTO loopDELAY100ms
    RETURN

;----------------------------Binary to BCD Conversion-------------------------
BIN2BCD:
    MOVFW ADC_L
    MOVWF TEMP1
    MOVFW ADC_H
    MOVWF TEMP2
    
    MOVF    TEMP2,W
    IORLW   0XF0            ;W=H2-16
    MOVWF   D1              ;D1=H2-16
    ADDWF   D1,F            ;D1=H2*2-32
    ADDWF   D1,F            ;D1=H2*3-48
    MOVWF   D2              ;D2=H2-16
    ADDLW   -D'5'           ;W=H2-21
    ADDWF   D2,F            ;D2=H2*2-37 DONE!
    ADDLW   D'41'           ;W=H2+20
    MOVWF   D0              ;D0=H2+20

    SWAPF   TEMP1,W
    IORLW   0XF0            ;W=H1-16
    ADDWF   D1,F            ;D1=H2*3+H1-64
    ADDWF   D0,F            ;D0=H2+H1+4, C=1
    RLF     D0,F            ;D0=(H2+H1)*2+9, C=0
    COMF    D0,F            ;D0=-(H2+H1)*2-10
    RLF     D0,F            ;D0=-(H2+H1)*4-20

    MOVF    TEMP1,W
    ANDLW   0X0F            ;W=H0
    ADDWF   D0,F            ;D0=H0-(H2+H1)*4-20 DONE!
    RLF     D1,F            ;C=0, D1=H2*6+H1*2-128 DONE! 

    MOVLW   D'5'
    MOVWF   D3

    MOVLW   D'10'
MOD0
    ADDWF   D0,F            ;D(X)=D(X)MOD10
    DECF    D1,F            ;D(X+1)=D(X+1)+D(X)DIV10
    BTFSS STATUS,C
    GOTO    MOD0
MOD1
    ADDWF   D1,F
    DECF    D2,F
    BTFSS STATUS,C
    GOTO    MOD1
MOD2
    ADDWF   D2,F
    DECF    D3,F
    BTFSS STATUS,C
    GOTO    MOD2  
STORE:
    MOVFW D0
    MOVWF UNITS
    MOVFW D1
    MOVWF TENS
    MOVFW D2
    MOVWF HUNDREDS
    MOVFW D3
    MOVWF THOUSANDS
    
    RETURN
;------------------------MATH RELATED AND MAPPING FUNCTIONS---------------------
SUBTRACTADC:
    MOVFW ADC_L
    MOVWF TEMP1
    MOVFW ADC_H
    MOVWF TEMP2
    MOVLW 0x9E
    MOVWF DIVIDEND_L
    MOVLW 0x02
    MOVWF DIVIDEND_H
    
    MOVFW   TEMP1
    SUBWF   DIVIDEND_L, F
    MOVFW   TEMP2
    BTFSS   STATUS,C
    INCFSZ  TEMP2,W
    SUBWF   DIVIDEND_H, F 
    RETURN
;-------------------------------------------------------------------------------
MULTIPLY100:
	CLRF	R2
	CLRF	R1
	CLRF	R0
	MOVLW   0x64
	MOVWF	TEMP1		
	BSF	R0,7
M1:
	RRF	TEMP1,F
	BTFSS STATUS, C
	GOTO	M2
	MOVFW	DIVIDEND_L
	ADDWF	R1,F
	MOVFW	DIVIDEND_H
	BTFSC STATUS, C
	INCFSZ	DIVIDEND_H,W
	ADDWF	R2,F
M2:
	RRF	R2,F
	RRF	R1,F	    ; HIGHER BYTE
	RRF	R0,F	    ; LOWER BYTE
	BTFSS STATUS, C
	GOTO	M1
	
	RETURN
;-------------------------------------------------------------------------------
DIV580:
    MOVLW 0x58
    MOVWF DIVISOR_L
    MOVLW 0x02
    MOVWF DIVISOR_H
        CLRF REM1
        CLRF REM0
        MOVLW .24
        MOVWF LOOPCOUNT
LOOPU2416:
        RLF R0, F           ;shift left divider to pass next bit to remainder
        RLF R1, F           ;and shift in next bit of result
        RLF R2, F

        RLF REM0, F            ;shift carry into remainder
        RLF REM1, F

        RLF LOOPCOUNT, F        ;save carry in counter
         
        MOVF DIVISOR_L, W          ;substract divisor from remainder
        SUBWF REM0, F
        MOVF DIVISOR_H, W
        BTFSS STATUS, C
        INCFSZ DIVISOR_H, W
        SUBWF REM1, W          ;keep that byte in W untill we make sure about borrow

	BTFSC STATUS, C
        BSF LOOPCOUNT, 0       ;set bit 0 of counter (saved carry)

        BTFSC LOOPCOUNT, 0      ;if no borrow
         GOTO UOK46LL           ;jump

        MOVF DIVISOR_L, W          ;restore remainder if borrow
        ADDWF REM0, F
        MOVF REM1, W           ;read high byte of remainder to W
                                ;to not change it by next instruction
UOK46LL:
        MOVWF REM1             ;store high byte of remainder
        CLRC                    ;copy bit 0 to carry
        RRF LOOPCOUNT, F        ;and restore counter
        DECFSZ LOOPCOUNT, F     ;decrement counter
        GOTO LOOPU2416         ;and repeat loop if not zero
         
        RLF R0, F           ;shift in last bit of result
        RLF R1, F   
        RLF R2, F
	
	; SETTING UP FOR BCD CONVERSION
	MOVFW R0
	MOVWF ADC_L
	MOVFW R1
	MOVWF ADC_H

        RETURN  
;-------------------------------------------------------------------------------
COMPARE:
 
    MOVFW ADC_L       
    MOVWF REM0        
    MOVFW ADC_H       
    MOVWF REM1        

    ; Load the threshold value (50% level = 364)
    MOVLW 0x6C        
    MOVWF TEMP1      
    MOVLW 0x10        
    MOVWF TEMP2       

EQUAL:
    ; Compare the high byte of the ADC result
    MOVFW REM1        
    SUBWF TEMP2, W    
    BTFSS STATUS, Z   ; Check if result is zero (REM1 == TEMP2)
    GOTO DONEOP       ; If equal, go to DONEOP

    ; Compare the low byte of the ADC result
    MOVFW REM0        
    SUBWF TEMP1, W    
    BTFSS STATUS, C   
    GOTO RELAY_OFF

RELAY_ON:
    BSF RELAY_FLAG, 0 ; Set relay flag
    BSF PORTB, 4      ; Turn relay ON
    GOTO DONEOP

RELAY_OFF:
    CLRF RELAY_FLAG    ; Clear relay flag
    BCF PORTB, 4       ; Turn relay OFF

DONEOP:
    RETURN             ; Return from subroutine
    
;    compare_unsigned_16: ; 7
;	movf Xhi,w
;	subwf Yhi,w ; subtract Y-X
;Are_they_equal:
;	; Are they equal ?
;	skpz
;	    goto results16
;	; yes, they are equal -- compare lo
;		movf Xlo,w
;		subwf Ylo,w	; subtract Y-X
;results16:
;	; if X=Y then now Z=1.
;	; if Y<X then now C=0.
;	; if X<=Y then now C=1.
;	return   
    END
	
