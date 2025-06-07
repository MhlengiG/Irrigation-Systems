;-------------------------------------------------------------------------------
;			CODE BY: MHLENGI GIGABA
;			DATE: 07/10/2024
;  DESCRIPTION: CODE USED TO CALIBRATE THE CAPACITIVE SOIL MOISTURE SENSOR
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
;	READ ADC RESULT
;	UPDATE LCD
;
; LCD RELATED SUBROUTINES:
;	INIT_LCD
;	LCD_CMD
;	LCD_DATA
;
; ADC RELATED SUBROUTINES:
;	ADC_CONVERT
;	ADC_PROCESS
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

LCDTEMP EQU 0x60
TEMP1_CONVERT EQU 0x61
TEMP2_CONVERT EQU 0x62
DELAYTEMPVAR EQU 0x63 
COUNTER_CONVERT EQU 0x64
TEMPVALUE EQU 0x65
COUNT EQU 0x68	   ; ADC VALUE
HUNDREDS EQU 0x66  ; For hundreds digit
TENS EQU 0x67      ; For tens digit
UNITS EQU 0x69     ; For units digit
ADC_H EQU 0x6B
ADC_L EQU 0x6C
 
 
 ORG 0x00
 GOTO INIT
 
 
;--------------------------CONFIGURATION SECTION--------------------------------

INIT:
    BANKSEL TRISA	    ; BANK1
    BSF TRISA, 0	    ; AN0 PIN AS INPUT
    BCF TRISA, 4
    BCF TRISA, 5
    CLRF TRISC
    BANKSEL PORTC
    CLRF PORTC
    CLRF PORTA
    
ADC_INIT:
    CLRF ANSEL		    ; ALL PINS DIGITAL
    BSF ANSEL, 0	    ; RA0 AS ANALOGUE PIN
    CLRF ADCON1		    ; FOSC/ 2
    BANKSEL ANSELH	    ; BANK0
    CLRF ANSELH		    ; ALL PINS DIGITAL
    BANKSEL ADCON0
    MOVLW B'00000001'	    ; LEFT, VDD AS VREF, AN0 AS ANALOGUE
    MOVWF ADCON0
    
    CLRF ADC_H
    CLRF ADC_L
    CALL INIT_LCD
    GOTO MAIN
    
;---------------------------------MAIN------------------------------------------
MAIN:
    CALL ADC_READ
    
    BSF PORTA, 2
    CALL DELAY100ms	    
    BCF PORTA, 2	    
    CALL DELAY100ms	     ; UPDATE EVERY 2 SECONDS
    
    CALL UPDATE
    GOTO MAIN
    
;------------------------------ADC RELATED SUBROUTINES--------------------------
 
ADC_READ:
    CALL DELAY1ms
    BSF ADCON0, 1	    ; START ADC CONVERSION
    CALL DELAY1ms
ADC_POLL:
    BTFSC ADCON0, 1
    GOTO ADC_POLL
    
    MOVFW ADRESH
    MOVWF ADC_H    		
    
    RETURN   
    
    
   
;------------------------------LCD RELATED SUBROUTINES--------------------------
INIT_LCD:
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
    
    CALL START_MSG	;CALL START MSG
    RETURN
 
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
    
START_MSG:
    MOVLW 0x01        ; Clear Screen
    CALL LCD_CMD 
    MOVLW 0x80        ; Set Cursor to beginning of first line
    CALL LCD_CMD 
    
    MOVLW ' '
    CALL LCD_DATA
    MOVLW 'W'
    CALL LCD_DATA
    MOVLW 'E'
    CALL LCD_DATA
    MOVLW 'L'
    CALL LCD_DATA
    MOVLW 'C'
    CALL LCD_DATA
    MOVLW 'O'
    CALL LCD_DATA
    MOVLW 'M'
    CALL LCD_DATA
    MOVLW 'E'
    CALL LCD_DATA
    MOVLW ':'
    CALL LCD_DATA
   
    MOVFW ADC_H
    CALL BIN2BCD
    
    MOVFW HUNDREDS     ; Display hundreds digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW TENS         ; Display tens digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW UNITS        ; Display units digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    RETURN

    
 
UPDATE:
    MOVLW 0x01        ; Clear Screen
    CALL LCD_CMD
    MOVLW 0x80        ; Set Cursor to beginning of first line
    CALL LCD_CMD
    
    MOVLW ' '
    CALL LCD_DATA
    MOVLW 'V'
    CALL LCD_DATA
    MOVLW 'O'
    CALL LCD_DATA
    MOVLW 'L'
    CALL LCD_DATA
    MOVLW 'T'
    CALL LCD_DATA
    MOVLW 'A'
    CALL LCD_DATA
    MOVLW 'G'
    CALL LCD_DATA
    MOVLW 'E'
    CALL LCD_DATA
    MOVLW ':'
    CALL LCD_DATA
    MOVLW ' '
    CALL LCD_DATA
    
    MOVFW ADC_H
    CALL BIN2BCD
    
    MOVFW HUNDREDS     ; Display hundreds digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW TENS         ; Display tens digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    MOVFW UNITS        ; Display units digit
    ADDLW 0x30         ; Convert to ASCII
    CALL LCD_DATA
    
    MOVLW '.'          ; Display decimal point
    CALL LCD_DATA

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
    MOVWF TEMP1_CONVERT         ; Copy input to TEMP1_CONVERT
    CLRF COUNTER_CONVERT        ; Clear COUNTER_CONVERT
    CLRF TEMP2_CONVERT          ; Clear TEMP2_CONVERT for hundreds
    MOVWF TEMP2_CONVERT         ; TEMP2_CONVERT = Binary input

    ; Divide by 100 for hundreds place
    MOVLW .100
Div100Loop:
    SUBWF TEMP2_CONVERT, 1
    BTFSC TEMP2_CONVERT, 7      ; If negative, exit loop
    GOTO Div100Complete
    INCF COUNTER_CONVERT, 1     ; Increment quotient
    GOTO Div100Loop 
Div100Complete:
    ADDWF TEMP2_CONVERT, 1      ; Undo last subtraction
    MOVFW COUNTER_CONVERT       ; Move quotient to W
    MOVWF HUNDREDS              ; Store hundreds digit in HUNDREDS
    CLRF COUNTER_CONVERT        ; Clear Counter for next division

    ; Divide by 10 for tens place
    MOVLW 0x0A                  ; Divide by 10 for tens
Div10Loop:
    SUBWF TEMP2_CONVERT, 1
    BTFSC TEMP2_CONVERT, 7      ; If negative, exit loop
    GOTO Div10Complete
    INCF COUNTER_CONVERT, 1     ; Increment quotient
    GOTO Div10Loop 
Div10Complete:
    ADDWF TEMP2_CONVERT, 1      ; Undo last subtraction
    MOVFW COUNTER_CONVERT       ; Move quotient to W
    MOVWF TENS                  ; Store tens digit in TENS

    ; Remainder is units place
    MOVFW TEMP2_CONVERT         ; Move remainder to W
    MOVWF UNITS                 ; Store units digit in UNITS
    RETURN
    
    END