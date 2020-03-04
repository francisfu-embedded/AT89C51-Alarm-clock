;Copyright Francis Fu
;AT89C51 Alarm Clock
$NOLIST
$MODLP51
$LIST


TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 16000000 ; Microcontroller system crystal frequency 
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RELOAD EQU 12202     ;timer cycle 40 ms with 16mhz crystal


BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
INCSEC        equ P0.0
INCMIN        equ P0.3
INCUHR        equ P0.6
SETPIN        equ P0.7
CLOCKPIN      equ P0.5
LEDPIN        equ P0.2
RESTART       equ P2.1

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count40ms:     ds 2 ; Used to determine when one second has passed
sec:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
min:  ds 1
Uhr:  ds 1
ssec: ds 1
smin: ds 1
suhr: ds 1
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
second_flag: dbit 1 ; Set to one in the ISR every time 1000 ms had passed
noon_flag:         dbit 1 ;noon flag to determine change to AM or PM, 0 is before noon
setting_flag:      dbit 1 ;operating mode: 0 setting mode: 1
snoon_flag:        dbit 1 ;noonflag in setting
cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

Morgen: db 'AM',0
Abend:  db 'PM',0
Setting:db 'Time to RING!',0
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count40ms, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count40ms    ; Increment the low 8-bits first
	mov a, Count40ms ; If the low 8-bits overflow, then increment high 8-bits
	cjne a, #255, Timer2_ISR_done;..............change the number to get different rates
	clr a
	mov Count40ms, a
	setb second_flag ; Let the main program know half second had passed
	                       ; Increment the BCD counter                      
	mov a, sec
	add a, #0x01
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov sec, a
	cjne a, #60h, next1
	mov sec, #0x00
	mov a, min
	add a,#0x01
	da a
	mov min, a
next1:
    mov a, min
    cjne a, #60h, next2
    mov  min, #0x00
    mov a, Uhr
    add a,#0x01
    da a
    mov Uhr,a
next2:
    mov a,Uhr
    cjne a,#12h,next3
    mov Uhr, #0x00
    jnb noon_flag, beforenoon
    ;so it's afternoon...
    Set_Cursor(1,15)
    Send_Constant_String(#Abend)
    clr noon_flag
    sjmp next3
beforenoon:
    ;so it's god damned morning !
    Set_Cursor(1,15)
    Send_Constant_String(#Morgen)
    setb noon_flag
next3:
    
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
; These custom characters copied from https://cdn.instructables.com/ORIG/FGY/5J1E/GYFYDR5L/FGY5J1EGYFYDR5L.txt
Custom_Characters:
	WriteCommand(#40h) ; Custom characters are stored starting at address 40h
; Custom made character 0
	WriteData(#00111B)
	WriteData(#01111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 1
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
; Custom made character 2
	WriteData(#11100B)
	WriteData(#11110B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 3
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#01111B)
	WriteData(#00111B)
; Custom made character 4
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 5
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11110B)
	WriteData(#11100B)
; Custom made character 6
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
; Custom made character 7
	WriteData(#11111B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#00000B)
	WriteData(#11111B)
	WriteData(#11111B)
	WriteData(#11111B)
	ret

; For all the big numbers, the starting column is passed in register R1
Draw_big_0:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)  
	WriteData(#1) 
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)  
	WriteData(#4)  
	WriteData(#5)
	WriteData(#' ')
	ret
	
Draw_big_1:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#1)
	WriteData(#2)
	WriteData(#' ')
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#4)
	WriteData(#255)
	WriteData(#4)
	WriteData(#' ')
	ret

Draw_big_2:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#7)
	WriteData(#' ')
	ret

Draw_big_3:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#6)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_4:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#4)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	WriteData(#' ')
	ret

Draw_big_5:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#255)
	WriteData(#6)
	WriteData(#6)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#7)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_6:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#6)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_7:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#1)
	WriteData(#1)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#0)
	WriteData(#' ')
	ret

Draw_big_8:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#3)
	WriteData(#7)
	WriteData(#5)
	WriteData(#' ')
	ret

Draw_big_9:
	mov a, R1
	orl a, #0x80 
	lcall ?WriteCommand 
	WriteData(#0)
	WriteData(#6)
	WriteData(#2)
	WriteData(#' ')
	mov a, R1
	orl a, #0xc0
	lcall ?WriteCommand 
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#255)
	WriteData(#' ')
	ret

; The number to display is passed in accumulator.  The column where to display the
; number is passed in R1. This works only for numbers 0 to 9.
Display_big_number:
	; We need to multiply the accumulator by 3 because the jump table below uses 3 bytes
	; for each 'ljmp' instruction.
	mov b, #3
	mul ab
	mov dptr, #Jump_table
	jmp @A+dptr
Jump_table:
	ljmp Draw_big_0 ; This instruction uses 3 bytes
	ljmp Draw_big_1
	ljmp Draw_big_2
	ljmp Draw_big_3
	ljmp Draw_big_4
	ljmp Draw_big_5
	ljmp Draw_big_6
	ljmp Draw_big_7
	ljmp Draw_big_8
	ljmp Draw_big_9
; No 'ret' needed because we are counting of on the 'ret' provided by the Draw_big_x functions above

; Takes a BCD 2-digit number passed in the accumulator and displays it at position passed in R0
Display_Big_BCD:
	push acc
	; Display the most significant decimal digit
	mov b, R0
	mov R1, b
	swap a
	anl a, #0x0f
	lcall Display_big_number
	
	; Display the least significant decimal digit, which starts 4 columns to the right of the most significant digit
	mov a, R0
	add a, #3
	mov R1, a
	pop acc
	anl a, #0x0f
	lcall Display_big_number
	
	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    clr TR0
    lcall Timer2_Init
    ; In case you decide to use the pins of P0 configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    lcall Custom_Characters ; Custom characters are needed to display big numbers.  This call generates them.
    setb second_flag
	mov sec, #0x00
	mov min, #0x00
	mov Uhr, #0x00
	clr noon_flag
	clr snoon_flag
	setb INCMIN
	Set_Cursor(1,15)
	setb setting_flag
	setb SETPIN
	clr LEDPIN
	mov ssec, #0x00
	mov smin, #0x00
	mov suhr, #0x00
	
	; After initialization the program stays in this 'forever' loop
loop:
    mov  a,sec
    cjne a,ssec,endcompare
    mov  a,min
    cjne a,smin,endcompare
    mov  a,Uhr
    cjne a,suhr,endcompare
    jb  setting_flag,endcompare
    sjmp singing
endcompare:
    ljmp endcomparing
singing:
    setb TR0
    setb LEDPIN
    ;clear the stuff
    lcall LCD_4BIT
    Wait_Milli_Seconds(#250)
    clr  TR0
    clr  LEDPIN
    ; display the stuff
    Set_Cursor(1,15)
    jb noon_flag, nachmit
    Send_Constant_String(#Morgen)
	sjmp gutenmorge
    nachmit:
    Send_Constant_String(#Abend)
    gutenmorge:
    
    mov R0, #0
	mov a,uhr
	lcall Display_Big_BCD
    
    mov R0, #7
	mov a,min
	lcall Display_Big_BCD
	
    Set_Cursor(2,15) ; Column where to display the big font 2-digit number
    Display_BCD(sec)
    
    Wait_Milli_Seconds(#250)
    jb  CLOCKPIN, contsing
    ljmp loop
contsing:
    sjmp singing

endcomparing:
    jb RESTART, nohappen
    mov min,#0x00
    mov sec,#0x00
    mov Uhr,#0x00
    clr noon_flag
nohappen:
    Set_Cursor(1,15)
    jb noon_flag, nachmittags
	Send_Constant_String(#Morgen)
	sjmp gutenmorgen
nachmittags:
    Send_Constant_String(#Abend)
gutenmorgen:
    jb SETPIN, nothing ;not
    Wait_Milli_Seconds(#250)
    
redosc:    jnb SETPIN, redosc

    setb setting_flag
    ljmp settings
nothing:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.           
	; Now clear the BCD counter
	jnb noon_flag, set_afternoon
    ;so it's afternoon...
    Set_Cursor(1,15)
    Send_Constant_String(#Abend)
    clr noon_flag
    sjmp finish
set_afternoon:
    ;so it's god damned morning !
    Set_Cursor(1,15)
    Send_Constant_String(#Morgen)
    setb noon_flag
finish:
	sjmp loop_b             ; Display the new value
loop_a:
	jb second_flag, fine
	ljmp loop
fine:
loop_b:
    clr second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    Set_Cursor(2,15) ; Column where to display the big font 2-digit number
    Display_BCD(sec)
	mov R0, #7
	mov a,min
	lcall Display_Big_BCD
	mov R0, #0
	mov a,Uhr
	lcall Display_Big_BCD
back1:
	jb INCMIN, postmin
	clr TR2
	Wait_Milli_Seconds(#150)
	mov a, min
	cjne a,#60h,not60
	clr a
	mov min, a
	sjmp postmin
not60:
	mov a, min
	add a, #0x01
	da a
	mov min, a
	mov R0, #7
	mov a,min
	lcall Display_Big_BCD
	sjmp back1
postmin:
    setb TR2
back2:
	jb INCUHR, postuhr
	clr TR2
	Wait_Milli_Seconds(#175)
	mov a, uhr
	cjne a,#12h,not12
	clr a
	mov uhr, a
	sjmp postuhr
not12:
	mov a, uhr
	add a, #0x01
	da a
	mov uhr, a
	mov R0, #0
	mov a,uhr
	lcall Display_Big_BCD
	sjmp back2
postuhr:
    setb TR2
back3:
	jb INCSEC, postsec
	clr TR2
	Wait_Milli_Seconds(#175)
	mov a, sec
	cjne a,#60h,notst
	clr a
	mov sec, a
	sjmp postsec
notst:
	mov a, sec
	add a, #0x01
	da a
	mov sec, a
    Set_Cursor(2,15) ; Column where to display the big font 2-digit number
    Display_BCD(sec)
	sjmp back3
postsec:
    setb TR2
    ljmp loop
; in the clock setting mode
settings:
    lcall LCD_4BIT
Imback:
    Set_Cursor(1,1)
    Send_Constant_String(#Setting)
    Set_Cursor(2,4)
	Display_BCD(smin)
	Set_Cursor(2,1)
	Display_BCD(suhr)
	Set_Cursor(2,7)
	Display_BCD(ssec)
	Set_Cursor(2,10)
	jb snoon_flag, itsafternoon
	Send_Constant_String(#Morgen)
	sjmp itsmorning
itsafternoon:
    Send_Constant_String(#Abend)
    itsmorning:
    ret1:  ;return to this line
	jb INCMIN, aftermin
	;clr TR2
	Wait_Milli_Seconds(#200)
	mov a, smin
	cjne a,#60h,not60m
	clr a
	mov smin, a
	sjmp aftermin
not60m:
	mov a, smin
	add a, #0x01
	da a
	mov smin,a
    Set_Cursor(2,4)
	Display_BCD(smin)
	sjmp ret1
aftermin:
    ;setb TR2
ret2:
	jb INCUHR, afteruhr
	;clr TR2
	Wait_Milli_Seconds(#200)
	mov a, suhr
	cjne a,#12h,not12h
	clr a
	mov suhr, a
	sjmp afteruhr
not12h:
	mov a, suhr
	add a, #0x01
	da a
	mov suhr, a
    Set_Cursor(2,1)
	Display_BCD(suhr)
	sjmp ret2
afteruhr:
    ;setb TR2
ret3:
	jb INCSEC, aftersec
	;clr TR2
	Wait_Milli_Seconds(#200)
	mov a, ssec
	cjne a,#60h,notsts
	clr a
	mov ssec, a
	sjmp aftersec
notsts:
	mov a, ssec
	add a, #0x01
	da a
	mov ssec, a
    Set_Cursor(2,7) ; Column where to display the big font 2-digit number
    Display_BCD(ssec)
	sjmp ret3
aftersec:
    jb BOOT_BUTTON, noproblem
    
redosc2:    jnb BOOT_BUTTON, redosc2

    cpl snoon_flag
noproblem:    
    ;setb TR2
    jb SETPIN, ignore
    Wait_Milli_Seconds(#250)
    
redosc1:    jnb SETPIN, redosc1

    clr setting_flag
    ljmp loop
ignore:
    ljmp Imback
END
