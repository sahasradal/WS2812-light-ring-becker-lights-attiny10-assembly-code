;
; beckerlight.asm
;
; Created: 17/06/2022 08:24:54
; Author : Manama
;


; Created: 17/05/2022 19:21:34
; Author : Manama
; data order green red blue
; RHS signal lamp - clockwise - left to rhs sequence , cycle balanced to match lhs signal at the time of hazard operation


;pb0 dataout
;
.def data = r19

.macro set_pointer
ldi ZL,low((2*@0)+0x4000)
ldi ZH,high((2*@0)+0x4000)
.endm 
 


.dseg

pad1: .byte 1
pad2: .byte 1



.cseg


reset:
    LDI r16,0xD8		;setting clock divider change enable
	OUT CCP,r16
	LDI r16,0x00		; selecting internal 8MHz oscillator
	OUT CLKMSR, r16
	LDI r16,0xD8		; setting clock divider change enable
	OUT CCP,r16	
	LDI r16,(0<<CLKPS3)+(0<<CLKPS2)+(0<<CLKPS1)+(0<<CLKPS0);
	OUT CLKPSR,r16		; set to 8MHz clock (disable div8)
	LDI r16,0xFF		; overclock (from 4MHz(0x00) to 15 MHz(0xFF))
	OUT OSCCAL,r16
portsetup:
	ldi r16,0b0001		; load r16 with 0x1
	out ddrb,r16		; enable pb0 as output
	ldi r16,0b0000		; load r16 0x00
	out portb,r16		; port b low (0v)
	rcall LED_RESET		;put data line low,positive edge is the main factor
mainloop:
	rcall audi1			;routine that lights up each led one by one until all LEDS are lit like a audi car indicator
;	rcall delay1		;761 cycles delay
;	rcall delay2		;251 cycles delay
;	nop					;1 cycle
;	nop					;1 cycle = total cycles till here 7627792 . this 1017 cycles added to synchronise with LHS signal which consumes 7627792 cycles to complete audi loop. will be unsynced in hazard operation.
	push r22			; save r22 to stack
	ldi r22,255			; load 255 for delay routine
	rcall delayms		; gives 331ms delay
	ldi r22,200			; load 255 for delay routine,5
	rcall delayms		; gives 331ms delay
	pop r22				; restore r22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	rcall dblackout		;proc to kill the light for 331ms
;	push r22			;
;	ldi r22,255			;
;	rcall delayms		;331ms   
;	pop r22				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	rcall audi			;routine that lights up each led one by one until all LEDS are lit like a audi car indicator
	push r22			; save r22 to stack
	ldi r22,255			; load 255 for delay routine
	rcall delayms		; gives 331ms delay
	ldi r22,200			; load 255 for delay routine
	rcall delayms		; gives 331ms delay
	pop r22				; restore r22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	rcall dblackout		;added on 10-06-22 for testing not needed on vehicle if connected from flasher relay output/flashing output
;	push r22			;added on 10-06-22 for testing not needed on vehicle if connected from flasher relay output/flashing output
;	ldi r22,255			;added on 10-06-22 for testing not needed on vehicle if connected from flasher relay output/flashing output
;	rcall delayms		;331ms   added on 10-06-22 for testing not needed on vehicle if connected from flasher relay output/flashing output
;	pop r22				;added on 10-06-22 for testing not needed on vehicle if connected from flasher relay output/flashing output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	rcall coloursend
	rcall flasher
	rcall flasher
	rcall flasher
	rcall blackout
	rcall init_colour
	rcall glow
	rcall fade
	rcall glow
	rcall fade
	rcall glow
	rcall fade
	rcall glow
	rcall fade
	rcall glow
	rcall fade
	rcall glow
	rcall fade
	rcall rotate
	rcall rotate
	rcall rotate
	rjmp mainloop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Transmits 1 byte to the led matrix ,call 3 times for 1 led to transmit g,r,b data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
bytetx:
	ldi r17,8			; number of bits 8
loop0:
	sbi portb,0			; set pb0 high
	nop					; 417ns = 0
	sbrc data,7			; if bit to be transmitted at position 7 is 0 skip next instruction of calling additional delay
	rcall ten66ns		; 1us = 1 (if bit 7 is 1 this instruction is executed and total delay of 1us for data to stay high)
	lsl data			; shift data out as we transmitted equalent pulse tp LED
	cbi portb,0			; pull pb0 low
	rcall ten66ns		; 1us = off time
	dec r17				; decrease bit counter
	brne loop0			; loop back until counter is 0
	ret					; return to caller


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;10 nano seconds delay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
ten66ns:
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the ws2812 reset procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LED_RESET:					;66us
	cbi portb,0
	ldi r16,255
loop1:
	dec r16
	brne loop1
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;delay routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
delay:
	push r16
	ldi r16,250
	rcall delay1
dd:	dec r16
	brne dd
	pop r16
	ret

delay1:
	push r20
	ldi r20,250
ddd:dec r20
	brne ddd
	pop r20
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1 milli second delay routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ms1:
	push r16
	ldi r16,10
msloop:
	rcall delay
	dec r16
	brne msloop
	pop r16
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
delayms:
;	ldi r22,16
delaymsloop:
	rcall ms1
	dec r22
	brne delaymsloop
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ms500:
	push r22			; save r22 to stack
	ldi r22,255			; load 255 for delay routine
	rcall delayms		; gives 331ms delay
	ldi r22,200			; load 255 for delay routine,5
	rcall delayms		; gives 331ms delay
	pop r22				; restore r22
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ms20:
	push r22				;save r22 for delay
	ldi r22,20		        ;load delay count
	rcall delayms			;20.4ms for value 16 on logic analyzer
	pop r22					;restore r22
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;RHS indicator lamp flash routine - leds sequence from lhs to Rhs / clockwise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

audi1:
	ldi r20,24				;load r20 with # of LEDs , tested with 24 led ring
	ldi r21,1				;load r21 with 1 (1st step ,will be increased by audiloop, max out at 24 steps for each led)
audiloop1:
	rcall sendorange		;procedure to light up led with orange colour (car signal is yelow/orange)
	inc r22					;r22 is loop counter from 0 -24
	cp r22,r21				;check if r22 has looped the amount stored in r21 (if r21 is 1 ,1 led is lit , if 2 ,2 led lit)
	brne audiloop1			;if r22 not equal to no of steps in r21 loop again
	ldi r22,24				;reach here when number of leds specified in r21 has already lit up (the remining leds need to be off)
	sub r22,r21				;subtract r22 with r21 the remaining number is the off leds
	breq alllit				;if r22 = r21 all leds are lit so branch to alllit label
blackloop:
	rcall sendblack			;send off frame to one led
	dec r22					;decrease r22
	brne blackloop			;loop till all remaining leds rae sent 0x00,0x00,0x00 frame
alllit:
	rcall LED_RESET			;send LED_RESET to latch data , 
	push r22				;save r22 for delay
	ldi r22,20		        ;load delay count
	rcall delayms			;20.4ms for value 16 on logic analyzer
	pop r22					;restore r22
	inc r21					;increase to be lighted led count by 1
	dec r20					;decrease led count as each led is lit up (used as condition check, al 24 leds lit up r20 =0)
	brne audiloop1			; loop back till r20 = 0	
	ret						; return to caller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this procedure sends 24 off frames to each led to switch the entire indicator off , needed on continous power supply only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
blackout:
	ldi r20,24				;load r20 with # of LEDs , here 24 leds on the ring from aliexpress
boloop:
	rcall sendblack			;call procedure to send 0s to all colours in each led 0x00,0x00,0x00
	dec r20					;decrease led counter
	brne boloop				;loop back till 24 sets are sent
	rcall LED_RESET			;sent led reset at the ned to latch sent data
	ret						;return to caller
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sends 0x00,0x00,0x00 to led to make it off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
sendblack:
	ldi data,0 ;green
	rcall bytetx
	ldi data,0 ;red
	rcall bytetx
	ldi data,0 ;blue
	rcall bytetx
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sends colour data to led ,3 bytes , call as many times as many leds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sendorange:
	ldi data,50 ;green
	rcall bytetx
	ldi data,255 ;red
	rcall bytetx
	ldi data,0
	rcall bytetx
	ret

delay2:
	push r20
	ldi r20,81
ddd2:dec r20
	brne ddd2
	pop r20
	ret
dblackout:
rcall blackout		;proc to kill the light for 331ms
	push r22			;
	ldi r22,255			;
	rcall delayms		;331ms   
	pop r22		
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LHS indicator lamp flash routine - leds sequence from rhs to lhs /anti clockwise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

audi:
	ldi r20,24			;load r20 with # of LEDs , tested with 24 led ring
	ldi r21,1			;load r21 with 1 (1st step ,will be increased by audiloop, max out at 24 steps for each led)
	ldi r22,24			;load r22 with # of leds , tested with 24 led ring
audiloop:
	cpi r21,25			;check r21 reached 25th step (means 24 steps finished for 24 leds and all leds light up)
	breq allon			;if all 24 steps are finished branch to allon to exit procedure
	cpi r21,24			;check if r21 is on the 24th step, the last step has no unlit leds so no need blank procedure
	breq orangeloop		;branch to orangeloop , override sendblack as all leds are to be litup in the 24th step
	rcall sendblack		;1st step to 23rd step needs this procedure to keep leds off in the range of 23 - 1
	dec r22				;decrease r22 (step counter)
	cp r22,r21			;check whether step counter has performed enough black frames
	brne audiloop		;if required number of blank steps has not been performed loop back
orangeloop:
	rcall sendorange	;call procedure to light up the LED in orange colour
	dec r22				;dec step counter
	brne orangeloop		;if steps not exhausted loop through orangeloop
	rcall LED_RESET		;if all 24 steps have finished perform LED_RESET to latch data
	push r22			;save r22 for delay routine
	ldi r22,20           ; 16 ,now increased to 20 for more delay
	rcall delayms		;20.4ms for value 16 on logic analyzer
	pop r22				;restore r22 after delay proc
	inc r21				;increase led counter
	ldi r22,24			;reload the step counter
	rjmp audiloop		;jump back to audiloop
allon:					; return to caller when all 24 leds are lit
	ret

white:
	ldi data,100 ;green
	rcall bytetx
	ldi data,100 ;red
	rcall bytetx
	ldi data,100	;blue
	rcall bytetx
	ret
/*	
colourinit:
	ld r23,Z+
	ld r24,Z+
	ld r25,Z+
	ret
*/ /*
colourarray:
	mov data,r23 ;green
	rcall bytetx
	mov data,r24 ;red
	rcall bytetx
	mov data,r25	;blue
	rcall bytetx
	ret
*/
fadearray:
	mov data,r23 ;green
	rcall bytetx
	mov data,r24 ;red
	rcall bytetx
	mov data,r25	;blue
	rcall bytetx
	ret

fadecalc:
	rcall init_colour
mainstep:
	ld r23,z+
	ld r24,z+
	ld r25,z+
	ret
innercalc:
	cpi r23,0x00
	brne greenreg
ent1:
	cpi r24,0x00
	brne redreg
ent2:
	cpi r25,0x00
	brne bluereg
	ret

greenreg:
	subi r23,5
	rjmp ent1
redreg:
	subi r24,5
	rjmp ent2
bluereg:
	subi r25,5
	ret



coloursend:
	rcall init_colour
	ldi r20,24				;load r20 with # of LEDs , tested with 24 led ring
	ldi r21,3				;load r21 with 3
colourloop1:
	ldi r20,24
	rcall addresscolour
	rcall mainstep
colourloop2:
	rcall fadearray		;procedure to light up led with orange colour (car signal is yelow/orange)
	dec r20
	brne colourloop2		;if r22 not equal to no of steps in r21 loop again
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms500
	dec r21
	brne colourloop1	
	ret						; return to caller
addresscolour:
	subi ZL,low(-3)
	sbci ZH,high(-3)
	ret
init_colour:
	ldi ZL,low((2*colour)+0x4000)	
	ldi ZH,high((2*colour)+0x4000)
	ret	
colour:
;.db 0xff,0x00,0x00,0x1,0x00,0x00,0x00,0xff,0x00,0x00,0x01,0x00,0x00,0x00,0xff,0x00,0x00,0x01,0xff,0xff,0x00,0x01,0x01,0x00,0x00,0xff,0xff,0x00,0x01,0x01,0xff,0x00,0xff,0x01,0x00,0x01
.db 0x1,0x00,0x00,0xff,0x00,0x00,0x00,0x01,0x00,0x00,0xff,0x00,0x00,0x00,0x01,0x00,0x00,0xff,0x01,0x01,0x00,0xff,0xff,0x00,0x00,0x01,0x01,0x00,0xff,0xff,0x01,0x00,0x01,0xff,0x00,0xff
flasher:
	ldi r20,24				;load r20 with # of LEDs , tested with 24 led ring
	ldi r21,6				;load r21 with 1 (1st step ,will be increased by audiloop, max out at 24 steps for each led)
flashloop1:
	ldi r20,24
flashloop2:
	rcall white				;procedure to light up led with orange colour (car signal is yelow/orange)
	dec r20
	brne flashloop2			;if r22 not equal to no of steps in r21 loop again
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	rcall blackout
	rcall ms20
	dec r21
	brne flashloop1	
	ldi r20,24				;load r20 with # of LEDs , tested with 24 led ring
flashloop3:
	rcall white				;procedure to light up led with orange colour (car signal is yelow/orange)
	dec r20
	brne flashloop3			;if r22 not equal to no of steps in r21 loop again
	rcall LED_RESET			;send LED_RESET to latch data , 
	ret						; return to caller


fade:
;	rcall init_colour
	ldi r20,24				;load r20 with # of LEDs , tested with 24 led ring
	ldi r27,6
	ldi r21,52				;load r21 with 1 (1st step ,will be increased by audiloop, max out at 24 steps for each led)
step:
	rcall mainstep
fadeloop1:
	ldi r20,24
	rcall innercalc			;procedure to light up led with orange colour (car signal is yelow/orange)
fadeloop2:
	rcall fadearray
	dec r20
	brne fadeloop2		;if r22 not equal to no of steps in r21 loop again
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	dec r21
	brne fadeloop1
;	dec r27
;	brne step
	ret						; return to caller
	
/*
glowarray:
	mov data,r23 ;green
	rcall bytetx
	mov data,r24 ;red
	rcall bytetx
	mov data,r25	;blue
	rcall bytetx
	ret
*/
;glowcalc:
;	rcall init_colour1
;	nop
/*	
glowstep:
	ld r23,z+
	ld r24,z+
	ld r25,z+
	ret
*/
innercalc1:
	cpi r23,0x00
	brne greenreg1
entry1:
	cpi r24,0x00
	brne redreg1
entry2:
	cpi r25,0x00
	brne bluereg1
	ret

greenreg1:
	cpi r23,0xff
	brne exit1
	ldi r23,0
exit1:
	subi r23,-5
	rjmp entry1
redreg1:
	cpi r23,0xff
	brne exit2
	ldi r23,0
exit2:
	subi r24,-5
	rjmp entry2
bluereg1:
	cpi r23,0xff
	brne exit3
	ldi r23,0
exit3:
	subi r25,-5
	ret







glow:
;	rcall init_colour1
	ldi r20,24				;load r20 with # of LEDs , tested with 24 led ring
	ldi r27,6
	ldi r21,52				;load r21 with 1 (1st step ,will be increased by audiloop, max out at 24 steps for each led)
step1:
	rcall mainstep
glowloop1:
	ldi r20,24
	rcall innercalc1			;procedure to light up led with orange colour (car signal is yelow/orange)
glowloop2:
	rcall fadearray
	dec r20
	brne glowloop2		;if r22 not equal to no of steps in r21 loop again
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	dec r21
	brne glowloop1
;	dec r27
;	brne step1
	ret						; return to caller
	
/*
red:.db 0x00,0xff,0x00
green: .db 0xff,0x00,0x00
blue: .db 0x00,0x00,0xff
*/


colour2:
.db 0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00
.db 0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00,0xff,0x00,0x00
;total 72 bytes , 3bytes = 1 led
colour2pointer:
set_pointer colour2
ret
	
step_0:
	ldi r20,24
step_1:
	rcall transferarray
	cpi r21,72
	breq resetpointer
entry22:
	dec r20
	brne step_1
	rcall colour2pointer
	clr r21
	ret

resetpointer:
	rcall colour2pointer
	clr r21
	rjmp entry22
	

transferarray:
	ld data,Z+
	inc r21
	rcall bytetx
	ld data,Z+
	inc r21
	rcall bytetx
	ld data,Z+
	inc r21
	rcall bytetx
	ret
rotate:
	rcall colour2pointer     ;macro
	ldi r21,0
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,70
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,67
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,64
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,61
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,58
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,55
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,52
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,49
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,46
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,43
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,40
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,37
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,34
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,31
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,28
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,25
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,22
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,19
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,16
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,13
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,10
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,7
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,4
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ldi r21,1
	rcall addresscalc
	rcall step_0
	rcall LED_RESET			;send LED_RESET to latch data , 
	rcall ms20
	ret

addresscalc:
	clr r27
	add ZL,r21
	adc ZH,r27
	ret