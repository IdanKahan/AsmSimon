IDEAL
MODEL small
STACK 100h

DATASEG
    filehandle  dw  ?
    Header      db  54 dup (0)
    Palette     db  256*4 dup (0)
    ScrLine     db  320 dup (0)
    ErrorMsg    db  'Error', 13, 10 ,'$'


	;pictures
    defaultPic db 'SimonPic.bmp',0
    yellowOnPic db 'YellowW.bmp',0
	blueOnPic db 'BlueA.bmp',0
    greenOnPic db 'GreenD.bmp',0
    redOnPic db 'RedS.bmp',0
	gameOverPic db 'gameOver.bmp',0	
	victoryPic db 'vic.bmp',0
	mainMenuPic db 'mainMenu.bmp',0
	
	;sounds:
    yellowSound dw 11D7h  ; 261.626 Hz (C4)
    blueSound   dw 17F6h  ; 195.998 Hz (G3)
    greenSound  dw 0BD5h  ; 391.995 Hz (G4)
    redSound    dw 0E16h  ; 329.628 Hz (E4)
	wrongButtonSound  dw 5F5Ch  ; 48.9994 Hz (G1)
	
	;time:
    clock equ es:6Ch
    timePeriod dw 9
	
	;arrays
	randomGameButtons db 10 dup(?)
	userPlayedButtons db 10 dup(?)
	
	;Counters:
	currentGameLevel db 0
	countButtonsUserPlayed db 0
	
	;Booleans
	isGameOver db 0
	
	;msg
    Line1Msg db 'ITS YOUR TURN!'
    Line2Msg db 'Level: '
	
CODESEG



proc OpenFile
    mov ah, 3Dh
    xor al, al
    int 21h
    mov [filehandle], ax
    ret
endp OpenFile


proc ReadHeader
    mov ah, 3Fh
    mov bx, [filehandle]
    mov cx, 54
    mov dx, offset Header
    int 21h
    ret
endp ReadHeader

proc ReadPalette
    mov ah, 3Fh
    mov bx, [filehandle]
    mov cx, 400h
    mov dx, offset Palette
    int 21h
    ret
endp ReadPalette

proc CopyPal
    mov si, offset Palette
    mov cx, 256
    mov dx, 3C8h
    mov al, 0
    out dx, al
    inc dx

PalLoop:
    mov al, [si+2]
    shr al, 2
    out dx, al
    mov al, [si+1]
    shr al, 2
    out dx, al
    mov al, [si]
    shr al, 2
    out dx, al
    add si, 4
    loop PalLoop
    ret
endp CopyPal

proc CopyBitmap
    mov ax, 0A000h
    mov es, ax
    mov cx, 200

PrintBMPLoop:
    push cx
    mov di, cx
    shl cx, 6
    shl di, 8
    add di, cx
    mov ah, 3Fh
    mov bx, [filehandle]
    mov cx, 320
    mov dx, offset ScrLine
    int 21h
    cld
    mov cx, 320
    mov si, offset ScrLine
    rep movsb
    pop cx
    loop PrintBMPLoop
    ret
endp CopyBitmap

proc CloseFile
    mov ah, 3Eh
    mov bx, [filehandle]
    int 21h
    ret
endp CloseFile

proc DisplayBMPProc
    push dx ; Already doing this for the filename

    ; --- Save registers ---
    push bx
    push di
    push si 
    push cx 

    call OpenFile

    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap

    call CloseFile

    ; --- Restore registers before returning ---
    pop cx
    pop si
    pop di
    pop bx
    pop dx
    clc      
    ret

endp DisplayBMPProc



proc ShowDefaultImage
	mov dx, offset defaultPic
    call DisplayBMPProc
endp ShowDefaultImage

proc Timer
    push bp
    mov bp, sp

    mov ax, 40h
    mov es, ax
    mov ax, [clock]

firstTick:
    cmp ax, [clock]
    je firstTick

    mov cx, [timePeriod]

delayLoop:
    mov ax, [clock]

tick:
    cmp ax, [clock]
    je tick

    loop delayLoop

    pop bp
    ret
endp Timer



proc PlaySound
    ; Enable the speaker
    in al, 61h
    or al, 00000011b
    out 61h, al

    mov al, 0B6h
    out 43h, al

    out 42h, al
    mov al, ah
    out 42h, al

    call Timer

    ; Disable the speaker after the sound finishes
    in al, 61h
    and al, 11111100b
    out 61h, al

    ret
endp PlaySound





;Displays all the previous buttons that are stored in the games button array plus the new one
;yellow  = 0 (W key)
;blue    = 1 (A key)
;green   = 2 (D key)
;red     = 3 (S key)

proc PlayColors
    mov di, offset randomGameButtons
    mov bl, 0                   
    mov dl, 255                 

PlayLoop:
    mov cl, [currentGameLevel]
    inc cl                      ; Total buttons = level + 1
    cmp bl, cl                  
    je DoneDisplaying

    ; Show default image first (unless it's the very first button)
    cmp bl, 0
    je SkipFirstDefault
	
	
    call ShowDefaultImage
    call Timer
SkipFirstDefault:

    mov al, [di]                ; Load current button value
    call ShowButtonVisualAndSound ; Show current button and play sound

    mov dl, al                  ; Store current button as the previous one
    inc bl                      ; Increment index
    inc di                      ; Move to next button
    jmp PlayLoop                ; Continue loop

DoneDisplaying:
    call ShowDefaultImage       ; Show default image after sequence
    ret
endp PlayColors







;compares the last pressed button by the player of that of the game and sets isGameOver accordingly
proc CompareGameAndPlayerArr
    
    mov bl, [countButtonsUserPlayed] ; bl = index of user pressed btns

    mov si, offset userPlayedButtons
    add si, bx                  ;  point si to userPlayedButtons[index]

    mov di, offset randomGameButtons
    add di, bx                  ; point di to randomGameButtons[index]

    ; Compare the buttons
    mov al, [si]                ; al = user's button value (0, 1, 2, or 3)
    cmp [di], al                ; Compare with the game's value at this position
    jne SetGameOver          

    ; If we reach here the player was correct 
    ret        

SetGameOver:
    mov [isGameOver], 1       
    ret                        
endp CompareGameAndPlayerArr





proc ResetArr
	mov cx, 10
	mov di,offset userPlayedButtons
	ResetLoop:
		mov al,0
		mov [di],al
		inc di 
		
		loop ResetLoop
	ret
endp ResetArr



proc FillRandomButtons
    push ax
    push bx
    push cx
    push dx
    push di          
    push si
    push es

    ; Use timer and code address to make a random seed
    mov ax, 40h
    mov es, ax
    mov bx, [clock]
    mov di, offset FillRandomButtons
    add bx, di

    mov ax, 25173    ; Random math numbers for more randomness
    mov cx, 13849

    mov si, offset randomGameButtons
    push cx
    mov cx, 10       ; Make 10 random numbers

FillLoop:
    ; Make next random number
    mul bx
    pop bx
    add ax, bx
    push bx
    mov bx, ax

    ; Mix with timer and code
    mov ax, [clock]
    mov dl, [byte cs:di]
    inc di

    xor bx, ax
    rol bx, 5
    xor bl, dl

    ; Get a number 0-3
    mov ax, bx
    shr ax, 14
    and al, 00000011b

    mov [si], al     ; Store in array
    inc si
    loop FillLoop

    pop cx
    pop es
    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
endp FillRandomButtons

	
; Helper procedures
proc ShowButtonVisualAndSound
    cmp al, 0
    je Yellow
    cmp al, 1
    je Blue
    cmp al, 2
    je Green
    cmp al, 3
    je Red
	
	Yellow:
		mov dx, offset yellowOnPic
		call DisplayBMPProc
		mov ax,[yellowSound]
		call PlaySound
		ret
	Blue:
		mov dx, offset blueOnPic
		call DisplayBMPProc    
		mov ax,[blueSound]
		call PlaySound
		ret
	Green:
		mov dx, offset greenOnPic
		call DisplayBMPProc
		mov ax,[greenSound]
		call PlaySound
		ret 
	Red:
		mov dx, offset redOnPic
		call DisplayBMPProc
		mov ax,[redSound]
		call PlaySound
		ret
endp ShowButtonVisualAndSound




proc PrintScore

    ; --- Save Registers ---
    push ax
    push bx
    push cx
    push dx
    push si


    ;Print "ITS YOUR TURN!"

    mov dh, 11      ;Row 11
    mov dl, 13      ;Column 13
    mov bh, 0       
    mov ah, 02h     ;Set Cursor Position
    int 10h

    mov si, offset Line1Msg
    mov cx, 14      ; Length of "ITS YOUR TURN!"
PrintLine1Loop:
    mov al, [si]    
	
    mov ah, 0Eh     ; Function: Teletype Output
    mov bh, 0       ; Display Page 0
    mov bl, 254     ; Color (Cyan)
    int 10h
    inc si          
    loop PrintLine1Loop

    ;Print "Level: N"
	
    ;Set Cursor for Line 2
    mov dh, 12      ;Row 12
    mov dl, 16      ;Column 16
    mov bh, 0       ; Display Page 0
    mov ah, 02h     ; Set Cursor Position
    int 10h

    mov si, offset Line2Msg
    mov cx, 7       ; Length of "Level: "
PrintLine2Loop:
    mov al, [si]    
    ; Display character using BIOS Teletype
    mov ah, 0Eh     ;Teletype Output
    mov bh, 0       
    mov bl, 254     ; Color (Cyan)
    int 10h
    inc si        
    loop PrintLine2Loop

    ;Print the currentGameLevel digit (0-9)
    mov al, [currentGameLevel] ;Get the level number (0-9)
    add al, '0'     ; Convert num to ASCII 

    ; Display the digit using BIOS Teletype
    mov ah, 0Eh ;Teletype Output
    mov bh, 0       
    mov bl, 254 ;Color (Cyan)
    int 10h

    ;Restore Registers
    pop si
    pop dx
    pop cx
    pop bx
    pop ax

    ret

endp PrintScore




start:
    mov ax, @data
    mov ds, ax
	
	; Set video mode 320x200
    mov ax, 13h
    int 10h
	
	
    ;Display Main Menu and wait for user to press enter:
    mov dx, offset mainMenuPic
    call DisplayBMPProc
	
	
		
	WaitForEnter:
		; Check for keypress
		mov ah, 01h         ;Get key satus
		int 16h             
		jz WaitForEnter     ;If no key is at down status, repeat the loop

		;If a key was pressed down, process the keypress
		mov ah, 00h ;read key func
		int 16h             

		cmp ah, 1Ch         
		jne WaitForEnter




		
		

	

    ; Initialize game state
    mov [currentGameLevel], 0
    mov [countButtonsUserPlayed], 0
    mov [isGameOver], 0
    call ResetArr
	;randomize levels
	call FillRandomButtons
	
	
    ; Show first sequence
    call PlayColors

; Main game loop
WaitForInput:
    ; Check for keypress
    mov ah, 01h
    int 16h
	
	call PrintScore
    jz WaitForInput  ; Loop if no key pressed

    ; Process keypress
    mov ah, 00h
    int 16h

    ; Route to correct handler
    cmp ah, 11h         ; W key
    je HandleYellow
    cmp ah, 1Eh         ; A key
    je HandleBlue
    cmp ah, 20h         ; D key
    je HandleGreen
    cmp ah, 1Fh         ; S key
    je HandleRed
    jmp WaitForInput    ; Ignore other keys

; Button handlers
HandleYellow:
    mov al, 0
    jmp StoreInput
HandleBlue:
    mov al, 1
    jmp StoreInput
HandleGreen:
    mov al, 2
    jmp StoreInput
HandleRed:
    mov al, 3
    jmp StoreInput

StoreInput:
    ; Save to user input array
    mov si, offset userPlayedButtons
    mov bl, [countButtonsUserPlayed]
    mov [si+bx], al

    ; Show button animation
    call ShowButtonVisualAndSound
	mov dx, offset defaultPic
	call DisplayBMPProc
	
	
	
    call CompareGameAndPlayerArr
    cmp [isGameOver], 1
    je GameOver
	
	
    ; Update input counter
    inc [countButtonsUserPlayed]
		
		
    ; Check if completed required inputs
    mov bl, [currentGameLevel]
    inc bl                ; Required inputs = level + 1
    cmp [countButtonsUserPlayed], bl
    jne WaitForInput      ; Continue if not enough inputs



    ; Check for final win condition
    cmp [currentGameLevel], 9
    je YouWin

    ; Advance to next level
    inc [currentGameLevel] ;increase level

	;reset vars
	call ResetArr
    mov [countButtonsUserPlayed], 0
	
	;show next level then wait for player then let player responde
	call ShowDefaultImage
    call PlayColors
    jmp WaitForInput

GameOver:
	;Play the losing sound
	push [timePeriod]
	mov [timePeriod],40
	mov ax,[wrongButtonSound]
	call PlaySound
	pop ax                  
	mov [timePeriod], ax      
    mov dx, offset gameOverPic
    call DisplayBMPProc
	
	;Wait for the user to press enter for retry
    jmp WaitForEnter

YouWin:
    mov dx, offset victoryPic
    call DisplayBMPProc
	
	;Wait for the user to press enter for retry
	jmp WaitForEnter
	

exit:
    mov ax, 4C00h
    int 21h

END start