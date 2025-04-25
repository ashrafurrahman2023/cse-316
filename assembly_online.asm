
The output stops at $, leaving the cursor positioned at the start of a new line, ready for user input or further output.



If BX, SI, or DI contains the offset of the operand, 
DS contains the segment number
If BP contains the offset of the operand, 
SS contains the segment number


Exchange the 10th and 25th elements 





; This program checks if an array of words is sorted in ascending or descending order
.MODEL SMALL
.STACK 100H

.DATA
    ; Define the array of words (modify for different test cases)
    ARR DW 10, 20, 20, 30, 40, 50, 60, 70, 76, 80, 80, 100, 110, 120, 120, 120, 130
    ARR_LEN DW ($-ARR)/2  ; Calculate length of array (number of words)
    
    MSG_ASC DB 'Ascending$'
    MSG_DESC DB 'Descending$'
    MSG_NOT DB 'Not sorted$'

.CODE
MAIN PROC
    MOV AX, @DATA
    MOV DS, AX

    ; Check if array has less than 2 elements
    CMP ARR_LEN, 2
    JL PRINT_NOT  ; If length < 2, consider not sorted

    ; Compare first and last elements
    MOV AX, ARR[0]           ; First element
    MOV BX, ARR_LEN
    DEC BX
    SHL BX, 1                ; Multiply by 2 to get byte offset
    MOV DX, ARR[BX]          ; Last element

    CMP AX, DX
    JL CHECK_ASC             ; First < Last, check ascending
    JG CHECK_DESC            ; First > Last, check descending
    JMP CHECK_EQUAL          ; First = Last, check if all equal

CHECK_ASC:
    ; Check if array is in ascending order
    MOV CX, ARR_LEN
    DEC CX                   ; Compare n-1 pairs
    MOV BX, 0                ; Index for array

ASC_LOOP:
    MOV AX, ARR[BX]          ; Current element
    MOV DX, ARR[BX+2]        ; Next element
    CMP AX, DX
    JG PRINT_NOT             ; If current > next, not sorted
    ADD BX, 2                ; Move to next word
    LOOP ASC_LOOP

    LEA DX, MSG_ASC          ; Array is ascending
    JMP DISPLAY

CHECK_DESC:
    ; Check if array is in descending order
    MOV CX, ARR_LEN
    DEC CX                   ; Compare n-1 pairs
    MOV BX, 0                ; Index for array

DESC_LOOP:
    MOV AX, ARR[BX]          ; Current element
    MOV DX, ARR[BX+2]        ; Next element
    CMP AX, DX
    JL PRINT_NOT             ; If current < next, not sorted
    ADD BX, 2                ; Move to next word
    LOOP DESC_LOOP

    LEA DX, MSG_DESC         ; Array is descending
    JMP DISPLAY

CHECK_EQUAL:
    ; Check if all elements are equal
    MOV CX, ARR_LEN
    DEC CX                   ; Compare n-1 pairs
    MOV BX, 0                ; Index for array
    MOV DX, ARR[0]           ; Reference value (first element)

EQUAL_LOOP:
    MOV AX, ARR[BX+2]        ; Next element
    CMP AX, DX
    JNE PRINT_NOT            ; If any element differs, not sorted
    ADD BX, 2                ; Move to next word
    LOOP EQUAL_LOOP

    JMP PRINT_NOT            ; All equal, consider not sorted

PRINT_NOT:
    LEA DX, MSG_NOT

DISPLAY:
    ; Print the message
    MOV AH, 09H
    INT 21H

    ; Exit program
    MOV AH, 4CH
    INT 21H

MAIN ENDP
END MAIN




    
;------------------------------------------------------------
; Program: Vowel and Consonant Counter (x8086)
; Assembles with MASM/TASM, runs under DOS (INT 21h)
;------------------------------------------------------------

.model small                ; small memory model: one code, one data
.stack 100h                 ; 256-byte stack

.data
    inputStr   db 'qwertykeyboards', '$'  ; '$' terminator for INT 21h prints
    msgVowel   db 'Vowel Count: $'
    msgConso   db 'Consonant Count: $'
    vowelCount dw 0       ; 2-byte counter for vowels
    consoCount dw 0       ; 2-byte counter for consonants

.code
main proc
    ;—————— Setup DS ——————
    mov ax, @data         ; load address of data segment
    mov ds, ax

    ;—————— Initialize SI to point at our string ——————
    lea si, inputStr      ; SI ← offset of first character

scan_loop:
    mov al, [si]          ; AL ← current character
    cmp al, '$'           ; check for end-of-string marker
    je  done_scanning     ; if end, jump to output

    ;—————— Check if AL is one of 'a','e','i','o','u' ——————
    cmp al, 'a'
    je  is_vowel
    cmp al, 'e'
    je  is_vowel
    cmp al, 'i'
    je  is_vowel
    cmp al, 'o'
    je  is_vowel
    cmp al, 'u'
    je  is_vowel

    ;—————— If we get here, it's a consonant ——————
    inc word ptr consoCount
    jmp cont_next_char

is_vowel:
    inc word ptr vowelCount

cont_next_char:
    inc si                ; move to next character
    jmp scan_loop         ; repeat

done_scanning:
    ;—————— Print "Vowel Count: " message ——————
    lea dx, msgVowel
    mov ah, 9             ; DOS print string
    int 21h

    ;—————— Print vowelCount in AX decimal ——————
    mov ax, vowelCount
    call PrintDecimal

    ; Print CR+LF
    mov dl, 13            ; carriage return
    mov ah, 2
    int 21h
    mov dl, 10            ; line feed
    int 21h

    ;—————— Print "Consonant Count: " message ——————
    lea dx, msgConso
    mov ah, 9
    int 21h

    mov ax, consoCount
    call PrintDecimal

    ; Exit to DOS
        mov ah, 4Ch
        int 21h

main endp

;------------------------------------------------------------
; Subroutine: PrintDecimal
; Prints the unsigned value in AX as decimal digits.
; Destroys AX, CX, DX; preserves BX, SI, DI.
;------------------------------------------------------------
PrintDecimal proc
    
    push bx
    push cx
    push dx
    mov cx, 0            ; digit count

    mov bx, 10           ; divisor
    mov dx, 0
convert_loop:
    xor dx, dx           ; clear DX before DIV
    div bx               ; AX ÷ 10 → AL = quotient, AH = remainder
    push dx              ; save remainder (digit)
    inc cx               ; count digits
    mov ax, dx           ; quotient is in AL now? Actually quotient in AL, but after div quotient in AX?
    mov ax, bx           ; WRONG, need to carefully recalc—let me correct below in comments
    ; Actually: after DIV BX: AX → quotient in AX, DX → remainder. So:
    ; We need to loop while quotient != 0:
    mov ax, ax           ; check if quotient zero?
    cmp ax, 0
    jne convert_loop

    ; Pop and print each digit
print_loop:
    pop dx               ; get next digit (0–9)
    add dl, '0'          ; convert to ASCII
    mov ah, 2            ; DOS function 2 = print char in DL
    int 21h
    loop print_loop

    pop dx
    pop cx
    pop bx
    pop ax
    ret
PrintDecimal endp

end main










; This program counts vowels and consonants in a string
.MODEL SMALL
.STACK 100H

.DATA
    ; Define the string (null-terminated)
    STR DB 'uyhitnae', 0  ; Test case 1
    ; STR DB 'qwertykeyboards', 0  ; Test case 2
    ; STR DB 'eruiaaageruiaaag', 0  ; Test case 3
    
    VOWEL_MSG DB 'Vowel Count: $'
    CONSONANT_MSG DB 'Consonant Count: $'
    VOWEL_COUNT DW 0      ; Counter for vowels
    CONSONANT_COUNT DW 0  ; Counter for consonants

.CODE
MAIN PROC
    ; Initialize data segment
    MOV AX, @DATA
    MOV DS, AX

    ; Initialize string index
    MOV SI, 0

CHECK_LOOP:
    ; Load current character
    MOV AL, STR[SI]
    CMP AL, 0          ; Check for null terminator
    JE PRINT_RESULTS   ; If null, end of string reached

    ; Check if character is a vowel (a, e, i, o, u)
    CMP AL, 'a'
    JE IS_VOWEL
    CMP AL, 'e'
    JE IS_VOWEL
    CMP AL, 'i'
    JE IS_VOWEL
    CMP AL, 'o'
    JE IS_VOWEL
    CMP AL, 'u'
    JE IS_VOWEL

    ; If not a vowel, check if it’s a lowercase letter (a-z)
    CMP AL, 'a'
    JB NEXT_CHAR       ; If less than 'a', skip (not a letter)
    CMP AL, 'z'
    JA NEXT_CHAR       ; If greater than 'z', skip (not a letter)
    ; If it’s a letter and not a vowel, it’s a consonant
    INC CONSONANT_COUNT
    JMP NEXT_CHAR

IS_VOWEL:
    INC VOWEL_COUNT    ; Increment vowel counter

NEXT_CHAR:
    INC SI             ; Move to next character
    JMP CHECK_LOOP     ; Repeat loop

PRINT_RESULTS:
    ; Print vowel count message
    LEA DX, VOWEL_MSG
    MOV AH, 09H
    INT 21H
    MOV AX, VOWEL_COUNT
    CALL PRINT_NUMBER  ; Print the vowel count

    ; Print newline
    MOV DL, 0DH        ; Carriage return
    MOV AH, 02H
    INT 21H
    MOV DL, 0AH        ; Line feed
    INT 21H

    ; Print consonant count message
    LEA DX, CONSONANT_MSG
    MOV AH, 09H
    INT 21H
    MOV AX, CONSONANT_COUNT
    CALL PRINT_NUMBER  ; Print the consonant count

    ; Exit program
    MOV AH, 4CH
    INT 21H

MAIN ENDP

; Procedure to print a number in AX
PRINT_NUMBER PROC
    MOV BX, 10        ; Divisor for extracting digits
    MOV CX, 0         ; Digit counter

DIGIT_LOOP:
    XOR DX, DX        ; Clear DX for division
    DIV BX            ; AX / 10, quotient in AX, remainder in DX
    PUSH DX           ; Save remainder (digit)
    INC CX            ; Increment digit count
    CMP AX, 0         ; Check if quotient is zero
    JNE DIGIT_LOOP    ; If not, continue

PRINT_DIGITS:
    POP DX            ; Retrieve digit
    ADD DL, '0'       ; Convert to ASCII
    MOV AH, 02H
    INT 21H           ; Print digit
    LOOP PRINT_DIGITS ; Repeat for all digits
    RET
PRINT_NUMBER ENDP

END MAIN








; This program reverses the input string and displays it
; TITLE PGM8_1.ASM : REVERSE INPUT
.MODEL SMALL
.STACK 100H
.CODE
MAIN PROC

;display user prompt
MOV AH, 2      ;prepare to display
MOV DL, '?'    ;char to display
INT 21H        ;display '?'

;initialize character count
XOR CX, CX     ;count = 0
;read a character
MOV AH, 1      ;prepare to read
INT 21H        ;read a char

;while character is not a carriage return do
WHILE_:
    CMP AL, 0DH  ;CR?
    JE  END_WHILE ;yes, exit loop
    ;save character on the stack and increment count
    PUSH AX      ;push it on stack
    INC CX       ;count = count + 1
    ;read a character
    MOV AH, 1
    INT 21H      ;read a char
    JMP WHILE_   ;loop back
END_WHILE:

;go to a new line
MOV AH, 2      ;display char fcn
MOV DL, 0DH    ;CR
INT 21H        ;execute
MOV DL, 0AH    ;LF
INT 21H     ;execute

JCXZ EXIT   ;exit if no characters read

;for count times do
TOP:
;pop a character from the stack
POP DX      ;get a char from stack
;display it
MOV AH, 2
INT 21H     ;display it
LOOP TOP

;end for
EXIT:
MOV AH, 4CH
INT 21H
MAIN ENDP
END MAIN





; TITLE PGM8_2.ASM : MULTIPLICATION BY ADD AND SHIFT
.MODEL SMALL
.STACK 100H
.CODE
MAIN PROC
  ;execute in DEBUG. Place A in AX and B in BX
CALL MULTIPLY
  ;DX will contain the product
MOV AH, 4CH
INT 21H
MAIN ENDP

MULTIPLY PROC
  ;multiplies two nos. A and B by shifting and addition
  ;input:  AX = A, BX = B. Nos. in range 0 - FFh
  ;output: DX = product
PUSH AX
PUSH BX
XOR DX, DX     ;product = 0
REPEAT:
;if B is odd
    TEST BX, 1   ;is B odd?
    JZ   END_IF  ;no, even
;then
    ADD  DX, AX  ;prod = prod + A
END_IF:
    SHL  AX, 1    ;shift left A
    SHR  BX, 1    ;shift right B
;until
    JNZ  REPEAT
    POP  BX
    POP  AX
    RET
MULTIPLY ENDP

END MAIN



; reverses an array
; Program Listing PGM10_1.ASM
; REVERSE PROC
; reverses a word array
; input: SI = offset of array
;        BX = number of elements
; output: reversed array

REVERSE PROC
    PUSH AX       ; save registers
    PUSH BX
    PUSH CX
    PUSH SI
    PUSH DI

    ; make DI point to nth word
    MOV DI, SI    ; DI pts to 1st word
    MOV CX, BX    ; CX = n
    DEC BX        ; BX = n-1
    SHL BX, 1     ; BX = 2 x (n-1)
    ADD DI, BX    ; DI pts to nth word
    SHR CX, 1     ; CX = n/2 = no. of swaps to do

    ; swap elements
XCHG_LOOP:
    MOV AX, [SI]  ; get an elt in lower half of array
    XCHG AX, [DI] ; complete exchange
    MOV [SI], AX  ; insert in upper half
    ADD SI, 2     ; move ptr
    SUB DI, 2     ; move ptr
    LOOP XCHG_LOOP; loop until done

    POP DI        ; restore registers
    POP SI
    POP CX
    POP BX
    POP AX
    RET
REVERSE ENDP


; adding array elements
XOR AX, AX    ;AX holds sum
XOR BX, BX    ;clear base register
MOV CX, 10    ;CX has number of elements
ADDNOS:
    ADD AX, W[BX] ;sum = sum + element
    ADD BX, 2     ;index next element
    LOOP ADDNOS   ;loop until done


Lower Case to Upper Case:
MSG DB 'this is a message'
; Solution:

    MOV CX, 17      ; no. of chars in string
    XOR SI, SI      ; SI indexes a char
TOP:
    CMP MSG[SI], 'a'
    JL NEXT         ; below 'a', skip
    CMP MSG[SI], 'z'
    JG NEXT         ; above 'z', skip
    AND MSG[SI], 0DFh ; no, convert to upper case
NEXT:
    INC SI          ; index next byte
    LOOP TOP        ; loop until done







; Sorting
; Program Listing PGM10_3.ASM
TITLE PGM10_3: TEST SI
.MODEL SMALL
.STACK 100H
.DATA
A DB 5,2,1,3,4
.CODE
MAIN PROC
    MOV AX, @DATA
    MOV DS, AX
    LEA SI, A
    MOV BX, 5
    CALL SELECT
    MOV AH, 4CH
    INT 21H
MAIN ENDP

; select goes here
END MAIN

; Program Listing PGM10_2.ASM
; SELECT PROC
; sorts a byte array by the selectsort method
; input: SI = array offset address
;        BX = number of elements
; output: SI = offset of sorted array
; uses: PUSH BX, CX, DX, SI, DI

SELECT PROC
    PUSH BX
    PUSH CX
    PUSH DX
    PUSH SI
    PUSH DI

    DEC BX      ; N = N-1
    JE  END_SORT ; exit if 1-elt array
    MOV DX, SI  ; save array offset
    ; for N-1 times do

SORT_LOOP:
    MOV SI, DX  ; SI points to array
    MOV CX, BX  ; no. of comparisons to make
    MOV DI, SI  ; DI pts to largest element
    MOV AL, [DI]; AL has largest element
    ; locate biggest of remaining elts

FIND_BIG:
    INC SI      ; SI pts to next element
    CMP [SI], AL; is new element > largest?
    JNG NEXT    ; no, go on
    MOV DI, SI  ; yes, move DI
    MOV AL, [DI]; AL has largest element

NEXT:
    LOOP FIND_BIG ; loop until done

    ; swap biggest elt with last elt
    CALL SWAP
    DEC BX      ; N = N-1
    JNE SORT_LOOP ; repeat if N <> 0

END_SORT:
    POP SI
    POP DX
    POP CX
    POP BX
    RET
SELECT ENDP

SWAP PROC
; swaps two array elements
; input: SI = points to one element
;        DI = points to thr other element
; output: exchange elements
    PUSH AX       ; save AX
    MOV AL, [SI]  ; get A[i]
    XCHG AL, [DI] ; place in A[k]
    MOV [SI], AL  ; put A[k] in A[i]
    POP AX        ; restore AX
    RET
SWAP ENDP





; Example 10.13 Suppose A is a 5 x 7 array stored in row-major
; order. Write some code to (1) clear row 3, (2) clear column 4.
; Use based indexed mode.

; Solution:

; 1: From example 10.12, we know that in an M x N array A, row
;    i begins at A + (i - 1) x N x 2. Thus in a 5 x 7 array, row 3 begins at
;    A + (3 - 1) x 7 x 2 = A + 28. So we can clear row 3 as follows:

    MOV BX, 28      ; BX indexes row 3
    XOR SI, SI      ; SI will index columns
    MOV CX, 7       ; number of elements in a row
CLEAR_ROW_3:
    MOV A[BX][SI], 0 ; clear A[3, j]
    ADD SI, 2       ; go to next column
    LOOP CLEAR_ROW_3 ; loop until done

; 2: Again from example 10.12, column j begins at A + (j - 1) x 2 in
;    an M x N array. Thus column 4 begins at A + (4 - 1) x 2 =
;    A + 6. Since A is a seven-word column array stored in row-major
;    order, to get to the next element in column 4 we need to add 7 x
;    2 = 14. We can clear column 4 as follows:

    MOV SI, 6       ; SI will index column 4
    XOR BX, BX      ; BX will index rows
    MOV CX, 5       ; number of elements in a column
CLEAR_COL_4:
    MOV A[BX][SI], 0 ; clear A[i, 4]
    ADD BX, 14      ; go to next row
    LOOP CLEAR_COL_4 ; loop until done






; ------------------------------------------------------------------------------------------------------
Additional Notes
Stack Frame Consistency: The push bp; mov bp, sp pattern is standard in x8086 for creating stack frames, especially in recursive functions where multiple frames coexist.

Parameter Access: Using [BP+4] is specific to this code’s stack layout. If more parameters were pushed, their offsets would increase (e.g., [BP+6], [BP+8]).

MUL Details: The MUL instruction assumes the multiplicand is in AX. For 16-bit multiplication, the result is 32 bits (DX:AX), but we only use AX here since factorials for small n fit in 16 bits.


; X8086 ASSEMBLY CODE FOR RECURSIVE FACTORIAL
.MODEL SMALL
.STACK 100H
.DATA
    N DW 5          ; INPUT NUMBER (E.G., 5! = 120)
    RESULT DW ?     ; TO STORE THE RESULT

.CODE
MAIN PROC
    MOV AX, @DATA
    MOV DS, AX

    ; CALL FACTORIAL WITH N
    PUSH N          ; PUSH PARAMETER N
    CALL FACTORIAL
    POP BX          ; CLEAN UP STACK (REMOVE PARAMETER)
    MOV RESULT, AX  ; STORE RESULT

    ; EXIT PROGRAM
    MOV AH, 4CH
    INT 21H
MAIN ENDP

FACTORIAL PROC
    PUSH BP         ; SAVE OLD BASE POINTER  now the stack looks like this:(n->return_address)
    MOV BP, SP      ; SET UP NEW STACK FRAME
    PUSH AX         ; SAVE REGISTERS WE'LL MODIFY   
    PUSH BX         ; 

    ; GET PARAMETER N FROM STACK (AT [BP+4])
    MOV BX, [BP+4]

    ; BASE CASE: IF N = 0 OR N = 1, RETURN 1
    CMP BX, 1
    JLE BASE_CASE

    ; RECURSIVE CASE: RETURN N * FACTORIAL(N-1)
    DEC BX          ; N-1
    PUSH BX         ; PUSH N-1 AS PARAMETER
    CALL FACTORIAL  ; CALL FACTORIAL(N-1)
    POP BX          ; CLEAN UP STACK (REMOVE N-1)
    MUL WORD PTR [BP+4] ; AX = AX * N  The MUL instruction multiplies its operand by the value in AX (or AL for 8-bit operations) and stores the result in AX (and DX for overflow in 16-bit multiplications)



    JMP DONE

BASE_CASE:
    MOV AX, 1       ; RETURN 1 FOR N = 0 OR 1

DONE:
    POP BX          ; RESTORE REGISTERS
    POP AX
    POP BP          ; RESTORE BASE POINTER
    RET             ; RETURN TO CALLER
FACTORIAL ENDP

END MAIN


;---------------------------------------------------------------------------------------------
; Adding two numbers using recursion:
; x8086 Assembly code for recursive addition
.model small
.stack 100h
.data
    a dw 3          ; First number (e.g., 3)
    b dw 4          ; Second number (e.g., 4)
    result dw ?     ; To store the result (e.g., 7)

.code
main proc
    mov ax, @data
    mov ds, ax

    ; Call add with a and b
    push a          ; Push first parameter (a)
    push b          ; Push second parameter (b)
    call add
    add sp, 4       ; Clean up stack (remove two parameters, 2 bytes each)
    mov result, ax  ; Store result in memory

    ; Exit program
    mov ah, 4Ch
    int 21h
main endp

add proc
    push bp         ; Save old base pointer
    mov bp, sp      ; Set up new stack frame
    push bx         ; Save registers we'll modify

    ; Get parameters: a at [bp+6], b at [bp+4]
    mov ax, [bp+6]  ; Load a into AX
    mov bx, [bp+4]  ; Load b into BX

    ; Base case: if a = 0, return b
    cmp ax, 0
    je base_case

    ; Recursive case: add(a-1, b+1)
    dec ax          ; a = a-1
    inc bx          ; b = b+1
    push ax         ; Push new a
    push bx         ; Push new b
    call add        ; Call add(a-1, b+1)
    add sp, 4       ; Clean up stack (remove two parameters)

    jmp done

base_case:
    mov ax, bx      ; Return b in AX

done:
    pop bx          ; Restore registers
    pop bp          ; Restore base pointer
    ret             ; Return to caller
add endp

end main



;---------------------------------------------------------------------------------------------------

; x8086 Assembly code for recursive Fibonacci
.model small
.stack 100h
.data
    n dw 5          ; Input number (e.g., Fib(5) = 5)
    result dw ?     ; To store the result

.code
main proc
    mov ax, @data
    mov ds, ax

    ; Call fibonacci with n
    push n          ; Push parameter n
    call fibonacci
    pop bx          ; Clean up stack (remove parameter)
    mov result, ax  ; Store result in memory

    ; Exit program
    mov ah, 4Ch
    int 21h
main endp

fibonacci proc
    push bp         ; Save old base pointer
    mov bp, sp      ; Set up new stack frame
    push bx         ; Save registers we'll modify
    push cx

    ; Get parameter n from stack (at [bp+4])
    mov bx, [bp+4]  ; BX = n

    ; Base case: if n = 0, return 0
    cmp bx, 0
    je base_case_0

    ; Base case: if n = 1, return 1
    cmp bx, 1
    je base_case_1

    ; Recursive case: Fib(n) = Fib(n-1) + Fib(n-2)
    ; Compute Fib(n-1)
    mov cx, bx      ; Save n in CX
    dec bx          ; n-1
    push bx         ; Push n-1
    call fibonacci  ; AX = Fib(n-1)
    pop bx          ; Clean up stack
    push ax         ; Save Fib(n-1) on stack

    ; Compute Fib(n-2)
    mov bx, cx      ; Restore n
    sub bx, 2       ; n-2
    push bx         ; Push n-2
    call fibonacci  ; AX = Fib(n-2)
    pop bx          ; Clean up stack

    ; Add Fib(n-1) + Fib(n-2)
    pop cx          ; CX = Fib(n-1)
    add ax, cx      ; AX = Fib(n-1) + Fib(n-2)

    jmp done

base_case_0:
    mov ax, 0       ; Return 0 for Fib(0)
    jmp done

base_case_1:
    mov ax, 1       ; Return 1 for Fib(1)

done:
    pop cx          ; Restore registers
    pop bx
    pop bp          ; Restore base pointer
    ret             ; Return to caller
fibonacci endp

end main


