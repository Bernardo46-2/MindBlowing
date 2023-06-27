.global _start
.intel_syntax noprefix

# A: +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.

# esi: code pointer
# edi: memory pointer

.macro store_rdi_rsi
    push rdi
    push rsi
.endm

.macro retrieve_rsi_rdi
    pop rsi
    pop rdi
.endm

.macro parse_token token, err, fn
    mov bl, byte ptr [esi]
    cmp bl, \token
    jne \err
    call \fn
    jmp parse_done
.endm

    # ================================================== Globals ================================================== #

.section .data
    code_len: .word 0        # length of the code string that was read

.section .bss
    .lcomm buffer, 1000      # buffer for reading and printing 
    .lcomm code, 1000        # buffer for the code string
    buffers_len = 1000       # max length of the buffers

    memory:
        .zero 100            # memory array (change to 30000 later)


.section .text
    # ================================================== Utils ================================================== #

    parse_int:                       # edi -> buffer
        xor eax, eax                 # clear eax
        movzx esi, byte ptr [edi]    # esi = *edi
        jmp parse_int_loop_a         # start loop

    parse_int_loop_b:
        imul eax, 10                 # eax *= 10

    parse_int_loop_a:
        cmp esi, 0                   # if esi == '\0' then ..
        je parse_int_done            # .. return

        sub esi, 48                  # esi -= 48
        add eax, esi                 # eax += esi
        inc edi                      # edi++

        movzx esi, byte ptr [edi]    # esi = *edi
        cmp esi, 0                   # if esi == '\0' then ..
        jne parse_int_loop_b         # .. next iteration
    
    parse_int_done:
        ret                          # return eax


    al_to_buffer:
        store_rdi_rsi
        
        lea edi, [buffer]            # edi -> buffer
        mov byte ptr [edi], al       # *edi = al
        inc edi                      # edi++
        mov byte ptr [edi], 10       # *edi = '\n'
        inc edi                      # edi++
        mov byte ptr [edi], 0        # *edi = '\0'

        retrieve_rsi_rdi
        ret                          # return


    # ================================================== IO ================================================== #

    print:
        store_rdi_rsi

        mov eax, 1                   # write
        mov edi, 1                   # stdout
        lea esi, [buffer]            # buffer to print
        mov edx, buffers_len         # buffer len
        syscall                      # print buffer

        retrieve_rsi_rdi
        ret                          # return


    read_code:
        store_rdi_rsi
        xor eax, eax
        xor edi, edi
        lea esi, [code]
        mov edx, buffers_len
        syscall
        jmp read_test_error

    read:
        xor eax, eax                 # read
        store_rdi_rsi
        xor edi, edi                 # stdin
        lea esi, [buffer]            # buffer to read to
        mov edx, buffers_len         # buffer len
        syscall                      # read input

    read_test_error:
        cmp eax, -1                  # if read failed then ..
        jne read_clean_up            # place '\0' at the end of string

    read_handle_error:
        mov edi, 1                   # read error code: 1
        jmp done                     # exit

    read_clean_up:
        add eax, esi                 # eax += esi
        dec eax                      # eax-- (ignore '\n')
        mov byte ptr [eax], 0        # *eax = '\0'

    read_done:
        retrieve_rsi_rdi
        ret                          # return


    # ================================================== Interpreter ================================================== #

    # TODO: check if ptr is gonna loop around

    inc_cell:
        movzx ebx, byte ptr [edi]
        inc ebx
        and ebx, 0xff
        mov byte ptr [edi], bl
        ret

    dec_cell:
        dec byte ptr [edi]
        ret

    inc_ptr:
        inc edi
        ret

    dec_ptr:
        dec edi
        ret

    print_cell:
        mov al, byte ptr [edi]
        call al_to_buffer
        ret

    read_cell:
        ret


    parse_char:                                       # match *esi
    parse_plus:
        parse_token 43, parse_minus, inc_cell         # case '+'

    parse_minus:
        parse_token 45, parse_right, dec_cell         # case '-'
        
    parse_right:
        parse_token 62, parse_left, inc_ptr           # case '>'
        
    parse_left:
        parse_token 60, parse_print, dec_ptr          # case '<'
        
    parse_print:
        parse_token 46, parse_read, parse_print       # case '.'
        
    parse_read:
        parse_token 44, parse_loop_start, parse_read  # case ','
        
    parse_loop_start:
        cmp byte ptr [esi], 91       # case '['
        nop                          # TODO: loop start
        jmp parse_done
        
    parse_loop_end:
        cmp byte ptr [esi], 93       # case ']'
        nop                          # TODO: loop end
        jmp parse_done
    
    parse_done:
        ret


    parse_code:
        jmp parse_code_loop_b

    parse_code_loop_a:
        inc esi

    parse_code_loop_b:
        call parse_char
        mov eax, esi
        sub eax, [code]
        cmp eax, code_len
        jl parse_code_loop_a

    parse_code_done:
        ret


    # ================================================== Main ================================================== #

    _start:
        lea edi, [memory]            # edi -> memory array
        lea esi, [code]              # esi -> code string
        call read_code               # read code
        mov [code_len], eax          # code_len = code.len()
        call parse_code
    
    exit_success:
        xor edi, edi                 # exit code 0

    done:
        mov eax, 60                  # exit
        syscall                      # exit program
