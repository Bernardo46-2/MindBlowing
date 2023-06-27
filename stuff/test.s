.global _start
.intel_syntax noprefix


.section .bss
    .lcomm buffer, 255
    buffer_len = 255


.section .text
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


    eax_to_buffer:
        lea edi, [buffer]            # edi -> buffer
        mov byte ptr [edi], al       # *edi = eax
        inc edi                      # edi++
        mov byte ptr [edi], 10       # *edi = '\n'
        inc edi                      # edi++
        mov byte ptr [edi], 0        # *edi = '\0'
        ret                          # return


    print:
        mov eax, 1                   # write
        mov edi, 1                   # stdout
        lea esi, [buffer]            # buffer to print
        mov edx, buffer_len          # buffer len
        syscall                      # print buffer
        ret                          # return


    read:
        xor eax, eax                 # read
        xor edi, edi                 # stdin
        lea esi, [buffer]            # buffer to read to
        mov edx, buffer_len          # buffer len
        syscall                      # read input
        cmp eax, -1                  # if read failed then ..
        je read_error                # .. exit with read error code
        jmp read_clean_up            # place '\0' at the end of string

    read_error:
        mov edi, 1                   # read error code: 1
        jmp done                     # exit

    read_clean_up:
        add eax, esi                 # eax += esi
        dec eax                      # eax-- (ignore '\n')
        mov byte ptr [eax], 0        # *eax = '\0'

    read_done:
        ret                          # return


    _start:
        call read                    # read input from user
        lea edi, [buffer]            # edi -> buffer
        call parse_int               # parse input
        call eax_to_buffer           # move eax to buffer
        call print                   # print that input

    exit_success:
        xor edi, edi                 # exit code 0

    done:
        mov eax, 60                  # exit
        syscall                      # exit program
