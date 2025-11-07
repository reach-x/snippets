; Hello World in x86 Assembly (NASM syntax for Linux)

section .data
    msg db 'Hello, Assembly!', 0xA    ; message with newline
    len equ $ - msg                    ; length of message

section .text
    global _start

_start:
    ; write(1, msg, len)
    mov eax, 4          ; sys_write
    mov ebx, 1          ; stdout
    mov ecx, msg        ; message
    mov edx, len        ; message length
    int 0x80            ; syscall

    ; exit(0)
    mov eax, 1          ; sys_exit
    mov ebx, 0          ; exit code 0
    int 0x80            ; syscall
