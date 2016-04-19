	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:                                ## %.preheader
	pushq	%rbp
Ltmp0:
	.cfi_def_cfa_offset 16
Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp2:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	pushq	%rax
Ltmp3:
	.cfi_offset %rbx, -24
	movl	_arr(%rip), %edx
	leaq	_arr(%rip), %rsi
	leaq	L_.str(%rip), %rbx
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	movl	_arr+4(%rip), %edx
	leaq	_arr+4(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	movl	_arr+8(%rip), %edx
	leaq	_arr+8(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	movl	_arr+12(%rip), %edx
	leaq	_arr+12(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	movl	_arr+16(%rip), %edx
	leaq	_arr+16(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	movl	_arr+20(%rip), %edx
	leaq	_arr+20(%rip), %rsi
	xorl	%eax, %eax
	movq	%rbx, %rdi
	callq	_printf
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc

	.section	__DATA,__data
	.globl	_arr                    ## @arr
	.align	4
_arr:
	.long	1                       ## 0x1
	.long	2                       ## 0x2
	.long	3                       ## 0x3
	.long	4                       ## 0x4
	.long	5                       ## 0x5
	.long	6                       ## 0x6

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%x %d\n"


.subsections_via_symbols
