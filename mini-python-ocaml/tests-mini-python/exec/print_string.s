	.text
my_malloc:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call malloc
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq str0, %rax
	movq -8(%rbp), %rax
	movq 0(%rax), %rsi
	leaq print_int, %rdi
	call printf
	addq $8, %rbp
	testq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
str0:
	.string "foo"
print_int:
	.string " %d\n"
