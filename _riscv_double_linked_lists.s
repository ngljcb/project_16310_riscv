.data	

# Command references used to check if the correct formats are followed.
add:		.string		"ADD(#)"
del:		.string 	"DEL(#)"
print:		.string 	"PRINT"
rev:		.string 	"REV"
sort:		.string 	"SORT"



# Various messages used within the program.
space:		.word		0
verifying:	.string 	"\n>> Validating operation > "
executing:	.string 	"\n>> SUCCESS. Valid function identified! \n>> Executing function ... "
invalid:	.string 	"\n>> ERROR. Invalid function! \n"
added:		.string 	"\n>> Added char = '"
addedIn:	.string 	"' in address "
deleted:	.string 	"\n>> Element found! Delete complete! \n"
notFound:	.string 	"\n>> Element not found! Command complete! \n"
sorted:		.string 	"\n>> Sorting complete!\n"
printing:	.string 	"\n>> Printing: "
reversed:	.string 	"\n>> Reversing complete!"
noElem:		.string		"\n>> No elements available!"
running:	.string 	"\n>> Running: Linked List Simulation. \n"
inputsOk:	.string 	"\n>> Valid number of commands identified. Proceed to command verification & execution. \n"
inputerror:	.string 	"\n>> Invalid input: maximum number[30] of commands has been exceeded! \n"
fine:		.string 	"\n>> Program terminated."
newLine:	.string		"\n"



##############     TEST INPUTS     ##############

listInput:		.string 	"ADD(1) ~ ADD(a) ~ ADD(a) ~ ADD(B) ~ ADD(;) ~ ADD(9) ~PRINT~SORT~PRINT~DEL(b) ~DEL(B) ~PRI~REV~PRINT"
# listInput:		.string 	"ADD(1) ~ ADD(a) ~ ADD() ~ ADD(B) ~ ADD ~ ADD(9) ~PRINT~SORT(a)~PRINT~DEL(bb) ~DEL(B) ~PRINT~REV~PRINT"
# listInput:		.string 	""
# listInput:		.string 	"~~~"
# listInput:		.string 	"REV ~PRINT ~ SORT~DEL(2)"
# listInput:		.string 	"ADD(1)~DEL(2)~add(3)~del(4)~SORT(5)~ADD(6)~DEL(7)~add(8)~del(9)~SORT(10)~DD(11)~DEL(12)~add(13)~del(14)~SORT(15)~ADD(16)~DEL(17)~add(18)~del(19)~SORT(20)~aDD(21)~DEL(22)~add(23)~del(24)~SORT(25)~ADD(26)~DEL(27)~add(28)~del(29)~SORT(30)~add(31)"
# listInput:		.string 	"# ADD(1)~ ADD(a)~ ADD(,) ~ add(a) ~ADD(a) ~ ADD(B) ~ PRiNT ~DEL(a) ~ REV() ~ ~ DEL(A)  ~ ADD(9)~ ADD()) ~ print ~ ADD(}) ~PRI~REV~PRINT ~SORT ~PRINT~ PRINvT "



.text

la s0, listInput					# array of commands to be executed
li s1, 0xFFFFFFFF					# PTAIL - last element pointer
li s2, 0xFFFFFFFF					# PHEAD - first element pointer
									# s7 cmd format address
li s11, 0							# listInput pointer
li s10, 0							# listInput pointer value
li s9, 1							# no_error [ 1 - true, 0 - false ]
li s8, 0							# DATA char value for ADD & DEL
li s6, 0							# cmd_status [ 0 - NULL or no valid command found, 65 - ADD, 68 - DEL, 80 - PRINT, 82 - REV, 83 - SORT ]
li s5, 0x0011						# default LFSR
li s4, 0							# element counter




MAIN:
jal msg_running

verify_command_numbers:
add a2, s0, zero
jal func_counter					# func_counter(listInput) - Returns the number of commands within the listInput.
li t0, 30
blt a0, t0, inputs_ok				# If commands are greater than 30, interrupt the program. Else proceed executing the commands on listInput.
jal msg_inputerror
j end

inputs_ok:		jal msg_inputsOk
execute_inputs: jal msg_verifying

while_has_char:
add t4, s0, s11
lb s10, 0(t4)						# Get the current value on listInput pointer.
jal msg_print_data
beq s10, zero, Phase4_Exec			# If no char left from listInput, verify cmd_status & execute last command.
beq s9, zero, Phase3_TildaCheck		# If no_error == false, find tilda & execute error message
bgt s6, s9, Phase2_CmdFormatCheck	# If cmd_status > 1, & has a potential function, verify function format.

Phase1_Ident_1st_Letter:			# PHASE 1 - Verifies the first character of the command & setup the cmdAddress accordingly for reference to check if the format is correct.
addi s11, s11, 1					# Set listInput pointer to 1.
li t0, 1							# Set cmdAddress pointer to 1 which is the second letter of the reference array.
li t1, 32
beq s10, t1, while_has_char			# If it encounters a ' ', a space character, return to phase1 and continue searching for a valid first letter.
li t1, 65
beq s10, t1, cmd_A					# If it starts with 'A', set the cmdAddress to ADD(#) as the potential command to execute, then proceed to phase2.
li t1, 68
beq s10, t1, cmd_D					# If it starts with 'D', set the cmdAddress to DEL(#) as the potential command to execute, then proceed to phase2.
li t1, 80
beq s10, t1, cmd_P					# If it starts with 'P', set the cmdAddress to PRINT as the potential command to execute, then proceed to phase2.
li t1, 82
beq s10, t1, cmd_R					# If it starts with 'R', set the cmdAddress to REV as the potential command to execute, then proceed to phase2.
li t1, 83
beq s10, t1, cmd_S					# If it starts with 'S', set the cmdAddress to SORT as the potential command to execute, then proceed to phase2.
li t1, 126
beq s10, t1, type2_error			# If it encounters a '~', a null function, send an error message & return to phase1.
j type1_error						# If it encounters all other invalid characters, set an error status & continue checking until tilda(~) is found.

	cmd_A:	li s6, 65				# Set the cmd status to ADD.
			la s7, add				# Set the cmdAddress to ADD(#) as the reference array.
			j while_has_char
	cmd_D:	li s6, 68				# Set the cmd status to DEL.
			la s7, del				# Set the cmdAddress to DEL(#) as the reference array.
			j while_has_char
	cmd_P:	li s6, 80				# Set the cmd status to PRINT.
			la s7, print			# Set the cmdAddress to PRINT as the reference array.
			j while_has_char
	cmd_R:	li s6, 82				# Set the cmd status to REV.
			la s7, rev				# Set the cmdAddress to REV as the reference array.
			j while_has_char
	cmd_S:	li s6, 83				# Set the cmd status to SORT.
			la s7, sort				# Set the cmdAddress to SORT as the reference array.
			j while_has_char

	type1_error:	li s6, 0		# Set the cmd status to NULL.
					li s9, 0		# Set the phase tracker to 0, which signals that an error is found.
					j while_has_char
	type2_error:	jal msg_error
					j end_exec


Phase2_CmdFormatCheck:				# PHASE 2 - Verifies the rest of the characters of the command using an array in cmdAddress as a reference if they follow the correct format. 
add t1, t0, s7
lb t1, 0(t1)						# Get the current value on cmdAddress pointer.
beq t1, zero, Phase3_TildaCheck		# If all the letters of cmdAddress are verified & corrisponds correctly, go to phase3 to look for tilda(~).
addi t0, t0, 1						# Increment cmdAddress pointer by 1.
addi s11, s11, 1					# Increment listInput pointer by 1.
li t2, 35			
bne t1, t2, verify					# If the character '#' is encountered on the cmdAddress, save the value from the current listInput pointer which is the DATA (input value).
add s8, s10, zero					# Save the DATA to s8(input value for ADD or DEL commands).
j while_has_char
verify:
beq t1, s10 while_has_char			# While the values of cmdAddress & listinput corrisponds correctly, continue verifying.
li t2, 126
beq s10, t2, type2_error			# If the command is not fully verified & a tilda is met, then the command is invalid. Send an error message & return to phase1.
j type1_error						# If it encounters all other invalid characters, set an error status & continue checking until a tilda(~) is found.


Phase3_TildaCheck:					# PHASE 3 - Verifies the rest of the characters of the listInput until a tilda(~) is found.
addi s11, s11, 1					# Increment listInput pointer by 1.
li t2, 32
beq s10, t2 while_has_char			# If it encounters a ' ', a space character, continue checking until tilda(~) is found.
li t2, 126
beq s10, t2 Phase4_Exec				# If tilda(~) is found, go to phase4 & execute the function.
j type1_error						# If it encounters all other invalid characters, set an error status & continue checking until tilda(~) is found.


Phase4_Exec:
bne s6, zero, execute_command
bne s10, zero, type2_error			# If the listInput still has other commands & an error is found, execute an error message.
jal msg_error						# If the last command has an error execute an error message & end the program.
j end
execute_command:
jal msg_executing
j setup&call_func					# Setup the arguments & call the current correct function.
end_exec:
beq s10, zero, end
li s9, 1
li s6, 0
j execute_inputs

	setup&call_func:
	li t0, 65
	beq s6, t0, call_ADD
	beq s4, zero, err_noElem
	li t0, 68
	beq s6, t0, call_DEL
	li t0, 80
	beq s6, t0, call_PRINT
	li t0, 82
	beq s6, t0, call_REV
	li t0, 83
	beq s6, t0, call_SORT
	j end_exec
	call_ADD:	add a2, s8, zero	# DATA
				add a3, s5, zero	# lfsr
				add a4, s1, zero	# PTAIL, the address of the last element.
				add a5, s2, zero	# PHEAD, the address of the last element.
				jal Func_ADD
				addi s4, s4, 1		# Increment DATA counter by 1
				add s1, a0, zero	# Updates the PTAIL
				add s5, a1, zero	# Updates the SEED
				lw s2, 0(sp)		# Updates the PHEAD
				addi sp, sp, 4
				j end_exec
	call_DEL:	add a2, s8, zero	# Set DATA as function argument.
				add a3, s2, zero	# Set PHEAD as function argument.
				add a4, s1, zero	# Set PTAIL as function argument.
				add a5, s4, zero	# Set data counter as function argument.
				jal Func_DEL
				add s1, a0, zero	# Updates the PTAIL
				add s2, a1, zero	# Updates the PHEAD
				lw s4, 0(sp)		# Updates DATA counter
				addi sp, sp, 4
				j end_exec
	call_PRINT:	jal msg_printing
				add a2, s2, zero	# Set PHEAD as function argument.
				jal Func_PRINT
				j end_exec
	call_REV:	add a2, s2, zero	# Set PHEAD as function argument.
				jal Func_REV
				add s2, a0, zero	# Updates the PHEAD
				j end_exec
	call_SORT:	add a2, s2, zero	# set phead as function argument1
				add a3, s4, zero	# set n(number of data) as function argument2
				jal Bubblesort
				jal msg_sorted
				j end_exec
	err_noElem:	jal msg_noElem
				j end_exec

# END MAIN











# ==================================================================================
# - @name		func_counter(char[] listInput)
# - @descript.	Counts the number of commands within the listInput.
# - @args		char[]	a2 - listInput
# - @return		int 	a0 - number_of_commands

func_counter:
li t0, 0
li t1, 126
li t3, 0
funz_counter_while:
add t2, a2, t0
lb t2, 0(t2)
beq t2, zero, funz_counter_end
addi t0, t0, 1
bne t2, t1, funz_counter_while
addi t3, t3, 1
j funz_counter_while
funz_counter_end:
add a0, t3, zero
jr ra




# ==================================================================================
# - @name		Func_ADD(char DATA, byte lfsr, byte PTAIL, byte PHEAD)
# - @descript.	Inserts the DATA char to the linked list.
# - @args		char 	a2 - DATA
# - @args		byte 	a3 - lfsr
# - @args		byte 	a4 - PTAIL
# - @args		byte 	a5 - PHEAD
# - @return		byte	a0 - new_PTAIL
# - @return		byte	a1 - new_lfsr
# - @return		byte	sp - new_PHEAD

Func_ADD:
addi sp, sp, -8
sw a2, 4(sp)			# Save DATA to the stack memory.
sw ra, 0(sp)			# Save return address to the stack memory.

add a2, a3, zero		# Setup lfsr argument.
jal Get_new_address		# Call Get_new_address(lfsr) function.
add t0, a0, zero		# New address where DATA must be stored.
add t3, a1, zero		# New lfsr
lw t2, 4(sp)			# Retrieve DATA from the stack memory.

insertData:
li t1, 0xFFFFFFFF
sw a4, 0(t0)			# Store PBACK
sb t2, 4(t0)			# Store DATA
sw t1, 5(t0)			# Store PAHEAD

add a0, t0, zero
jal msg_add1

beq a4, t1, setHead		# IF PTAIL == 0xFFFFFFFF, set PHEAD.
sw t0, 5(s1)			# Update the PAHEAD of the previous element with the new address of the new element.
sw a5, 4(sp)			# return value - PHEAD
j end_add
setHead:
sw t0, 4(sp)			# return value - PHEAD

end_add:
add a0, t0, zero		# return value - PTAIL
add a1, t3, zero		# return value - lfsr
lw ra, 0(sp)
addi sp, sp, 4
jr ra




# ==================================================================================
# - @name		Get_new_address(byte lfsr)
# - @descript.	Generates a new memory address.
# - @args		byte	a2 - lfsr
# - @return		byte	a0 - new_address
# - @return		byte	a1 - new_LFSR

Get_new_address:
add t0, a2, zero

calculate_LFSR:
srli t1, t0, 2			# lfsr >> 2
xor t2, t0, t1			# newBit = lfsr >> 0 XOR lfsr >> 2
srli t1, t0, 3			# lfsr >> 3
xor t2, t2, t1			# newBit = newBit XOR lfsr >> 3
srli t1, t0, 5			# lfsr >> 5
xor t2, t2, t1			# newBit = newBit XOR lfsr >> 5
slli t2, t2, 15			# newBit = newBit << 15
srli t1, t0, 1			# lfsr >> 1
or t0, t2, t1			# newBit = newBit OR lfsr >> 2

trim_16bit:
li t1, 0xFFFF0000
or t0, t0, t1
xor t0, t0, t1

new_address:
li t1, 0x00010000
or t2, t1, t0			# newAddress = 0x001000 OR 16-bit-LFSR

check_address:
lb t4, 9(t2)
bne t4, zero, calculate_LFSR
lw t4, 0(t2)
beq t4, zero, end_get_new_address
j calculate_LFSR

end_get_new_address:
add a0, t2, zero		# return value - new address
add a1, t0, zero		# return value - new LFSR
jr ra




# ==================================================================================
# - @name		Func_DEL(char DATA, byte PHEAD, byte PTAIL, int element_number)
# - @descript.	Find the first element which corrisponds to the DATA within the linked list & eliminates it.
# - @args		char	a2 - DATA
# - @args		byte 	a3 - PHEAD
# - @args		byte	a4 - PTAIL
# - @args		int		a5 - data_counter
# - @return		byte 	a0 - new_PTAIL
# - @return		byte 	a1 - new_PHEAD
# - @return		int 	sp - new_value_data_counter

Func_DEL:
addi sp, sp, -4
sw ra, 0(sp)
add t0, a3, zero		# PHEAD - the address of the first element.

fetchData:
lb t1, 4(t0)
bne t1, a2, checkNext

savePointers:
lw t2, 0(t0)
lw t3, 5(t0)

deleteChar:
sw zero, 0(t0)
sb zero, 4(t0)
sw zero, 5(t0)

li t4, 0xFFFFFFFF

fixPHead:
bne t3, t4, fixPTail
sw t4, 5(t2)			# beq t3 FFFF
add t0, t2, zero
add t1, a3, zero
j _deleted

fixPTail:
bne t2, t4, fixPHeadTail
sw t4, 0(t3)			# beq t2 FFFF
add t0, a4, zero
add t1, t3, zero
j _deleted

fixPHeadTail:
add t5, t2, zero
add t6, t3, zero
sw t6, 5(t2)			# bne t2 FFFF
sw t5, 0(t3)			# bne t3 FFFF
add t0, a4, zero
add t1, a3, zero
j _deleted

checkNext:
li t4, 0xFFFFFFFF
lw t1, 5(t0)
beq t1, t4, char_not_found
add t0, t1, zero
j fetchData

char_not_found:
jal msg_notFound
add a0, a4, zero		# return value - PTAIL
add a1, a3, zero		# return value - PHEAD
j _end_del

_deleted:
jal msg_deleted
addi a5, a5, -1
add a0, t0, zero		# return value - PTAIL
add a1, t1, zero		# return value - PHEAD

_end_del:
lw ra, 0(sp)
sw a5, 0(sp)			# return value - data_counter
jr ra




# ==================================================================================
# - @name		Func_PRINT(byte PHEAD)
# - @descript.	Prints every element of the linked list to the console.
# - @args		byte	a2 - PHEAD
# - @return		null

Func_PRINT:
addi sp, sp, -4
sw ra, 0(sp)

add t0, a2, zero
li t2, 0xFFFFFFFF

print_data:
lb a0, 4(t0)
li a7, 11
ecall

lw t1, 5(t0)
beq t1, t2, end_print
add t0, t1, zero
j print_data

end_print:
jal msg_newline
lw ra, 0(sp)
addi sp, sp, 4
jr ra




# ==================================================================================
# - @name		Func_REV(byte PHEAD)
# - @descript.	Changes the position of every element of the linked list in reverse order by switching its pointers.
# - @args		byte	a2 - PHEAD
# - @return		byte	a0 - new_PHEAD

Func_REV:
addi sp, sp, -4
sw ra, 0(sp)

add t0, a2, zero

load_pointers_to_temp:
lw t2, 0(t0)
lw t3, 5(t0)

swap_pointers:
sw t3, 0(t0)
sw t2, 5(t0)

li t4, 0xFFFFFFFF
beq t3, t4, rev_end
add t0, t3, zero
j load_pointers_to_temp

rev_end:
jal msg_reversed
add a0, t0, zero		# return value - phead address
lw ra, 0(sp)
addi sp, sp, 4
jr ra




# ==================================================================================
# - @name		Bubblesort(byte PHEAD, int data_counter)
# - @descript.	Sorts the elements of the linked list.
# - @args		byte	a2 - PHEAD
# - @args		int 	a3 - data_counter
# - @return		null

Bubblesort:				# bubblesort(arr[], n)
li t5, 1

base_case:				# n==1
bne a3, t5, non_base_case
jr ra

non_base_case:			# n>1
li t6, 0				# i=0
addi t0, a3, -1			# n-1

while:
beq t0, t6, recursive_call

lb t2, 4(a2)			# Get arr[i]
lw t3, 5(a2)
lb t3, 4(t3)			# Get arr[i+1]

addi sp, sp, -12
sw t0, 8(sp)
sw a2, 4(sp)
sw ra, 0(sp)

add a2, t2, zero
jal Categorize			# Perform categorize(arr[i]) function.
add t4, a0, zero		# category(arr[i]) value.

add a2, t3, zero
jal Categorize			# Perform categorize(arr[i+1]) function.
add t5, a0, zero		# category(arr[i+1]) value.

lw ra, 0(sp)
lw a2, 4(sp)
lw t0, 8(sp)
addi sp, sp, 12

beq t4, t5 check_ascii	# If category(arr[i]) == category(arr[i+1]), compare their ascii values.
bgt t4, t5 swap			# If category(arr[i]) > category(arr[i+1]), then swap values.
j sort_next				# Otherwise category(arr[i]) < category(arr[i+1]), no need to swap. Proceed to next pair.

check_ascii:
bgt t2, t3 swap			# If arr[i] > arr[i+1], then swap values.
j sort_next				# Otherwise arr[i] < arr[i+1], no need to swap. Proceed to next pair.

swap:
sb t3, 4(a2)
lw t4, 5(a2)
sb t2, 4(t4)

sort_next:
addi t6, t6, 1
lw a2, 5(a2)
j while

recursive_call:
add a2, s2, zero		# head pointer
addi a3, a3, -1			# n-1

addi sp, sp, -4
sw ra, 0(sp)

jal Bubblesort			# Bubblesort(arr[], n)

lw ra, 0(sp)
addi sp, sp, 4
jr ra



# ==================================================================================
# - @name		Categorize(char DATA)
# - @descript.	Returns an int value which represents the category of the char [ 1 - spec.char, 2 - numbers, 3 - min.alpha., 4 - maus.alpha.]
# - @args		char	a2 - DATA
# - @return		int		a0 - category

Categorize:
li t0, 48
blt a2, t0, cat1
li t0, 58
blt a2, t0, cat2
li t0, 65
blt a2, t0, cat1
li t0, 91
blt a2, t0, cat4
li t0, 97
blt a2, t0, cat1
li t0, 123
blt a2, t0, cat3
cat1: li a0, 1
jr ra
cat2: li a0, 2
jr ra
cat3: li a0, 3
jr ra
cat4: li a0, 4
jr ra




################################################
##############      MESSAGES      ##############
################################################

msg_add1:		add t0, a0, zero
				la a0, added
				li a7, 4
				ecall
				addi a0, s8, 0
				li a7, 11
				ecall
				la a0, addedIn
				li a7, 4
				ecall
				addi a0, t0, 0
				li a7, 34
				ecall
				lw a0, newLine
				li a7, 11
				ecall
				jr ra

msg_verifying:	la a0, verifying
				li a7, 4
				ecall
				jr ra

msg_error:		la a0, invalid
				li a7, 4
				ecall
				jr ra

msg_executing:	la a0, executing
				li a7, 4
				ecall
				jr ra

msg_sorted:		la a0, sorted
				li a7, 4
				ecall
				jr ra

msg_printing:	la a0, printing
				li a7, 4
				ecall
				jr ra

msg_print_data:	li t4, 126
				beq s10, t4, end_msg_print_data
				addi a0, s10, 0
				li a7, 11
				ecall
				end_msg_print_data:
				jr ra

msg_deleted:	la a0, deleted
				li a7, 4
				ecall
				jr ra

msg_notFound:	la a0, notFound
				li a7, 4
				ecall
				jr ra

msg_noElem:		la a0, noElem
				li a7, 4
				ecall
				lw a0, newLine
				li a7, 11
				ecall
				jr ra

msg_reversed:	la a0, reversed
				li a7, 4
				ecall
				lw a0, newLine
				li a7, 11
				ecall
				jr ra

msg_newline:	lw a0, newLine
				li a7, 11
				ecall
				jr ra

msg_inputerror:	la a0, inputerror
				li a7, 4
				ecall
				jr ra

msg_inputsOk:	la a0, inputsOk
				li a7, 4
				ecall
				jr ra

msg_running:	la a0, running
				li a7, 4
				ecall
				jr ra




end:
la a0, fine
li a7, 4
ecall