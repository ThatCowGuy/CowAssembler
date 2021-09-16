
identifiers = {
	".WORD": 4,
	".HALF": 2, ".HALFWORD": 2,
	".BYTE": 1,
	".ORG": 4, ".START": 4,
	".ASCII": 0,
}

constants_hi = {}
constants_lo = {}
labels = {}

conditioncodes = {
	"F": "0000",
	"UN": "0001",
	"EQ": "0010",
	"OLT": "0100", "LT": "0100",
	"OLE": "0110", "LE": "0100",
}

fmts = {
	"S": "10000",
	"D": "10001",
	"W": "10100",
	"L": "10101",
}

copcodes = {
	"ADD": "000000",
	"SUB": "000001",
	"MUL": "000010",
	"DIV": "000011",
	"SQRT": "000100",
	"ABS": "000101",
	"MOV": "000110",
	"NEG": "000111",
	"CVT.S": "100000",
	"CVT.W": "100100",
}

functcodes = {
	"ADD": "100000",
	"ADDU": "100001",
	"AND": "100100",
	"DIV": "011010",
	"DIVU": "011011",
	"MULT": "011000",
	"MULTU": "011001",
	"NOR": "100111",
	"OR": "100101",
	"SLL": "000000",
	"SLLV": "000100",
	"SRA": "000011",
	"SRAV": "000111",
	"SRL": "000010",
	"SRLV": "000110",
	"SUB": "100010",
	"SUBU": "100011",
	"XOR": "100110",
	"SLT": "101010",
	"SLTU": "101001",
	"MFHI": "010000",
	"MFLO": "010010",
	"MTHI": "010001",
	"MTLO": "010011",
	# REGIMM codes
	"BLTZ": "00000", #2nd = 00000 - REGIMM
	"BLTZL": "000010", #2nd = 00010 - REGIMM
	"BGEZ": "000001",  #2nd = 00001 - REGIMM
	"BGEZL": "000011",  #2nd = 00011 - REGIMM
}
# Important Pseudos
# LI xi (LUI xihi, ORI xilo)
# MUL xyz (MULT yz, MFLO x)	 also U-variant
# DIV xyz (DIV yz, MFLO x)	  also U-variant

# Formats
# Reg : oooo ooss ssst tttt dddd dggg ggff ffff
#	   opcode source source2 dest shift function
# Imm : oooo oobb bbbd dddd iiii iiii iiii iiii
#	   opcode base dest immediate/adddress
# Jum : oooo ooaa aaaa aaaa aaaa aaaa aaaa aaaa
#	   opcode address
argformats = {
	**dict.fromkeys([ # SPECIAL_target format-family
		"MFHI", "MTHI", "MFLO", "MTLO"], "SPECIAL_TARGET"),
	**dict.fromkeys([ # SPECIAL target-source-source format-family
		"MULT", "MULTU", "DIV", "DIVU", "ADD", "ADDU",
		"SUB", "SUBU", "AND", "OR", "XOR", "NOR",
		], "SPECIAL_TSS"),
	**dict.fromkeys([ # SPECIAL Shift format-family
		"SLL", "SRL", "SRA", "SLLV", "SRLV", "SRAV",
		], "SPECIAL_SHIFT"),
	**dict.fromkeys([ # JUMP
		"JAL", "J"], "JUMP"),
	**dict.fromkeys([ # NOP
		"NOP"], "NOP"),
	**dict.fromkeys([ # IMMEDIATE
		"ADDI", "ADDIU", "DADDI", "DADDIU", "SLTI", "SLTIU",
		"ANDI", "ORI", "XORI"], "IMMEDIATE"),
	**dict.fromkeys([ # REGREG_BRANCH
		"BEQ", "BNE", "BEQL", "BNEL"], "REGREG_BRANCH"),
	**dict.fromkeys([ # LOADSTORE
		"LB", "LH", "LWL", "LW", "LBU", "LWR",
		"LWU", "SB", "SH", "SWL", "SW", "SBU",
		"SHU", "SRR", "LWC1", "SWC1"], "LOADSTORE"),
	**dict.fromkeys([ # ZERO_BRANCH
		"BLTZ", "BLTZL", "BGEZ", "BGEZL", "BLEZ", "BELZL",
		"BGTZ", "BGTZL"], "ZERO_BRANCH"),
	**dict.fromkeys([ # COP1 source-source-target format-family
		"ADD.", "SUB.", "MUL.", "DIV."], "COP1_SST"),
	**dict.fromkeys([ # COP1 source-target format-family
		"SQRT.", "ABS.", "MOV.", "NEG.", "CVT.S.", "CVT.W."], "COP1_ST"),
	**dict.fromkeys([ # COP1 branch format-family
		"BC1F", "BC1FL", "BC1T", "BC1TL"], "COP1_BRANCH"),
}

opcodes = {
	"NOP": "000000",
	**dict.fromkeys([ # SPECIAL opcode-family
		"SLL", "SRL", "SRA", "SLLV", "SRLV", "SRAV",
		"JR", "JALR", "SYSCALL", "BREAK", "SYNC", "MFHI",
		"MTHI", "MFLO", "MTLO", "DSLLV", "DSRLV", "DSRAV",
		"MULT", "MULTU", "DIV", "DIVU", "DMULT", "DMULTU",
		"DDIV", "DDIVU", "ADD", "ADDU", "SUB", "SUBU",
		"AND", "OR", "XOR", "NOR", "SLT", "SLTU",
		"DADD", "DADDU", "DSUB", "DSUBU", "TGE", "TGEU",
		"TLT", "TLTU", "TEQ", "TNE", "DSLL", "DSRL",
		"DSRA", "DSLL32", "DSRL32", "DSRA32"], "000000"),
	**dict.fromkeys([ # COP1 opcode-family
		"MUL.S", "DIV.S", "ABS.S", "ADD.S", "SUB.S", "C.EQ.S",
		"C.LE.S", "C.LT.S", "CVT.S.W", "CVT.W.S", "LWC1", "MOV.S",
		"NEG.S", "SWC1", "MULT.S", "BC1T", "BC1F", "SQRT.S",
		"MTC1", "MFC1", "BC1TL", "BC1FL"], "010001"),

		"J": "000010",
		"JAL": "000011",
		"ADDI": "001000",
		"ADDIU": "001001",
		"SLTI": "001010",
		"SLTIU": "001011",
		"ANDI": "001100",
		"ORI": "001101",
		"XORI": "001110",
		"LUI": "001111",
	"COP0": "010000",
		"COP1": "010001",
	"COP2": "010010",
		"LWC1": "110001",
		"SWC1": "111001",
		"BEQ": "000100",
		"BEQL": "010100",
		"BNE": "000101",
		"BNEL": "010101",
		"BLTZ": "000001", #2nd = 00000 - REGIMM
		"BLTZL": "000001", #2nd = 00010 - REGIMM
		"BLEZ": "000110",  #2nd = 00000
		"BLEZL": "010110",  #2nd = 00000
		"BGTZ": "000111",  #2nd = 00000
		"BGTZL": "010111",  #2nd = 00000
		"BGEZ": "000001",  #2nd = 00001 - REGIMM
		"BGEZL": "000001",  #2nd = 00011 - REGIMM
		"DADDI": "011000",
		"DADDIU": "011001",
		"LDL": "011010",
		"LDR": "011011",
		"LB": "100000",
		"LH": "100001",
		"LWL": "100010",
		"LW": "100011",
		"LBU": "100100",
		"LHU": "100101",
		"LWR": "100110",
		"LWU": "100111",
		"SB": "101000",
		"SH": "101001",
		"SWL": "101010",
		"SW": "101011",
		"SBU": "101100",
		"SHU": "101101",
		"SWR": "101110",
	"P": "101111",
	"LL": "110000",
	"LLD": "110100",
	"LD": "110111",
	"SC": "111000",
	"SLD": "111100",
	"SD": "111111",
}

regs = {
	**dict.fromkeys(["R0", "00", "0", "F0", "ZERO"], "00000"),
	**dict.fromkeys(["AT", "01", "1", "F1"], "00001"),
	**dict.fromkeys(["V0", "02", "2", "F2"], "00010"),
	**dict.fromkeys(["V1", "03", "3", "F3"], "00011"),
	**dict.fromkeys(["A0", "04", "4", "F4"], "00100"),
	**dict.fromkeys(["A1", "05", "5", "F5"], "00101"),
	**dict.fromkeys(["A2", "06", "6", "F6"], "00110"),
	**dict.fromkeys(["A3", "07", "7", "F7"], "00111"),
	**dict.fromkeys(["T0", "08", "8", "F8"], "01000"),
	**dict.fromkeys(["T1", "09", "9", "F9"], "01001"),
	**dict.fromkeys(["T2", "10", "F10"], "01010"),
	**dict.fromkeys(["T3", "11", "F11"], "01011"),
	**dict.fromkeys(["T4", "12", "F12"], "01100"),
	**dict.fromkeys(["T5", "13", "F13"], "01101"),
	**dict.fromkeys(["T6", "14", "F14"], "01110"),
	**dict.fromkeys(["T7", "15", "F15"], "01111"),
	**dict.fromkeys(["S0", "16", "F16"], "10000"),
	**dict.fromkeys(["S1", "17", "F17"], "10001"),
	**dict.fromkeys(["S2", "18", "F18"], "10010"),
	**dict.fromkeys(["S3", "19", "F19"], "10011"),
	**dict.fromkeys(["S4", "20", "F20"], "10100"),
	**dict.fromkeys(["S5", "21", "F21"], "10101"),
	**dict.fromkeys(["S6", "22", "F22"], "10110"),
	**dict.fromkeys(["S7", "23", "F23"], "10111"),
	**dict.fromkeys(["T8", "24", "F24"], "11000"),
	**dict.fromkeys(["T9", "25", "F25"], "11001"),
	**dict.fromkeys(["K0", "26", "F26"], "11010"),
	**dict.fromkeys(["K1", "27", "F27"], "11011"),
	**dict.fromkeys(["GP", "28", "F28"], "11100"),
	**dict.fromkeys(["SP", "29", "F29", "STACK"], "11101"),
	**dict.fromkeys(["FP", "30", "F30"], "11110"),
	**dict.fromkeys(["RA", "31", "F31", "R"], "11111"),
}



#===========================================================================
asm_bytesize = 0
asm_start = 0
error_count = 0

filename = "test.asm"
asm_file = open(filename, 'r')
log_file = open(filename[:-4]+".log.txt", "w")
lines = asm_file.readlines()

def imm_to_string(string, charsize, type):
	string = string.upper()
	if string[0] == '-':
		negative = True
		abs = string[1:]
	else:
		negative = False
		abs = string
	try:
		if len(abs) > 2:
			if abs[1] == 'X':
				result = int(abs, 16)
			elif abs[1] == 'O':
				result = int(abs[2:], 8)
			elif abs[1] == 'B':
				result = int(abs[2:], 2)
			else:
				result = int(abs, 10)
		else:
			result = int(abs, 10)
	except:
		log_file.write("[ERROR] Can't parse Immediate: " + string + "\n")
		return "ERROR"
	# negativity correction (overflowing)
	if negative == True:
		if type == "hex":
			result = 16**charsize - result
		elif type == "bin":
			result = 2**charsize - result
	# conversion to string
	if type == "hex":
		result = hex(result)[2:]
	elif type == "bin":
		result = bin(result)[2:]
	# length corrections
	if len(result) > charsize:
		log_file.write("[ERROR] " + string + " is too big to fit "+str(charsize)+" Char(s) in "+type+"\n")
		return "ERROR"
	while len(result) < charsize:
		result = "0" + result
	# returnal
	if type == "hex":
		return "0x" + result.upper()
	elif type == "bin":
		return "0b" + result.upper()

def char_is_digit(char):
	if char == '0':
		return True
	if char == '1':
		return True
	if char == '2':
		return True
	if char == '3':
		return True
	if char == '4':
		return True
	if char == '5':
		return True
	if char == '6':
		return True
	if char == '7':
		return True
	if char == '8':
		return True
	if char == '9':
		return True
	return False

# file = target file
# content = 
# location = (int) location where to write to
# line = (int) line of the asm file that's read from
def write_line(file, content, location=None, line=None):
	if location == None and line == None:
		file.write(content)
	elif location == None:
		file.write("L %05d: "%(line+1) + content)
	elif line == None:
		file.write("L -----: " + imm_to_string(str(location), 8, "hex") + " : " + content)
	else:
		file.write("L %05d: "%(line+1) + imm_to_string(str(location), 8, "hex") + " : " + content)
	file.write("\n")
	return
	

# Round 1 - finding Labels and Constants
#############################################################################
#############################################################################
#############################################################################
write_line(log_file, "=======================================")
write_line(log_file, "Listing Constants (C-) and Labels (L-):")
write_line(log_file, "=======================================")
for i in range(0, len(lines)):
	temp_line = lines[i].split(' ')
	args = []
	for k in range(0, len(temp_line)):
		# discard dead segments
		if temp_line[k] == "":
			continue
		# discard L ends
		if temp_line[k][0] == '\n':
			continue
		# discard everything after comments
		if temp_line[k][0] == '/' or temp_line[k][0] == '#':
			break
		# filter some junk out
		temp_line[k] = temp_line[k].replace("\n","")
		temp_line[k] = temp_line[k].replace(",","")
		temp_line[k] = temp_line[k].replace("$","")
		# move it over
		args.append(temp_line[k].upper())

	if len(args) > 0:
		full_compilation = ""
		linetype = "UNDEFINED"
		# check if arg0 is a valid identifier
		if identifiers.get(args[0], "unknown") != "unknown":
			linetype = "DATA"
			# filter .org and .start cases
			if args[0] == ".ORG" or args[0] == ".START":
				linetype = "IDENTIFIER"
				asm_start = int(args[1], 16)
				asm_bytesize = 0
			else:
				bytesize = identifiers.get(args[0])
				# silent auto-alignment for misaligned data
				while asm_bytesize % bytesize != 0:
					asm_bytesize += 1
				asm_bytesize += bytesize
		# check if arg0 is a valid Assembler-Constant
		elif len(args[0]) > 3 and args[0][0] == '[':
			linetype = "CONSTANT"
			constant_name = ""
			m = 1
			while args[0][m] != ']':
				constant_name += args[0][m]
				m += 1
			if constant_name in constants_lo:
				write_line(log_file, "[ERROR] Constant already exists: " + constant_name)
			else:
				value = imm_to_string(args[1], 8, "hex")
				if value == "ERROR":
					value = imm_to_string("0", 8, "hex")
					error_count += 1
				else:
					constants_hi[constant_name] = "0x" + value[2:6]
					constants_lo[constant_name] = "0x" + value[6:10]
					write_line(log_file, "C-"+constant_name+" = "+constants_hi[constant_name]+constants_lo[constant_name][2:], line=i)
		# check if arg0 is a Label
		# I think there are some exceptions to this (for the future)
		elif args[0] != "NOP" and len(args) == 1:
			linetype = "LABEL"
			if args[0][len(args[0])-1] != ':':
				write_line(log_file, "[AUTOFIX] Missing ':' after Label ("+args[0]+"); ':' is added")
			label = args[0].replace(":","")
			if label in labels:
				write_line(log_file, "[ERROR] Label already exists: " + label)
			else:
				labels[label] = imm_to_string(str(asm_start+asm_bytesize), 8, "hex")
				write_line(log_file, "L-"+label+" = "+labels[label], line=i)

		# at this point, the next line HAS to be an opcode line, so we can add this
		# NOTE THOUGH!!!! When implementing pseudos, this needs checking !!!
		else:
			# silent auto-alignment for misaligned opcodes
			while asm_bytesize % 4 != 0:
				asm_bytesize += 1
			asm_bytesize += 4

# Round 2 - assembling
#############################################################################
#############################################################################
#############################################################################

def assembleLine(line_string, line_num):
	global asm_bytesize
	global asm_start
	global error_count
	temp_line = line_string.split(' ')
	args = []
	for k in range(0, len(temp_line)):
		# discard dead segments
		if temp_line[k] == "":
			continue
		# discard L ends
		if temp_line[k][0] == '\n':
			continue
		# discard everything after comments
		if temp_line[k][0] == '/' or temp_line[k][0] == '#':
			break
		# filter some junk out
		temp_line[k] = temp_line[k].replace("\n","")
		temp_line[k] = temp_line[k].replace(",","")
		temp_line[k] = temp_line[k].replace("$","")
		# move it over
		args.append(temp_line[k].upper())
	# start assembling the args
	if len(args) > 0:
		full_compilation = ""
		linetype = "UNDEFINED"
		# check if arg0 is a valid identifier
		if identifiers.get(args[0], "unknown") != "unknown":
			linetype = "DATA"
			# filter .org and .start cases
			if args[0] == ".ORG" or args[0] == ".START":
				linetype = "IDENTIFIER"
				asm_start = int(args[1], 16)
				asm_bytesize = 0
			else:
				bytesize = identifiers.get(args[0])
				if asm_bytesize % bytesize != 0:
					write_line(log_file, "[AUTOFIX] Misaligned Data; FillerBytes neccessary: "+str(bytesize-(asm_bytesize%bytesize)))
					while asm_bytesize % bytesize != 0:
						write_line(log_file, "0x00", location=asm_start+asm_bytesize)
						asm_bytesize += 1
				value = imm_to_string(args[1], bytesize*2, "hex")
				if value == "ERROR":
					value = imm_to_string("0", bytesize*2, "hex")
					error_count += 1
				else:
					write_line(log_file, value, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += bytesize
		# check if arg0 is a valid opcode
		elif opcodes.get(args[0], "unknown") != "unknown":
			linetype = "INSTRUCTION"
			if asm_bytesize % 4 != 0:
				write_line(log_file, "[AUTOFIX] Misaligned OpCode; FillerBytes neccessary: "+str(4-(asm_bytesize%4)))
				while asm_bytesize % 4 != 0:
					write_line(log_file, "0x00", location=asm_start+asm_bytesize)
					asm_bytesize += 1
			opcode = opcodes.get(args[0])
			# NOP format
			# 0000 0000 0000 0000 0000 0000 0000 0000
			if argformats.get(args[0]) == "NOP":
				if len(args) != 1:
					write_line(log_file, "[ERROR] NOP-Format Instruction has format \"OPCODE\"")
					error_count += 1
				else:
					full_compilation = imm_to_string("0B" + opcode, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# JUMP format
			# oooo ooaa aaaa aaaa aaaa aaaa aaaa aaaa
			elif argformats.get(args[0]) == "JUMP":
				if len(args) != 2:
					write_line(log_file, "[ERROR] JUMP-Format Instruction has format \"OPCODE IMM\"")
					error_count += 1
				else:
					if args[1][0] == '@':
						address = constants_hi.get(args[1][1:], "Unknown Constant")
						if address == "Unknown Constant":
							write_line(log_file, "[ERROR] Unknown Constant: "+args[1][1:]+"\"")
							error_count += 1
						else:
							address = address + constants_lo.get(args[1][1:])[2:]
							address = int(address, 16)
					else:	
						address = int(imm_to_string(args[1], 8, "hex"), 16)
					if address % 4 != 0:
						write_line(log_file, "[ERROR] Jump Address ("+arg[1]+") is not word-aligned")
					else:
						if address < 2147483648:
							write_line(log_file, "[ERROR] Address "+address+" is not valid\"")
							error_count += 1
						else:
							address = address - 2147483648 # 0x80000000
							address = imm_to_string(str(address//4), 26, "bin")[2:]
							full_compilation = imm_to_string("0B" + opcode + address, 8, "hex")
							write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
							asm_bytesize += 4
			# SHIFT format
			# oooo oo00 000s ssss dddd diii iiff ffff
			elif argformats.get(args[0]) == "SPECIAL_SHIFT":
				if len(args) != 4:
					write_line(log_file, "[ERROR] SPECIAL_SHIFT-Format Instruction has format \"OPCODE DST SRC IMM\"")
					error_count += 1
				elif 'F' in args[1] or 'F' in args[2]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					dest = regs.get(args[1], "Unknown Reg")
					source = regs.get(args[2], "Unknown Reg")
					shift = imm_to_string(args[3], 5, "bin")[2:]
					functcode = functcodes.get(args[0], "Unknown FunctCode")
					# swap args 1 and 2, because dest-reg is last
					full_compilation = opcode + source + dest + shift + functcode
					full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# LOADSTORE format
			# oooo ooss sssd dddd iiii iiii iiii iiii
			elif argformats.get(args[0]) == "LOADSTORE":
				if len(args) != 3:
					write_line(log_file, "[ERROR] LOADSTORE-Format Instruction has format \"OPCODE DST IMM(SRC)\"")
					error_count += 1
				elif 'F' in args[1]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					dest = regs.get(args[1], "Unknown Reg")
					split = args[2].split("(")
					if len(split) != 2:
						write_line(log_file, "[ERROR] LOADSTORE-Format Instruction has format \"OPCODE DST IMM(SRC)\"")
						error_count += 1
					else:
						if split[1][len(split[1])-1] != ')':
							write_line(log_file, "[ERROR] LOADSTORE-Format Instruction has format \"OPCODE DST IMM(SRC)\"")
							error_count += 1
						else:
							split[1] = split[1][:-1]
							if split[0][0] == '@':
								constant = constants_lo.get(split[0][1:], "Unknown Constant")
								if constant == "Unknown Constant":
									write_line(log_file, "[ERROR] Unknown Constant: "+split[0][1:]+"\"")
									error_count += 1
								else:
									immediate = imm_to_string(constant, 16, "bin")[2:]
							else:
								immediate = imm_to_string(split[0], 16, "bin")[2:]
							source = regs.get(split[1], "Unknown Reg")
							full_compilation = opcode + source + dest + immediate
							full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
							write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
							asm_bytesize += 4
			# IMMEDIATE format
			# oooo ooss sssd dddd iiii iiii iiii iiii
			elif argformats.get(args[0]) == "IMMEDIATE":
				if len(args) != 4:
					write_line(log_file, "[ERROR] IMMEDIATE-Format Instruction has format \"OPCODE DST SRC1 SRC2\"")
					error_count += 1
				elif 'F' in args[1] or 'F' in args[2]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					dest = regs.get(args[1], "Unknown Reg")
					source = regs.get(args[2], "Unknown Reg")
					if args[3][0] == '@':
						immediate = constants_lo.get(args[3][1:], "Unknown Constant")
						if immediate == "Unknown Constant":
							write_line(log_file, "[ERROR] Unknown Constant: "+args[3][1:]+"\"")
							error_count += 1
						else:
							immediate = imm_to_string(immediate, 16, "bin")
					else:
						immediate = imm_to_string(args[3], 16, "bin")
					full_compilation = opcode + source + dest + immediate[2:]
					full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# REGREG_BRANCH format
			# oooo ooss sssS SSSS iiii iiii iiii iiii
			elif argformats.get(args[0]) == "REGREG_BRANCH":
				if len(args) != 4:
					write_line(log_file, "[ERROR] REGREG_BRANCH-Format Instruction has format \"OPCODE SRC1 SRC2 LABEL\"")
					error_count += 1
				elif 'F' in args[1] or 'F' in args[2]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					source_A = regs.get(args[1], "Unknown Reg")
					source_B = regs.get(args[2], "Unknown Reg")
					label = args[3]
					if label[0] == '@':
						label = label[1:]
					label_value = labels.get(label, "Unknown Label")
					if label_value == "Unknown Label":
						write_line(log_file, "[ERROR] Branch Label is unknown: "+label)
						error_count += 1
					else:
						offset = int(label_value, 16) - (asm_start + asm_bytesize + 4)
						if offset % 4 != 0:
							write_line(log_file, "[ERROR] Branch Offset ("+imm_to_string(str(offset), 8, "hex")+") is not word-aligned")
							error_count += 1
						else:
							offset = imm_to_string(str(offset//4), 16, "bin")[2:]
							full_compilation = opcode + source_A + source_B + offset
							full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
							write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
							asm_bytesize += 4
			# SPECIAL_TSS format
			# 0000 00ss sssS SSSS dddd d000 00ff ffff
			elif argformats.get(args[0]) == "SPECIAL_TSS":
				if len(args) != 4:
					write_line(log_file, "[ERROR] SPECIAL_TSS-Format Instruction has format \"OPCODE DEST SRC1 SRC2\"")
					error_count += 1
				elif 'F' in args[1] or 'F' in args[2] or 'F' in args[3]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					dest = regs.get(args[1], "Unknown Reg")
					source_A = regs.get(args[2], "Unknown Reg")
					source_B = regs.get(args[3], "Unknown Reg")
					functcode = functcodes.get(args[0], "Unknown FunctCode")
					full_compilation = opcode + source_A + source_B + dest + "00000" + functcode
					full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# SPECIAL_TARGET format
			# 0000 0000 0000 0000 dddd d000 00ff ffff
			elif argformats.get(args[0]) == "SPECIAL_TARGET":
				if len(args) != 2:
					write_line(log_file, "[ERROR] SPECIAL_TARGET-Format Instruction has format \"OPCODE DEST\"")
					error_count += 1
				elif 'F' in args[1]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					dest = regs.get(args[1], "Unknown Reg")
					functcode = functcodes.get(args[0], "Unknown FunctCode")
					full_compilation = opcode + "0000000000" + dest + "00000" + functcode
					full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# JR format
			# 0000 00ss sss0 0000 0000 0000 0000 1000
			elif args[0] == "JR":
				if len(args) != 2:
					write_line(log_file, "[ERROR] JR Instruction has format \"OPCODE DEST\"")
					error_count += 1
				else:
					dest = regs.get(args[1], "Unknown Reg")
					full_compilation = "00000" + dest + "000000000000000001000"
					full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# LUI format
			# 0011 1100 000d dddd iiii iiii iiii iiii
			elif args[0] == "LUI":
				if len(args) != 3:
					write_line(log_file, "[ERROR] LUI Instruction has format \"OPCODE DEST IMM\"")
					error_count += 1
				elif 'F' in args[1]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					dest = regs.get(args[1], "Unknown Reg")
					if args[2][0] == '@':
						immediate = constants_hi.get(args[2][1:], "Unknown Constant")
						if immediate == "Unknown Constant":
							write_line(log_file, "[ERROR] Unknown Constant: "+args[2][1:]+"\"")
							error_count += 1
						else:
							immediate = imm_to_string(immediate, 16, "bin")[2:]
					else:
						immediate = imm_to_string(args[2], 16, "bin")[2:]
					full_compilation = "00111100000" + dest + immediate
					full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# ZERO_BRANCH format
			# oooo ooss sssz zzzz iiii iiii iiii iiii
			elif argformats.get(args[0]) == "ZERO_BRANCH":
				if len(args) != 3:
					write_line(log_file, "[ERROR] ZERO_BRANCH Instruction has format \"OPCODE SRC LABEL\"")
					error_count += 1
				elif 'F' in args[1]:
					write_line(log_file, "[ERROR] Wrong FPU/GP Regs specified")
					error_count += 1
				else:
					source = regs.get(args[1], "Unknown Reg")
					if opcode == "000001": # REGIMM
						zeroslot = functcodes.get(args[0], "Unknown FunctCode")
						if zeroslot == "Unknown FunctCode":
							write_line(log_file, "[ERROR] Unknown FunctCode: "+args[1])
							error_count += 1
					else:
						zeroslot = "00000"
					label = args[2]
					if label[0] == '@':
						label = label[1:]
					label_value = labels.get(label, "Unknown Label")
					if label_value == "Unknown Label":
						write_line(log_file, "[ERROR] Branch Label is unknown: "+label)
						error_count += 1
					else:
						offset = int(label_value, 16) - (asm_start + asm_bytesize + 4)
						if offset % 4 != 0:
							write_line(log_file, "[ERROR] Branch Offset ("+imm_to_string(str(offset))+") is not word-aligned")
							error_count += 1
						else:
							offset = imm_to_string(str(offset//4), 16, "bin")[2:]
							full_compilation = opcode + source + zeroslot + offset
							full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
							write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
							asm_bytesize += 4
			# MFC and MTC
			# oooo oomm mmmd dddd ssss s000 0000 0000
			elif args[0] == "MFC1" or args[0] == "MTC1":
				if len(args) != 3:
					write_line(log_file, "[ERROR] MFC/MTC Instruction has format \"OPCODE FReg GPReg\"")
					error_count += 1
				else:	
					if args[0] == "MFC1":
						movecode = "00000"
					else:
						movecode = "00100"
					opcode = opcodes.get("COP1")
					dest = regs.get(args[1], "Unknown Reg")
					source = regs.get(args[2], "Unknown Reg")
					full_compilation = opcode + movecode + dest + source + "00000000000"
					full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
					write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
					asm_bytesize += 4
			# COP1 BRANCH
			# oooo oo01 000c ccnt iiii iiii iiii iiii
			# n = nullify branch-delay instruction
			# t = true/false version
			elif argformats.get(args[0]) == "COP1_BRANCH":
				if len(args) != 2:
					write_line(log_file, "[ERROR] COP1-BRANCH Instruction has format \"OPCODE LABEL\"")
					error_count += 1
				else:
					if len(args[0]) == 5:
						nullify = "1"
					else:
						nullify = "0"
					if args[0][3] == 'F':
						truefalse = "0"
					else:
						truefalse = "1"
					opcode = opcodes.get("COP1")
					label = args[1]
					if label[0] == '@':
						label = label[1:]
					label_value = labels.get(label, "Unknown Label")
					if label_value == "Unknown Label":
						write_line(log_file, "[ERROR] Branch Label is unknown: "+label)
						error_count += 1
					else:
						offset = int(label_value, 16) - (asm_start + asm_bytesize + 4)
						if offset % 4 != 0:
							write_line(log_file, "[ERROR] Branch Offset ("+imm_to_string(str(offset), 8, "hex")+") is not word-aligned")
							error_count += 1
						else:
							offset = imm_to_string(str(offset//4), 16, "bin")[2:]
							full_compilation = opcode + "01000000" + nullify + truefalse + offset
							full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
							write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
							asm_bytesize += 4
			# FPU instructions
			elif '.' in args[0]:
				opcode = opcodes.get("COP1")
				copargs = args[0].split(".")
				# collapse copargs for CVT instructs
				if len(copargs) == 3 and copargs[0] == "CVT":
					copargs[0] += "." + copargs[1]
					copargs[1] = copargs[2]
					copargs = copargs[:-1]
				# COP1_ST Format
				# oooo ooFF FFF0 0000 ssss sddd ddcc cccc
				if argformats.get(copargs[0] + ".") == "COP1_ST":
					if len(copargs) != 2 or len(args) != 3:
						write_line(log_file, "[ERROR] COP1-ST-Format Instruction has format \"OPCODE.FMT DEST SRC\"")
						error_count += 1
					else:
						copcode = copcodes.get(copargs[0], "Unknown FunctCode")
						fmt = fmts.get(copargs[1], "Unknown FMT")
						dest = regs.get(args[1], "Unknown Reg")
						source = regs.get(args[2], "Unknown Reg")
						full_compilation = opcode + fmt + "00000" + source + dest + copcode
						full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
						write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
						asm_bytesize += 4
				# COP1_SST Format
				# oooo ooFF FFFS SSSS ssss sddd ddcc cccc
				elif argformats.get(copargs[0] + ".") == "COP1_SST":
					if len(copargs) != 2 or len(args) != 4:
						write_line(log_file, "[ERROR] COP1-SST-Format Instruction has format \"OPCODE.FMT DEST SRC1 SRC2\"")
						error_count += 1
					else:
						copcode = copcodes.get(copargs[0], "Unknown FunctCode")
						fmt = fmts.get(copargs[1], "Unknown FMT")
						dest = regs.get(args[1], "Unknown Reg")
						source_A = regs.get(args[2], "Unknown Reg")
						source_B = regs.get(args[3], "Unknown Reg")
						full_compilation = opcode + fmt + source_B + source_A + dest + copcode
						full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
						write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
						asm_bytesize += 4
				# COP1_CONDITION
				# oooo ooFF FFFS SSSS ssss s000 0011 cccc
				elif copargs[0] == "C":
					if len(copargs) != 3 or len(args) != 3:
						write_line(log_file, "[ERROR] COP1-CONDITION-Format Instruction has format \"C.CONDITION.FMT SRC1 SRC2\"")
						error_count += 1
					else:
						conditioncode = conditioncodes.get(copargs[1], "Unknown ConditionCode")
						fmt = fmts.get(copargs[2], "Unknown FMT")
						source_A = regs.get(args[1], "Unknown Reg")
						source_B = regs.get(args[2], "Unknown Reg")
						full_compilation = opcode + fmt + source_B + source_A + "0000011" + conditioncode
						full_compilation = imm_to_string("0B" + full_compilation, 8, "hex")
						write_line(log_file, full_compilation, location=asm_start+asm_bytesize, line=line_num)
						asm_bytesize += 4
			# Unsupported format
			else:
				write_line(log_file, "[ERROR] Unsupported Instruction: " + args[0] + "\n")
				asm_bytesize += 4

##############################################################
##############################################################
##############################################################
write_line(log_file, "=======================================")
write_line(log_file, " Line  | ROM Addr.  | Bytecode")
write_line(log_file, "=======================================")
for i in range(0, len(lines)):
	assembleLine(lines[i], i)

print("==============================================")
print("Assembler Constants found:")
for key in constants_hi:
	print("   " + key + ": " + constants_hi[key] + constants_lo[key][2:])
asm_file.close()
print("Labels found:")
for key in labels:
	print("   " + key + ": " + labels[key])

print("==============================================")

asm_file.close()
log_file.close()





