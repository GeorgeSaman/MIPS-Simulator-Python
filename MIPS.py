######### GEORGE SAMAN
######### 5 Stage MIPS Simulator
######### NOTE: It deals with memory stalls, and data hazards
######### 12/08/2017

regs = [[0,0,"$zero","constant zero"],
        [1,0,"$at","assembler temporary"],
        [2,0,"$v0","value for function results"],
        [3,0,"$v1","value for function results"],
        [4,0,"$a0","arguments"],
        [5,0,"$a1","arguments"],
        [6,0,"$a2","arguments"],
        [7,0,"$a3","arguments"],
        [8,0,"$t0","temporaries"],
        [9,0,"$t1","temporaries"],
        [10,0,"$t2","temporaries"],
        [11,0,"$t3","temporaries"],
        [12,0,"$t4","temporaries"],
        [13,0,"$t5","temporaries"],
        [14,0,"$t6","temporaries"],
        [15,0,"$t7","temporaries"],
        [16,0,"$s0","saved temporaries"],
        [17,0,"$s1","saved temporaries"],
        [18,0,"$s2","saved temporaries"],
        [19,0,"$s3","saved temporaries"],
        [20,0,"$s4","saved temporaries"],
        [21,0,"$s5","saved temporaries"],
        [22,0,"$s6","saved temporaries"],
        [23,0,"$s7","saved temporaries"],
        [24,0,"$t8","temporaries"],
        [25,0,"$t9","temporaries"],
        [26,0,"$k0","reserved for OS kernel"],
        [27,0,"$k1","reserved for OS kernel"],
        [28,0,"$gp","global pointer"],
        [29,0,"$sp","stack pointer"],
        [30,0,"$fp","frame pointer"],
        [31,0,"$ra","return address"] ]
LO_REG = 0

IF_ID = [0, 0] # Current instruction | Next instruction
ID_EX = [[0 , 0 , 0],[0 , 0 , 0]]  #current READ REG1 | READ REG2 | SIGN EXTEND...NEXT
EX_MEM= [[0, 0],[0, 0]] # ALU result | alu_sr2 , CUrrent and next
MEM_WB =[[0, 0],[0, 0]] # MEMORY_READ_DATA | ALU_result

RegDst =[0, 0, 0, 0] #WB | MEM | EX | DECODE stage
MemtoReg = [0, 0, 0, 0] #WB | MEM | EX | DECODE stage
RegWrite = [0, 0, 0, 0] #WB | MEM | EX | DECODE stage
MemRead =  [0, 0, 0] # MEM | EX | DECODE stage
MemWrite = [0, 0, 0, 0] # WB| MEM | EX | DECODE stage
ALUOp = [0, 0]   # EX | DECODE stage
ALUSrc = [0, 0]  # EX | DECODE stage
stallDetected = 0

my_rs = [0, 0, 0, 0] #WB | MEM | EX | DECODE stage
my_rt = [0, 0, 0, 0] #WB | MEM | EX | DECODE stage
my_rd = [0, 0, 0, 0] #WB | MEM | EX | DECODE stage
my_funct=[0, 0] # EX | DECODE stage
my_shamt=[0, 0] # EX | DECODE stage
my_op   =[0,0] # EX | DECODE stage
Branch  =[0,0] # EX | DECODE stage
branch_target =0 # EX 

inst_assembly = [0, 0, 0, 0, 0]

def display_regs():
    x = [x*2 for x in range(16)]
    for i in x:
        print(regs[i][2] + "=", "%#010x"% (regs[i][1])+'    '+regs[i+1][2] + "=", "%#010x"% (regs[i+1][1]))
    print("LO_REG = " + str(LO_REG))
    return
def display_mem():
    print("data Memory")
    for i in range(len(data_mem)):
        print("location %d"% i + " = " + str(data_mem[i]))    
    

# ---- CONTROL FIELDS ----
# From Page 266 - Figure 4.18 
# RegDst -- Destination Register selection based upon R or I Format
# ALUSrc -- ALU operand from either register file or sign extended immediate
# MemtoReg -- Loads - selects register write data from memory
# RegWrite -- Write Enable for Register File
# MemRead -- Read Enable for Data Memory
# MemWrite -- Write Enable for Data Memory
# Branch -- Branch instruction used to qualify next PC address
# ALUOp -- ALU operation predecode
#| RegDst | ALUSrc | MemtoReg | RegWrite | MemRead | MemWrite | Branch | ALUOp |
control = { 0b000000 : [1,0,0,1,0,0,0,2],     #R Format
            0b100011 : [0,1,1,1,1,0,0,0],     #lw
            0b101011 : [0,1,0,0,0,1,0,0],     #sw
            0b000100 : [0,0,0,0,0,0,1,1],     #beq
            0b001101 : [0,1,0,1,0,0,0,3],     #ori
            0b001000 : [0,1,0,1,0,0,0,3],     #addi
            0b000001 : [0,0,0,0,0,0,1,1]}     #bgez
            



ALU = { 0b0000 : lambda src1, src2 : ["and", src1 & src2, "bitwise and"],
        0b0001 : lambda src1, src2 : ["or",  src1 | src2, "bitwise or"],
        0b0010 : lambda src1, src2 : ["add", src1 + src2, "add signed"],
        0b0011 : lambda src1, src2 : ["sll", (src1 * (2**src2)), "shift logical left"],
        0b0110 : lambda src1, src2 : ["sub", src1 - src2, "sub signed"],
        0b0111 : lambda src1, src2 : ["slt", 0, "set on less than"],
        0b1100 : lambda src1, src2 : ["nor", ~(src1 | src2), "bitwise nor"],
        0b1101 : lambda src1, src2 : ["multu", src1*src2, "multiply"],
        0b1110 : lambda src1, src2 : ["mflo",LO_REG,"move from LO_REG"]}

decode_funct = { 0b000000 : ["sll", 0b0011],
                 0b100000 : ["add", 0b0010],
                 0b100010 : ["sub", 0b0110],
                 0b100100 : ["and", 0b0000],
                 0b100101 : ["or",  0b0001],
                 0b101010 : ["slt", 0b0111],
                 0b011001 : ["multu", 0b1101],
                 0b010010 : ["mflo", 0b1110]}


decode_Ifunct ={ 0b001101 : ["or", 0b0001],
                 0b001000 : ["addi", 0b0010]}

BranchAddress = { 0b000100 : lambda Zero, greatherThanZero: (branch_target if (Zero) else PC_plus_4), # beq
                  0b000001 : lambda Zero, greatherThanZero: (branch_target if ((greaterThanZero==1)or(Zero==1)) else PC_plus_4) } # bgez

def ALU_control(ALUOp, funct,opcode):
    if (ALUOp == 0): #lw, sw => add
        return(0b0010)
    
    if (ALUOp == 1):  #beq/bgez  => sub
        return(0b0110)
    if (ALUOp == 2): # Rtype
        return (decode_funct[funct][1])
    if ALUOp ==3:
        return (decode_Ifunct[opcode][1])

# Initialize Memory

inst_mem = []
data_mem = [1,2,3,4,5,6,0,0] # matA= [1,2],[3,4] matX=[4,5], matB=[0,0]

def read_instr():
    infile = open("instructions.txt","r")
    lines = infile.readlines()
    codelines = [x for x in lines if x[0] != "#"]
    for line in codelines:
        words = line.split()
        mem = (int(words[0],16),int(words[1],16),words[2])
        inst_mem.append(mem)
    infile.close()
    return

read_instr()   #Read Instruction Memory


#***** Start of Machine *****
PC = 0
clock = 0 # a variable holding clock counts
Zero  = 0
greaterThanZero = 0

for clockcount in range(170):
    

################################## FETCH STAGE ######################################
    #fetch instruction
    addr = inst_mem[PC>>2][0]
    inst_assembly[4] = inst_mem[PC>>2][2]
    instruction = inst_mem[PC>>2][1]
   
    #latch result of this stage into its pipeline reg
    IF_ID[1] = instruction
    
    clock = clock +1
    #increment PC if there is no STALL
    if stallDetected == 0:
        PC_plus_4 = PC + 4
        PC = PC_plus_4
        releaseStall = 0
    else:
        releaseStall = 1
        
    print('----------------------------------------------------')
    print('/F\ Fetched = ' + str(inst_assembly[4])+    '  ------Clock cycle = '+ str(clock))
    print('IF/ID ----- To decode = '+ "%#010x"%(IF_ID[0])+'  fetched = '+ "%#010x" %(IF_ID[1]))
   
        
################################### DECODE STAGE ######################################
    if (clock >= 2):
        ################### WRITE FIRST THEN READ#########################
        if (clock >= 5):
            #register write back
            register_write_data = MEM_WB[0][0] if MemtoReg[0] else MEM_WB[0][1]
            write_register = my_rd[0] if RegDst[0] else my_rt[0]

            if (RegWrite[0] and (write_register!= 0)): # do not write to reg zero. FOR MULTU
                regs[write_register][1] = register_write_data
            
            
        #decode instruction
        my_op[1] = IF_ID[0] >> 26
        my_rs[3] = (IF_ID[0] >> 21) & 0x1F
        my_rt[3] = (IF_ID[0] >> 16) & 0x1F # store in next3 since it is used in WB
        my_rd[3] = (IF_ID[0] >> 11) & 0x1F
        my_shamt[1] = (IF_ID[0] >> 6) & 0x1F
        my_funct[1] = IF_ID[0] & 0x3F
        my_imm = IF_ID[0] & 0xFFFF
       
        #control signals
        control_word = control[my_op[1]]
        ALUSrc[1] = control_word[1] 
        RegDst[3] = control_word[0] 
        MemtoReg[3] = control_word[2] 
        RegWrite[3] = control_word[3] 
        MemRead[2] = control_word[4]  
        MemWrite[3] = control_word[5] 
        Branch[1] = control_word[6]
        ALUOp[1] = control_word[7] 

        #register file sources
        read_register1 = my_rs[3]
        read_register2 = my_rt[3]
  
        read_data1 = regs[read_register1][1] 
        read_data2 = regs[read_register2][1]
        
     
                                
        #sign extension of immediate data
        sign_bit = (my_imm >> 15) & 0x1
        sign_ext = (my_imm-(0x10000)) if (sign_bit == 1) else my_imm
        
        
        #Latch results of that stage into its pipeline reg
        ID_EX[1] = [read_data1, read_data2, sign_ext]
        print('/D\ Decoded = ' + str(inst_assembly[3]))
        print("ID/EX -----  for current execute= [%d, %d, %d]" %((ID_EX[0][0]),(ID_EX[0][1]),(ID_EX[0][2]))," result of current decode =[%d, %d, %d]" %((ID_EX[1][0]),(ID_EX[1][1]),(ID_EX[1][2])))
        print("RD = [%d, %d, %d, %d]" %((my_rd[0]) ,(my_rd[1]), (my_rd[2]) ,(my_rd[3])))
        print("RS =  [%d, %d, %d, %d]" %((my_rs[0]) ,(my_rs[1]), (my_rs[2]) ,(my_rs[3])))
        print("RT =  [%d, %d, %d, %d]" %((my_rt[0]) ,(my_rt[1]), (my_rt[2]) ,(my_rt[3])))
        
        print("MemRead=      ", MemRead)
        print("MemWrite = ", MemWrite)
        print("MemToReg = " , MemtoReg)
        print("RegWrite = ", RegWrite)
        print("regDst =   ", RegDst)

     
#################################### EXECUTE STAGE ######################################
    if (clock >= 3):
        # alu sources when there is no hazard or stall
        alu_src1 = ID_EX[0][0]     # read_data1                                                             
        alu_src2 = ID_EX[0][2] if ALUSrc[0] else ID_EX[0][1]  # if ALUsrc_current = 1 then sign Extend is alu_src2 else read_data2
        readData2= ID_EX[0][1]
        ############## H     A     Z     A     R     D DETECTION BETWEEN WB AND EX STAGE ############################
        # hazard is present if RD of WB stage (RD[0]) == Ex stage's Rs[2] or RT[2]                                  #
        # hazard is present if RD of MEM stage (RD[1]) == Ex stage's RS[2] or RT[2]                                 #
        hazardDetected_rs =0                                                                                        #
        hazardDetected_rt =0                                                                                        #
        if (clock >5):                                                                                              #
            if ((RegWrite[0]) or (RegWrite[1])): # check if there is a write back to register
                if ((MemWrite[0]== 0) and (MemWrite[1]== 0) and (MemWrite[2]== 0)):                                 #
                    # check if intruction in WB or MEM writes back to register and WB,MEM,EX is not a sw inst       #
                                                                                                                    #
                    WB_hazard_RD_check = my_rd[0] if RegDst[0] else my_rt[0]                                        #
                    if (WB_hazard_RD_check == my_rs[2]): # with RS
                        if((ALUOp[0] == 3)): # if I instruction
                            print("--------------------------RS forwaded from WB stage. RS = %d" %(MEM_WB[0][1]))          #
                            alu_src1_rs = MEM_WB[0][1]  # load into alu_src1 the value of ALU result                        #
                            hazardDetected_rs = 1
                        else:
                            print("--------------------------RS forwaded from WB stage. RS = %d" %(MEM_WB[0][0]))       #
                            alu_src1_rs = MEM_WB[0][0]  # load into alu_src1 the value of the loaded data from memory   #
                            hazardDetected_rs = 1
                            
                    if ( WB_hazard_RD_check == my_rt[2]): # with RT
                        if((ALUOp[0] == 3)): # if I instruction
                            alu_src2 = alu_src2
                        else:
                            print("--------------------------RT forwaded from WB stage. RT = %d" %(MEM_WB[0][0]))       #
                            alu_src2_rt = MEM_WB[0][0]                                                                  #
                            hazardDetected_rt = 1
                            
                    MEM_hazard_RD_check = my_rd[1] if RegDst[1] else my_rt[1]                                           #
                    if (MEM_hazard_RD_check == my_rs[2]): # with RS
                            print("--------------------------RS forwaded from MEM stage. RS = %d" %(EX_MEM[0][0]))          #
                            alu_src1_rs = EX_MEM[0][0]  # load into alu_src1 the value of ALU result                        #
                            hazardDetected_rs = 1
                                              
                    if ( (MEM_hazard_RD_check == my_rt[2]) and (ALUSrc[0] == 0)): # with RT AND source is a reg not signext#
                            print("--------------------------RT forwaded from MEM stage. RT = %d" %(EX_MEM[0][0]))          #
                            alu_src2_rt = EX_MEM[0][0]                                                                      #
                            hazardDetected_rt = 1                                                                           #
                                                                                    #           
        #############################################################################################################
                
        ################## S     T     A     L     L DETECTION ######################################################
            # stall happens when execute sources needs a valid data which is not yet loaded from memory             #
                                                                                                                    #
            MEM_STALL_RD_check = my_rd[1] if RegDst[1] else my_rt[1]                                                #
                                                                                                                    #
            if releaseStall == 1: # if last clock cycle was a stall then release it, otherwise order one            #
                stallDetected = 0                                                                                   #
                alu_src1 = regs[my_rs[2]][1]# on the next cycle of the stall, re fetch registers.                   # 
                alu_src2 = regs[my_rt[2]][1]# in case a WB hazard happened with the stall                           #
                                                                                                                    #                      
            if ((MemRead[0] ==1)and ((MEM_STALL_RD_check == my_rs[2]) or (MEM_STALL_RD_check == my_rt[2]))):        #
                        print("--------------------------STALL CAUSED FROM EX DUE TO MEM LOAD")                     #
                        # turn everything into zero for the next cycle to operate as a NOP                          #
                        stallDetected = 1 # the releaseStall signal is used in the fetch stage                      #
                        EX_MEM[1] = [0, 0]  # result of current execution =0                                        #
                        PC = PC -4 # ADJUST PC TO RE FETCH LAST INSTRUCTION                                         #
                        print("/E\ Executed = NOP $zero,$zero,$zero")                                               #
                        print("EX/MEM -----  for current MEM= ",EX_MEM[0], " result of current execute = ", EX_MEM[1]) #                                                                                                  
        #############################################################################################################
                    
        if (stallDetected == 0):
            if hazardDetected_rs == 1: # fetch the forwaded data
                alu_src1 = alu_src1_rs
            if hazardDetected_rt == 1:
                alu_src2 = alu_src2_rt

            if my_op[0] == 0b000001: # if bgez then alusrc2 = zero
                alu_src2 = 0
                
            print("AluSrc = " ,ALUSrc)
            print("ALUOp = " , ALUOp)
            print("funct = " , my_funct)
            print("shamt = " , my_shamt)
            print("ALU SOURCES =", alu_src1, alu_src2)
            
            sll =((ALUOp[0]==2) and (my_funct[0]== 0b000000)) # check if there is a sll inst
            if sll: # if sll then alu_src2 = sign ext
                alu_src1 = alu_src2 
                alu_src2 = my_shamt[0] 
                print("SOURCES UPDATED___ALU SOURCES =", alu_src1, alu_src2)
            
            alu_operation = ALU_control(ALUOp[0], my_funct[0],my_op[0]) # ALUOp is the current instruction
            alu_entry = ALU[alu_operation](alu_src1,alu_src2)
            multiplication = ((ALUOp[0]==2) and (my_funct[0]==0b011001)) # check if there is a multu inst
            alu_result = 0 if multiplication else alu_entry[1]
            
            if multiplication:
                LO_REG = alu_entry[1]
            
            #Branch Target
            branch_target = PC_plus_4 + (ID_EX[0][2]*4)
        
            Zero = 1 if (alu_result == 0) else 0
            greaterThanZero = 1 if(alu_result >0) else 0 ;
            # ---- Next PC Calculation ----
            #pc_mux1
            pc_mux1 = BranchAddress[my_op[0]](Zero, greaterThanZero) if Branch[0] ==1 else PC_plus_4
            if pc_mux1 != PC_plus_4: # i.e branch Taken
                print("Branch Taken")
            else:
                print("Branch Not Taken")   
            Jump = 0
            pc_mux2 = 0 if (Jump) else pc_mux1

            PC = pc_mux2  #Next Instruction

            #Latch results of that stage into its pipeline reg
            EX_MEM[1] = [alu_result, readData2]
            print("/E\ Executed = "+ str(inst_assembly[2]))
            print("EX/MEM -----  for current MEM= ",EX_MEM[0], " result of current execute = ", EX_MEM[1])
            print("stall Detected= " + str(stallDetected))
            print("Zero = " + str(Zero) +  " greaterThanZero = " + str(greaterThanZero)+ "  Next_PC = " + str(PC) +"  Branch = "+str(Branch[0]))
            print("PC_MUX1= " +str(pc_mux1)+ " PC+4 = "+str(PC_plus_4)+ "  Branch_Target=", branch_target)
            
################################## MEM STAGE ######################################
    if (clock >= 4):
        memory_read_data = 0
        # data memory operations
        if MemWrite[1]: # current Control
            data_mem[EX_MEM[0][0]>>2] = EX_MEM[0][1]

        if MemRead[0]:
            memory_read_data = data_mem[EX_MEM[0][0]>>2] 

        #Latch results of that stage into its pipeline reg
        MEM_WB[1] = [memory_read_data, EX_MEM[0][0]]  # alu result
        print("/M\ Memory = " + str(inst_assembly[1]))
        print("MEM/WB -----  for current WB= ",MEM_WB[0], " result of current MEM load = ", MEM_WB[1])
     
        
################################## Write Back STAGE ######################################

    if (clock >= 5):
        print("/W\ WriteBack = "+str(inst_assembly[0]))
        display_regs()
        display_mem()
################################ UPDATE PIPELINE REGISTERS ##############################
    #update pipeline registers
    
          
    my_rd[0] = my_rd[1]
    my_rd[1] = my_rd[2]
    my_rt[0] = my_rt[1]
    my_rt[1] = my_rt[2]
    my_rs[0] = my_rs[1]
    my_rs[1] = my_rs[2]
    
    EX_MEM[0] = EX_MEM[1]
    MEM_WB[0] = MEM_WB[1]

    #update Memory control lists
    MemWrite[0] = MemWrite[1]
    MemWrite[1] = MemWrite[2]
    MemRead[0] = MemRead[1]
  #update WB control lists
    MemtoReg[0] = MemtoReg[1]
    MemtoReg[1] = MemtoReg[2]
    RegWrite[0] = RegWrite[1]
    RegWrite[1] = RegWrite[2]
    RegDst[0] = RegDst[1]
    RegDst[1] = RegDst[2]

    inst_assembly[0] = inst_assembly[1]
    inst_assembly[1] = inst_assembly[2]
  
    if stallDetected == 0:
        # if there is a stall, stall these controls and registers
        IF_ID[0] = IF_ID[1]
        ID_EX[0] = ID_EX[1]
        my_rd[2] = my_rd[3]
        my_rt[2] = my_rt[3]
        my_rs[2] = my_rs[3]
        
        ALUSrc[0] = ALUSrc[1] # update next instruction to current instruction for next clock cycle
        ALUOp[0]  = ALUOp[1]
        MemWrite[2] = MemWrite[3]
        MemRead[1] = MemRead[2]
        RegWrite[2] = RegWrite[3]
        RegDst[2] = RegDst[3]
        MemtoReg[2] = MemtoReg[3]
        
        my_funct[0] = my_funct[1]
        my_shamt[0] = my_shamt[1]
        my_op[0] = my_op[1]
        Branch[0] = Branch[1]
        
        
        inst_assembly[2] = inst_assembly[3]
        inst_assembly[3] = inst_assembly[4]
    else:
        # Turn all signals and registers to zero to form a NOP
        my_rs[1] = 0
        my_rt[1] = 0
        my_rd[1] = 0
        RegDst[1]  = 0
        MemtoReg[1]= 0
        RegWrite[1]= 0
        MemRead[0] = 0
        MemWrite[1]= 0
        inst_assembly[1] = "NOP $zero,$zer0,%zero"

# -- End of Main Loop

