import java.io.File;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class SAG_CPU {
	
	// Member variables
	
	private Program_Counter PC;
	private Register IR;
	private Register Zero;
	private Register[] GP_Registers;
	private Register[] Device_Registers;
	private Register Stack_Pointer;
	private Register Frame_Pointer;
	private Register Return_Address;
	private ALU ALU;
	private Memory Main_Memory;
	
	// Constructor
	public SAG_CPU(){
		this.PC = new Program_Counter();
		this.IR = new Register();
		this.GP_Registers = new Register[7];
		this.Device_Registers = new Register[4];
		this.Stack_Pointer = new Register();
		this.Frame_Pointer = new Register();
		this.Return_Address = new Register();
		this.ALU = new ALU();
		this.Main_Memory = new Memory(12);
	}
	
	// Methods
	
	public void Load_Program(File program){
		int i = 0;
		try(BufferedReader br = new BufferedReader(new FileReader(program))) {
		    for(String line; (line = br.readLine()) != null; ) {
		        int instruction = Integer.parseInt(line);
		        Main_Memory.Store(instruction, i);
		        i++;
		    }
		} catch (FileNotFoundException e) {
			System.out.println("The file does not exist.");
			e.printStackTrace();
		} catch (IOException e) {
			System.out.println("There was an error opening the file.");
			e.printStackTrace();
		}
		Fetch_Instruction();
	}
	
	public void Fetch_Instruction(){
		Main_Memory.Load(PC.Get_Count());
		int newPC = Decode();
		if(newPC != -1)
		{
			PC.Load_Address(newPC);
		}else{
			PC.Increment(1);
		}
		
		
	}
	
	public int Decode(){
		//might not work correctly if instruction has leading zeros, as they would be removed due to int to string conversions
		String temp = Integer.toString(IR.getContents());

		int digitsShort = 16 - temp.length();
		while(digitsShort > 0)
		{
			temp = "0" + temp;
			digitsShort--;
		}
		
		String opCodeString = temp.substring(0, 4);
		
		int pcReturnValue = -1;
		String src1, src2, dest;
		int src1int, src2int, destint;
		switch(opCodeString){
		case "0000":
			//do NOP stuff
			Execute(opCodeString, 0, 0, 0, 0, 0);
			break;
		case "0001":
			//do ADD stuff
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), GP_Registers[destint].getContents(), 0, 0);
			break;
		case "0010":
			//do SUB stuff
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), GP_Registers[destint].getContents(), 0, 0);
			break;
		case "0011":
			//do AND stuff
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), GP_Registers[destint].getContents(), 0, 0);
			break;
		case "0100":
			//do OR stuff
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), GP_Registers[destint].getContents(), 0, 0);
			break;
		case "0101":
			//do NOT stuff
			src1 = temp.substring(4,8);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), 0, GP_Registers[destint].getContents(), 0, 0);
			break;
		case "0110":
			//do SLL stuff, src2 is shift amount
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), GP_Registers[destint].getContents(), 0, 0);
			break;
		case "0111":
			//do SLR stuff, src2 is shift amount
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), GP_Registers[destint].getContents(), 0, 0);
			break;
		case "1000":
			//do SLT stuff
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), GP_Registers[destint].getContents(), 0, 0);
			break;
		case "1001":
			//do BEQ stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), destint, 0, 0);
			break;
		case "1010":
			//do BNEQ stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), GP_Registers[src2int].getContents(), destint, 0, 0);
			break;
		case "1011":
			//do ADDI stuff
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), src2int, GP_Registers[destint].getContents(), 0, 0);
			break;
		case "1100":
			//do SLTI stuff
			src1 = temp.substring(4,8);
			src2 = temp.substring(8,12);
			dest = temp.substring(12);
			src1int = Integer.parseInt(src1, 2);
			src2int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), src2int, GP_Registers[destint].getContents(), 0, 0);
			break;
		case "1101":
			//do LOAD stuff
			src1 = temp.substring(4,8);
			dest = temp.substring(8);
			src1int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), 0, GP_Registers[destint].getContents(), 0, 0);
			break;
		case "1110":
			//do STORE stuff
			src1 = temp.substring(4,8);
			dest = temp.substring(8);
			src1int = Integer.parseInt(src1, 2);
			destint = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), 0, GP_Registers[destint].getContents(), 0, 0);
			break;
		case "1111":
			//do JUMP stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			src1 = temp.substring(4);
			src1int = Integer.parseInt(src1, 2);
			Execute(opCodeString, GP_Registers[src1int].getContents(), 0, 0, 0, 0);
			break;
			
		}
		return pcReturnValue;
	}
	
	public void Execute(String opcode, int src1, int src2, int dest, int immediate, int address){
		switch(opcode){
		case "0000":
			//do NOP stuff
			
			break;
		case "0001":
			//do ADD stuff
			GP_Registers[dest].Update(ALU.ADD(src1, src2));
			break;
		case "0010":
			//do SUB stuff
			GP_Registers[dest].Update(ALU.SUBTRACT(src1, src2));
			break;
		case "0011":
			//do AND stuff
			GP_Registers[dest].Update(ALU.AND(src1, src2));
			break;
		case "0100":
			//do OR stuff
			GP_Registers[dest].Update(ALU.OR(src1, src2));
			break;
		case "0101":
			//do NOT stuff
			GP_Registers[dest].Update(ALU.NOT(src1));
			break;
		case "0110":
			//do SLL stuff, src2 is shift amount
			GP_Registers[dest].Update(ALU.SLL(src1,src2));
			break;
		case "0111":
			//do SLR stuff, src2 is shift amount
			GP_Registers[dest].Update(ALU.SLR(src1,src2));
			break;
		case "1000":
			//do SLT stuff
			GP_Registers[dest].Update(ALU.SLT(src1,src2));
			break;
		case "1001":
			//do BEQ stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			if(ALU.BEQ(src1,src2) == 1)
				//update branched PC
			else
				//increment PC normally
			break;
		case "1010":
			//do BNEQ stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			if(ALU.BNEQ(src1,src2) == 1)
				//update branched PC
			else
				//increment PC normally
			break;
		case "1011":
			//do ADDI stuff
			GP_Registers[dest].Update(ALU.ADDI(src1, src2));
			break;
		case "1100":
			//do SLTI stuff
			GP_Registers[dest].Update(ALU.SLTI(src1,src2));
			break;
		case "1101":
			//do LOAD stuff
			GP_Registers[dest].Update(Main_Memory.Load(src1));
			break;
		case "1110":
			//do STORE stuff
			Main_Memory.Store(src1,dest);
			break;
		case "1111":
			//do JUMP stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			break;
		
		}
	}
	

}
