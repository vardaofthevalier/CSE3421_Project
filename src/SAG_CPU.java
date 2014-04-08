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
		
		switch(opCodeString){
		case "0000":
			//do NOP stuff
			break;
		case "0001":
			//do ADD stuff
			break;
		case "0010":
			//do sub stuff
			break;
		case "0011":
			//do AND stuff
			break;
		case "0100":
			//do OR stuff
			break;
		case "0101":
			//do NOT stuff
			break;
		case "0110":
			//do SLL stuff
			break;
		case "0111":
			//do SLR stuff
			break;
		case "1000":
			//do SLF stuff
			break;
		case "1001":
			//do BEQ stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			break;
		case "1010":
			//do BNEQ stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			break;
		case "1011":
			//do ADDI stuff
			break;
		case "1100":
			//do SLTI stuff
			break;
		case "1101":
			//do LOAD stuff
			break;
		case "1110":
			//do STORE stuff
			break;
		case "1111":
			//do JUMP stuff
			//set new PC address to return
			//pcReturnValue = newPCValue
			break;
			
		}
		return pcReturnValue;
	}
	
	public void Execute(){
		
	}
	

}
