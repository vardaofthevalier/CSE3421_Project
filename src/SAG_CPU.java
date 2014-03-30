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
		PC.Increment(1);
		
	}
	
	public void Execute(){
		// Execute an instruction
	}
	

}
