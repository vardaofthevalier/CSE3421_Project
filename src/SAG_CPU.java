
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
	
	public void Load_Program(){
		// Load an assembled program into memory
	}
	
	public void Fetch_Instruction(){
		// Fetch an instruction
	}
	
	public void Execute(){
		// Execute an instruction
	}
	

}
