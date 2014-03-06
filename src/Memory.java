
public class Memory {
	
	// Member variables
	
	private int[] memory_contents;

	
	// Constructor
	
	public Memory(int address_width){
		this.memory_contents = new int[(int) Math.pow(2, address_width)];
	}
	
	//Methods
	
	public void Load(int instruction_address){
		// Load an instruction from instruction memory into program memory
	}
	
	public void Store(int value, int data_address){
		// Store the value at the given data address
	}
	
	public void Reset(){
		// Clear memory contents
	}
	

}
