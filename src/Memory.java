import java.util.Arrays;


public class Memory {
	
	// Member variables
	
	
	private int[] memory_contents;

	
	// Constructor
	
	public Memory(int address_width){
		this.memory_contents = new int[(int) Math.pow(2, address_width)];
	}
	
	//Methods
	
	public int Load(int instruction_address){
		return memory_contents[instruction_address];
		// Load an instruction from instruction memory into program memory
	}
	
	public void Store(int value, int data_address){
		// Store the value at the given data address
		memory_contents[data_address] = value;
	}
	
	public void Reset(){
		// Clear memory contents
		Arrays.fill(memory_contents, 0);
	}
	

}
