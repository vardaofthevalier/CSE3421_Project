
public class Program_Counter {
	
	// Member variables
	
	private int count;
	
	// Constructor
	
	public Program_Counter(){
		this.count = 0;
	}
	
	//Methods
	
	public void Reset(){
		this.count = 0;
	}
	
	public void Increment(int amt){
		this.count += amt;
	}

}
