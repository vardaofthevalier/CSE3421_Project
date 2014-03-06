
public class Register {
	
	// Member variables
	Object contents;

	// Constructor
	public Register(){
		this.contents = null;
	}
	
	// Methods
	
	public void Update(int value){
		this.contents = value;
	}
	public void Reset(){
		this.contents = null;
	}
}
