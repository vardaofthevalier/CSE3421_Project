
public class ALU {
	
	// Member variables
	
	// Constructor

	
	// Methods
	public Integer Add(int src1, int src2)
	{
		return src1 + src2;
	}
	
	public Integer Subtract(int src1, int src2)
	{
		return src1 - src2;
	}
	
	public Integer And(int src1, int src2)
	{
		return src1 & src2;
	}
	
	public Integer Or(int src1, int src2)
	{
		return src1 | src2;
	}
	
	public Integer Not(int src1)
	{
		return ~src1;
	}
	
	public Integer SLL(int src1, int src2)
	{
		return src1 << src2;
	}
	
	public Integer SLR(int src1, int src2)
	{
		return src1 >> src2;
	}
	
	public Integer SLT(int src1, int src2)
	{
		if(src1 < src2)
			return 1;
		else
			return 0;
	}
	
	
	//Immediate Methods
	public Integer Addi(int src1, int src2)
	{
		return src1 + src2;
	}
	
	public Integer SLTI(int src1, int src2)
	{
		return src1 << src2;
	}
}
