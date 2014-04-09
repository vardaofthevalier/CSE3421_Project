
public class ALU {
	
	// Member variables
	
	// Constructor

	
	// Methods
	public Integer ADD(int src1, int src2)
	{
		return src1 + src2;
	}
	
	public Integer SUBTRACT(int src1, int src2)
	{
		return src1 - src2;
	}
	
	public Integer AND(int src1, int src2)
	{
		return src1 & src2;
	}
	
	public Integer OR(int src1, int src2)
	{
		return src1 | src2;
	}
	
	public Integer NOT(int src1)
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
	
	public Integer BEQ(int src1, int src2)
	{
		if(src1 == src2)
			return 1;
		else
			return 0;
	}

	public Integer BNEQ(int src1, int src2)
	{
		if(src1 != src2)
			return 1;
		else
			return 0;
	}

	//Immediate Methods
	public Integer ADDI(int src1, int imm)
	{
		return src1 + imm;
	}
	
	public Integer SLTI(int src1, int imm)
	{
		return src1 << imm;
	}
}
