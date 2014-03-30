import java.io.IOException;


public class Proof_Of_Concept {
	
	public static void main(String [] args){
	
		// First, assemble a SAG Assembly Language program into machine language that can be understood by the SAG CPU
	
		SAG_Language_Assembler assembler = SAG_Language_Assembler.getInstance();
		
		String inputFile = new String("test_program.sag");
		String outputFile = new String("test_program.out");
		try {
			assembler.AssembleProgram(inputFile, outputFile);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	
		// Initialize the SAG CPU
	
		// Run the assembled program on the SAG CPU, watch the magic happen! :)
	}

}
