/* Grammar Rules 
		 * 
		 * Program Parse:
		 * 
		 * <program> -> PROGRAM <identifier>
		 * 				{ <data> }
		 * 				{ <routine> }
		 * 				END <identifier>
		 * 
		 * <data> -> { <identifier> <value> | <pseudo-instruction> }
		 * 
		 * <routine> -> BEGIN <identifier>
		 * 				{ <instruction> }
		 * 				END <identifier>
		 * 
		 * 
		 * Instruction Parse:
		 * 
		 * <instruction> -> <math-op> <reg> <reg> <reg> | <imm-op> <reg> <imm-value> | <jump-op> <address>
		 * 
		 * <pseudo-instruction> -> (not sure yet)
		 * 
		 * <math-op> -> NOOP | ADD | SUB | AND | OR | NOT | SLL | SLR | SLT | BEQ | BNEQ
		 * 
		 * <reg> -> %<decimal-number>
		 * 
		 * <imm-op> -> ADDI | SLTI | LOAD | STORE
		 * 
		 * <imm-value> -> <binary-number>  
		 * 
		 * <jump-op> -> JUMP
		 * 
		 * <address> -> <binary-number>   
		 * 
		 * 
		 * Tokenizer:
		 * 
		 * <identifier> -> { <capital-letter> } 
		 * 
		 * <value> -> <binary-number>
		 * 
		 * <binary-number> -> { 0 | 1 }
		 * 
		 * <decimal-number> -> { 0 | 1 | 2 | 3 | ... | 15 }
		 * 
		 * <capital-letter> -> A | B | C | ... | Z
		 * 
		 * */

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;


public class SAG_Language_Assembler {
	
	// Constructor
	
	private static SAG_Language_Assembler instance = null; 
	
	private SAG_Language_Assembler()
	{
		// Empty constructor (Singleton)
	}
	
	public static synchronized SAG_Language_Assembler getInstance()
	{
		if (instance == null)
		{
			instance = new SAG_Language_Assembler();
		}
		
		return instance;
	}
	
	// Additional Classes/Enums
	
	public class ParseTree
	{
		String root;
		ArrayList<ParseTree> children;
		
		public ParseTree(String token)
		{
			this.root = token;
			this.children = new ArrayList<ParseTree>();
		}
		
		public void Insert(String token)
		{
			ParseTree newChild = new ParseTree(token);
			this.children.add(newChild);
		}
		
	}
	
	public enum ArithmeticOperations
	{
		NOOP, ADD, SUB, AND, OR, NOT, SLL, SLR, SLT, BEQ, BNEQ
	}
	
	public enum ImmediateOperations
	{
		ADDI, SLTI, LOAD, STORE
	}
	
	public enum JumpOperations
	{
		JUMP
	}
	
	public enum PseudoOperations
	{
		// List of pseudo operations
	}
	
	// Methods
	
	public void AssembleProgram(String inputFile, String outputFile) throws IOException
	{
		// Read in the file ("program") as a string
		BufferedReader fileReader = new BufferedReader(new FileReader(inputFile));
		String nextLine = null;
		StringBuilder fileContents = new StringBuilder();
		String separator = System.getProperty("line.separator");
		
		while((nextLine = fileReader.readLine()) != null){
	        fileContents.append(nextLine);
	        fileContents.append(separator);
	    }
		
		fileReader.close();
		
		// Break into tokens
		
		ArrayList<String> tokens = Tokenize(fileContents.toString());
		
		// Parse
		ParseTree ptree = ProgramParse(tokens);
		
		// Generate machine code
		String machineCode = GenerateCode(ptree);
		
		// Write the machine code to a file ("destination")
		
	}

	public static ArrayList<String> Tokenize(String prog)
	{		
		ArrayList<String> tokens = new ArrayList<String>(Arrays.asList(prog.split("\\s")));
		
		return tokens;
	}
	
	public ParseTree ProgramParse(ArrayList<String> tokens)
	{
		// Create a parse tree out of the string of tokens using the grammar rules for the language
		ParseTree program = new ParseTree("");
		
		if (!tokens.isEmpty()){
			String nextToken = tokens.remove(0);
			
			if (nextToken == "PROGRAM")
			{
				program.root = nextToken;
				
				if (!tokens.isEmpty()){
				
					nextToken = tokens.remove(0);
					
					if (isValidIdentifier(nextToken))
					{
						program.Insert(nextToken);
					}
					
					else
					{
						System.out.println("Error: The program identifier is not valid.");
						
					}
					
					while (!tokens.isEmpty() && nextToken != "END"){
						// Recursively descend
					}
				}
			}
			else{
				// Not a valid program, report error
			}
		
		}
		else{
			// Nothing to assemble, report error
		}
		
		// Lots of stuff here
		
		// Return the parse tree
		return program;
	}
	
	public ParseTree InstructionParse(ArrayList tokens)
	{
		ParseTree itree = new ParseTree("Placeholder");
		
		return itree;
	}
	

	public String GenerateCode(ParseTree ptree)
	{
		// Use the parse tree to generate the machine code
		String machineCode = "Placeholder";
		
		// Write the machine code to a file
		return machineCode;
	}
	
	public boolean isValidIdentifier(String token){
		boolean isValidIdentifier = false;
		
		return isValidIdentifier;
	}

}
