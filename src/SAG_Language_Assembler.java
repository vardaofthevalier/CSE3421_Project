import java.io.File;
import java.util.ArrayList;


public class SAG_Language_Assembler {
	
	// Member variables
	
	private static SAG_Language_Assembler instance = null; 

	// Constructor
	
	private SAG_Language_Assembler()
	{
		// Empty constructor 
	}
	
	// Methods
	
	public static synchronized SAG_Language_Assembler getInstance()
	{
		if (instance == null)
		{
			instance = new SAG_Language_Assembler();
		}
		
		return instance;
	}
	
	public static class ParseTree
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
			
		}
		
	}
	
	public void AssembleProgram(File program, File destination)
	{
		// Read in the file ("program") as a string
		String prog = new String("Placeholder");
		
		// Break into tokens
		String[] tokens = Tokenize(prog);
		
		// Parse
		ParseTree ptree = Parse(tokens);
		
		// Generate machine code
		String machineCode = GenerateCode(ptree);
		
		// Write the machine code to a file ("destination")
		
	}
	public String[] Tokenize(String program)
	{
		// Break the SAG Assembly Language Program into tokens
		String[] tokens = program.split("\\s");
		// Return the string of tokens
		return tokens;
		
	}
	
	public ParseTree Parse(String[] tokens)
	{
		// Create a parse tree out of the string of tokens using the grammar rules for the language
		ParseTree ptree = new ParseTree("Placeholder");
		
		// Lots of stuff here
		
		// Return the parse tree
		return ptree;
	}
	
	
	public String GenerateCode(ParseTree ptree)
	{
		// Use the parse tree to generate the machine code
		String machineCode = "Placeholder";
		
		// Write the machine code to a file
		return machineCode;
	}

}
