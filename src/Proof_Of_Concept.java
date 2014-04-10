import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Scanner;
import java.awt.*;
import java.awt.event.*;

import javax.swing.*; 

import jpl.*;


public class Proof_Of_Concept extends JFrame implements ActionListener {
	// GUI Attributes
	private JTextField command_prompt;
	private JLabel device1;
	private JLabel device2;
	private JLabel device3;
	private JLabel device4;
	
	private ImageIcon not_avail;
	private ImageIcon off;
	private ImageIcon on;
	
	private String next_command;
	
	// Constructor
	Proof_Of_Concept() {
		super("SwitcherOO"); 
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(1250, 350, 1250, 350);
		
		not_avail = new ImageIcon("not_av.png");
		off = new ImageIcon("off.png");
		on = new ImageIcon("on.png");
		
		device1 = new JLabel("Device 1", not_avail, JLabel.CENTER);
		device1.setVerticalTextPosition(JLabel.BOTTOM);
		device1.setHorizontalTextPosition(JLabel.CENTER);
		
		device2 = new JLabel("Device 2", not_avail, JLabel.CENTER);
		device2.setVerticalTextPosition(JLabel.BOTTOM);
		device2.setHorizontalTextPosition(JLabel.CENTER);
		
		device3 = new JLabel("Device 3", not_avail, JLabel.CENTER);
		device3.setVerticalTextPosition(JLabel.BOTTOM);
		device3.setHorizontalTextPosition(JLabel.CENTER);
		
		device4 = new JLabel("Device 4", not_avail, JLabel.CENTER);
		device4.setVerticalTextPosition(JLabel.BOTTOM);
		device4.setHorizontalTextPosition(JLabel.CENTER);
		
		command_prompt = new JTextField(50);
		command_prompt.addActionListener(this);
		
		Container con = this.getContentPane();
		con.add(device1);
		con.add(device2);
		con.add(device3);
		con.add(device4);
		con.add(command_prompt);
		con.setLayout(new FlowLayout());
		
		next_command = "";
		
		setVisible(true);
		
	}
	
	// Methods
	public void actionPerformed(ActionEvent event) {
		// program logic goes here
		next_command = command_prompt.getText();
		command_prompt.setText("");
		
		// Consult the assembler file
		Query consult = new Query("consult", new Term[] {new Atom("sag_assembler.pro")});
		consult.query();
		
		// Create a query to assemble the command entered by the user
		Query assemble = new Query("assemble_command", new Term[] { new Atom(next_command) });
		
		// Run the query and get the output
		
		// Execute the assembled instructions
		
		// Check device states, and update the images appropriately
		
		
	}
	
	public static void main(String [] args){
		new Proof_Of_Concept();
		 
	}

}
