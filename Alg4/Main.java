import java.util.*;
import SuffixTreePackage.*;

/**
 * Main class - for accessing suffix tree applications
 * David Manlove, Jan 03.  Modified by David Manlove, Jan 07 and Jan 09.
 */

public class Main {

	/**
	 * The main method.
	 * @param args the arguments
	 */
	public static void main(String args[]) {

		Scanner standardInput = new Scanner(System.in);
		do {   
			// display prompt for user
			System.out.println();
			System.out.print("Enter the number of the task or type 'q' to quit: ");

			// read in a line from standard input
			String line = standardInput.nextLine();
			System.out.println();

			try {
				// try to extract an integer from line if possible
				int numTask = Integer.parseInt(line);
				

				switch (numTask) {
				case 1: 
					System.out.println("You entered '1'");
					String treeString = standardInput.nextLine();
					byte[] treeInput = treeString.getBytes();
					SuffixTree theTree = new SuffixTree(treeInput);
					SuffixTreeAppl ourTree = new SuffixTreeAppl(theTree);
					System.out.println("What would you like to search the tree for");
					String searchString = standardInput.nextLine();
					byte[] searchBytes = searchString.getBytes();
					Task1Info result = ourTree.searchSuffixTree(searchBytes);
					if (result.getPos() == -1){
						System.out.println("There search text does not exist in the tree");
					}else{
						System.out.println("The index of the first character is " + result.getPos() + 
								"\nNB. All indexes start counting from 1 upwards\n");
					}
					//System.out.println(theTree.toString());

					break;
				case 2: System.out.println("You entered '2'");
				
					treeString = standardInput.nextLine();
					treeInput = treeString.getBytes();
					theTree = new SuffixTree(treeInput);
					SuffixTreeAppl ourTree2 = new SuffixTreeAppl(theTree);
					System.out.println("What would you like to search the tree for");
					searchString = standardInput.nextLine();
					searchBytes = searchString.getBytes();
					Task2Info result2 = ourTree2.allOccurrences(searchBytes);
					System.out.println("Locations are: " + result2.getPositions().toString());
					break;
				case 3: System.out.println("You entered '3'"); break;
				case 4: System.out.println("You entered '4'"); break;
				/* replace the above four lines with code to display relevant
				 * output for each task    
                 *
				 * in the case of Tasks 1, 2 and 3, get the name of a text file
				 * from standard input; in the case of Task 4, get the names of
				 * two text files from standard input

				 * then, in all cases, read the data from the text file(s) using 
				 * the FileInput class and build the relevant suffix tree

				 * in the case of Tasks 1 and 2, get a string from standard input
				 * and convert the string to bytes, with the relevant information
				 * stored in the array of bytes from positions 0 onwards

				 * then call the relevant method from above to process the
				 * information, and display the output using System.out.print
				 * and System.out.println */

				default: throw new NumberFormatException();
				}
			}
			catch (NumberFormatException e) {
				if (line.length()==0 || line.charAt(0)!='q')
					System.out.println("You must enter either '1', '2', '3', '4' or 'q'.");
				else
					break;
			}
		} while (true);
		standardInput.close();
	}
}