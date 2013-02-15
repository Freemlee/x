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
	static SuffixTreeAppl theTree;
	static byte[] treeBytes;
	static byte[] searchTerm;
	
	public static void displayIndexWarning(){
		System.out.println("NB. All indexes start counting from 1 upwards");
	}
	
	public static void main(String args[]) {
		Scanner standardInput = new Scanner(System.in);
		do {   
			System.out.println();		
			System.out.print("Enter the number of the task or type 'q' to quit: ");

			String line = standardInput.nextLine();
			System.out.println();
			try {
				// try to extract an integer from line if possible
				int numTask = Integer.parseInt(line);
				

				switch (numTask) {
				case 1: 
					System.out.print("What file would you like to search: ");
					treeBytes = new FileInput(standardInput.nextLine()).readFile();
					System.out.print("What would you like to search the tree for: ");
					searchTerm = standardInput.nextLine().getBytes();
					theTree = new SuffixTreeAppl(new SuffixTree(treeBytes));
					Task1Info result1 = theTree.searchSuffixTree(searchTerm);
					if (result1.getPos() == -1){
						System.out.println("The string \"" + new String(searchTerm) + "\" does not occur");
					}else{
						System.out.println("The string \"" + new String(searchTerm) + "\" occurs at position " + result1.getPos());
						displayIndexWarning();
					}
					break;
				case 2:
					System.out.print("What file would you like to search: ");
					treeBytes = new FileInput(standardInput.nextLine()).readFile();
					System.out.print("What would you like to search the tree for: ");
					searchTerm = standardInput.nextLine().getBytes();
					theTree = new SuffixTreeAppl(new SuffixTree(treeBytes));
					//System.out.println(new String(searchTerm));
					Task2Info result2 = theTree.allOccurrences(searchTerm);
					if (result2.getPositions().isEmpty()){
						System.out.println("The string \"" + new String(searchTerm) + "\" does not occur");
					}else{
						System.out.println("The string \"" + new String(searchTerm) + "\" occurs " + result2.getPositions().size() + " times at positions: ");
						for (int x : result2.getPositions()){
							System.out.println(x);
						}
						displayIndexWarning();
					}
					break;
				case 3:
					System.out.print("What file would you like to search: ");
					treeBytes = new FileInput(standardInput.nextLine()).readFile();
					theTree = new SuffixTreeAppl(new SuffixTree(treeBytes));
					Task3Info result3 = theTree.traverseForLrs();
					String str = "";
					int pos1 = result3.getPos1(); int pos2 = result3.getPos2();
					int len = result3.getLen();
					if (len!=0){
						str = new String(theTree.getTree().getString()).substring(pos1, pos1+len);
					}
					if (len == 0){
						System.out.println("There are no repeating substrings");
					}else{
						System.out.printf("Longest Repeating Substring is: %s\nLength: %d\nOne occurrence is at position %d\nAnother occurrence is at position %d\n", str,len,pos1,pos2);
						displayIndexWarning();
					}
					break;
				case 4: 
					System.out.print("What is the first file would you like to search: ");
					String file1Name = standardInput.nextLine();
					treeBytes = new FileInput(file1Name).readFile();
					System.out.print("What is the second file would you like to search: ");
					String file2Name = standardInput.nextLine();
					byte[] tree2Bytes = new FileInput(file2Name).readFile();
					System.out.print("What would you like to search the tree for: ");
					theTree = new SuffixTreeAppl(new SuffixTree(treeBytes, tree2Bytes));
					Task4Info res = theTree.traverseForLcs(treeBytes.length);
					if (res.getLen() == 0){
						System.out.println("There are no common substrings");
					}else{
						System.out.println("Longest Common Substring is: " + new String(tree2Bytes).substring(res.getPos1(), res.getLen() + res.getPos1()));
						System.out.printf("Occurring at position %d in %s and position %d in %s\n",res.getPos2(), file1Name, res.getPos1(), file2Name);
						displayIndexWarning();
					}
					break;
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