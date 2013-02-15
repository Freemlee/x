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
	static byte[] tree1Bytes;
	public static void main(String args[]) {
		boolean first = true;
		Scanner standardInput = new Scanner(System.in);
		do {   
			// display prompt for user
			System.out.println();
			String resp = null;
			if (!first){
				System.out.println("Would you like to open another file? y/n");
				resp = standardInput.nextLine();
			}
			if (resp == "y" || first == true){
				System.out.println("What file would you like to open: ");
				FileInput file = new FileInput(standardInput.nextLine());
				tree1Bytes = file.readFile();
				theTree = new SuffixTreeAppl(new SuffixTree(tree1Bytes));
			}
			System.out.print("Enter the number of the task or type 'q' to quit: ");

			// read in a line from standard input
			String line = standardInput.nextLine();
			System.out.println();
			first = false;
			try {
				// try to extract an integer from line if possible
				int numTask = Integer.parseInt(line);
				

				switch (numTask) {
				case 1: 
					/*
					System.out.println("You entered '1'");
					String treeString = standardInput.nextLine();
					byte[] treeInput = treeString.getBytes();
					SuffixTree theTree = new SuffixTree(treeInput);
					SuffixTreeAppl ourTree = new SuffixTreeAppl(theTree); */
					System.out.println("What would you like to search the tree for");
					String searchString = standardInput.nextLine();
					byte[] searchBytes = searchString.getBytes();
					Task1Info result1 = theTree.searchSuffixTree(searchBytes);
					if (result1.getPos() == -1){
						System.out.println("There search text does not exist in the tree");
					}else{
						System.out.println("The index of the first character is " + result1.getPos() + 
								"\nNB. All indexes start counting from 1 upwards\n");
					}
					//System.out.println(theTree.toString());

					break;
				case 2: System.out.println("You entered '2'");
				/*
					treeString = standardInput.nextLine();
					treeInput = treeString.getBytes();
					theTree = new SuffixTree(treeInput);
					ourTree = new SuffixTreeAppl(theTree);
					*/
					System.out.println("What would you like to search the tree for");
					searchString = standardInput.nextLine();
					searchBytes = searchString.getBytes();
					String tString = new String(searchBytes);
					Task2Info result2 = theTree.allOccurrences(searchBytes);
					if (result2.getPositions().isEmpty()){
						System.out.println("There are no occurances of: " + tString);
					}
					for (int x : result2.getPositions()){
						System.out.println(tString + " occurs at index " + x);
					}
					break;
				case 3: System.out.println("You entered '3'");
				/*
					treeString = standardInput.nextLine();
					treeInput = treeString.getBytes();
					theTree = new SuffixTree(treeInput);
					ourTree = new SuffixTreeAppl(theTree);
					searchString = standardInput.nextLine();
					searchBytes = searchString.getBytes();
					*/
					Task3Info result3 = theTree.traverseForLrs();
					String str = "";
					int pos1 = result3.getPos1(); int pos2 = result3.getPos2();
					int len = result3.getLen();
					if (len!=0){
						str = new String(theTree.getTree().getString()).substring(pos1, pos1+len);
					}
					if (len == 0){
						System.out.println("There are no repeating substrings..");
					}else{
						System.out.printf("Longest Repeating Substring is: %s\nLength: %d\nFirst Occurance: %d\nLast Occurance: %d\n", str,len,pos1,pos2);
					}
					break;
				case 4: 
					System.out.println("You entered '4'"); 
					System.out.println("What is the second file you would like to open? - ");
					FileInput file = new FileInput(standardInput.nextLine());
					byte[] file2Bytes = file.readFile();
					//theTree = new SuffixTreeAppl(new SuffixTree(tree1Bytes,file2Bytes));
					SuffixTreeAppl Task4Tree = new SuffixTreeAppl(new SuffixTree(tree1Bytes, file2Bytes));
					Task4Info res = Task4Tree.traverseForLcs(tree1Bytes.length);
					System.out.println(res.getPos1());
					System.out.println(res.getPos2());								//swapped
					//System.out.println(new String(Task4Tree.getTree().getString()));
					System.out.println("Longest Common Substring is: " + new String(file2Bytes).substring(res.getPos1(), res.getLen() + res.getPos1()));
					System.out.println("Longest Common Substring is: " + new String(tree1Bytes).substring(res.getPos2(), res.getLen() + res.getPos2()));
					System.out.printf("Occurring at location %d in file1 and location %d in file2\n",res.getPos2(),res.getPos1());
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