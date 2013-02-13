package SuffixTreePackage;

import java.util.Arrays;
import java.util.LinkedList;

/**
 * Class with methods for carrying out applications of suffix trees
 * David Manlove, Jan 03.  Modified by David Manlove, Jan 07 and Jan 09.
 */

public class SuffixTreeAppl {

	/** The suffix tree */
	private SuffixTree t;
	private LinkedList<Integer> occurences = null;

	/**
	 * Default constructor.
	 */
	public SuffixTreeAppl () {
		t = null;
	}
	
	/**
	 * Constructor with parameter.
	 * 
	 * @param tree the suffix tree
	 */
	public SuffixTreeAppl (SuffixTree tree) {
		t = tree;
	}
	
	/**
	 * Search the suffix tree t representing string s for a target x.
	 * Stores -1 in Task1Info.pos if x is not a substring of s,
	 * otherwise stores p in Task1Info.pos such that x occurs in s
	 * starting at s[p] (p counts from 0)
	 * - assumes that characters of s and x occupy positions 0 onwards
	 * 
	 * @param x the target string to search for
	 * 
	 * @return a Task1Info object
	 */
	public Task1Info searchSuffixTree(byte[] x) {
		Task1Info result = new Task1Info();
		boolean match = true;
		int startLocation = -1;
		int xIndex = 0;
		int nodeIndex = 0;
		int lengthAtNode;
		result.setPos(-2);
		SuffixTreeNode currentNode = t.getRoot().getChild();
		
		while (currentNode != null){
			lengthAtNode = currentNode.getRightLabel() - currentNode.getLeftLabel() + 1;
			String temp = new String(Arrays.copyOfRange(t.getString(), currentNode.getLeftLabel(), currentNode.getRightLabel()+1));
			/*System.out.println("Characters at Node : " + temp + "\n(" + currentNode.getLeftLabel() + 
					","+currentNode.getRightLabel()+") of " + new String(t.getString()) + " (length = " + lengthAtNode + ")");
		*/	//checks x with all the values at the currentNode
			nodeIndex = 0;
			match = true;
			while (nodeIndex < lengthAtNode && match && xIndex < x.length){
				int i = (currentNode.getLeftLabel()) + nodeIndex;
			/*	System.out.println("Comparing:\n\tThe " + xIndex + " of " + new String(x) + " WITH\n\t" +
						"The " + i + " of " + new String(t.getString())); */
				if (x[xIndex] != t.getString()[i]){
					//System.out.println("\t\tNO MATCH -- here should go straight to sibling");
					match = false;
					result.setPos(-1);
				}else{
					//System.out.println("\t\tMATCH");
					if (startLocation == -1){
						startLocation = currentNode.getLeftLabel();
					}
					xIndex++;
					nodeIndex++;
				}
				//nodeIndex++;
				//xIndex++;
			}
			if (match){
				currentNode = currentNode.getChild();
				//System.out.println("GOING TO CHILD");
			}else{
				currentNode = currentNode.getSibling();
				//System.out.println("GOING TO SIBLING");
			}
		}
		if (match == true){
			result.setPos(startLocation + 1);
		}
		return result;
			// if mismatch at current node check all of the siblings until no more siblings
				//at which point set match to false.
			// if a match at current node then set currentNode to be the child of that node (recurse)
			
	}

	/**
	 * Search suffix tree t representing string s for all occurrences of target x.
	 * Stores in Task2Info.positions a linked list of all such occurrences.
	 * Each occurrence is specified by a starting position index in s
	 * (as in searchSuffixTree above).  The linked list is empty if there
	 * are no occurrences of x in s.
	 * - assumes that characters of s and x occupy positions 0 onwards
	 * 
	 * @param x the target string to search for
	 * 
	 * @return a Task2Info object
	 */
	public Task2Info allOccurrences(byte[] x) {
		SuffixTreeNode currentNode= t.getRoot().getChild();
		Task2Info result = new Task2Info();
		occurences.clear();
		int tRes = 0;
		boolean isNull = true;
		getNextOccurrence(x);
		result.setPositions(occurences); // replace with your code!
		return result;
	}
	
	public void getNextOccurrence(byte[] x, SuffixTreeNode node) {
		int result = -2;
		boolean match = true;
		int startLocation = -1;
		int xIndex = 0;
		int nodeIndex = 0;
		int lengthAtNode;
		SuffixTreeNode currentNode = node;
		
		while (currentNode != null){
			lengthAtNode = currentNode.getRightLabel() - currentNode.getLeftLabel() + 1;
			String temp = new String(Arrays.copyOfRange(t.getString(), currentNode.getLeftLabel(), currentNode.getRightLabel()+1));
			System.out.println("Characters at Node : " + temp + "\n(" + currentNode.getLeftLabel() + 
					","+currentNode.getRightLabel()+") of " + new String(t.getString()) + " (length = " + lengthAtNode + ")");
			//checks x with all the values at the currentNode
			nodeIndex = 0;
			match = true;
			while (nodeIndex < lengthAtNode && match && xIndex < x.length){
				int i = (currentNode.getLeftLabel()) + nodeIndex;
				System.out.println("Comparing:\n\tThe " + xIndex + " of " + new String(x) + " WITH\n\t" +
						"The " + i + " of " + new String(t.getString())); 
				if (x[xIndex] != t.getString()[i]){
					System.out.println("\t\tNO MATCH -- here should go straight to sibling");
					match = false;
					result = -1;
				}else{
					System.out.println("\t\tMATCH");
					if (startLocation == -1){
						startLocation = currentNode.getLeftLabel();
					}
					xIndex++;
					nodeIndex++;
				}
				//nodeIndex++;
				//xIndex++;
			}
			if (match){
				currentNode = currentNode.getChild();
				System.out.println("GOING TO CHILD");
			}else{
				currentNode = currentNode.getSibling();
				System.out.println("GOING TO SIBLING");
			}
		}
		if (match == true){
			result = startLocation + 1;
		}

		System.out.println("ONE RESULT: " + result);
		
		//subTree.setRoot(currentNode);
		getNextOccurrence(x, currentNode);
		
		//return result;
			// if mismatch at current node check all of the siblings until no more siblings
				//at which point set match to false.
			// if a match at current node then set currentNode to be the child of that node (recurse)
			
	}

	/**
	 * Traverses suffix tree t representing string s and stores ln, p1 and
	 * p2 in Task3Info.len, Task3Info.pos1 and Task3Info.pos2 respectively,
	 * so that s[p1..p1+ln-1] = s[p2..p2+ln-1], with ln maximal;
	 * i.e., finds two embeddings of a longest repeated substring of s
	 * - assumes that characters of s occupy positions 0 onwards
	 * so that p1 and p2 count from 0
	 * 
	 * @return a Task3Info object
	 */
	public Task3Info traverseForLrs () {
		
		return null; // replace with your code!
	}

	/**
	 * Traverse generalised suffix tree t representing strings s1 (of length
	 * s1Length), and s2, and store ln, p1 and p2 in Task4Info.len,
	 * Task4Info.pos1 and Task4Info.pos2 respectively, so that
	 * s1[p1..p1+ln-1] = s2[p2..p2+ln-1], with len maximal;
	 * i.e., finds embeddings in s1 and s2 of a longest common substring 
         * of s1 and s2
	 * - assumes that characters of s1 and s2 occupy positions 0 onwards
	 * so that p1 and p2 count from 0
	 * 
	 * @param s1Length the length of s1
	 * 
	 * @return a Task4Info object
	 */
	public Task4Info traverseForLcs (int s1Length) {
		
		return null; // replace with your code!
	}
}
