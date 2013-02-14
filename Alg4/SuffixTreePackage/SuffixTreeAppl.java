package SuffixTreePackage;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * Class with methods for carrying out applications of suffix trees
 * David Manlove, Jan 03.  Modified by David Manlove, Jan 07 and Jan 09.
 */

public class SuffixTreeAppl {

	/** The suffix tree */
	private SuffixTree t;
	private SuffixTreeNode t2Node;

	/**
	 * Default constructor.
	 */
	public SuffixTreeAppl () {
		t = null;
		t2Node = null;
	}
	
	public SuffixTree getTree(){
		return t;
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
		
		//Previously currentNode != null
		while (currentNode != null){
			lengthAtNode = currentNode.getRightLabel() - currentNode.getLeftLabel() + 1;
			//String temp = new String(Arrays.copyOfRange(t.getString(), currentNode.getLeftLabel(), currentNode.getRightLabel()+1));
			/*System.out.println("Characters at Node : " + temp + "\n(" + currentNode.getLeftLabel() + 
					","+currentNode.getRightLabel()+") of " + new String(t.getString()) + " (length = " + lengthAtNode + ")");*/
			//checks x with all the values at the currentNode
			nodeIndex = 0;
			match = true;
			while (nodeIndex < lengthAtNode && match && xIndex < x.length){
				int i = (currentNode.getLeftLabel()) + nodeIndex;
				
				/*System.out.println("Comparing:\n\tThe " + xIndex + " of " + new String(x) + " WITH\n\t" +
						"The " + i + " of " + new String(t.getString())); */
				if (x[xIndex] != t.getString()[i]){
					//System.out.println("\t\tNO MATCH -- here should go straight to sibling");
					match = false;
					result.setPos(-1);
					//NEWLINE
					startLocation = -1;
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
				if (currentNode.getChild() != null)
					t2Node = currentNode;
				currentNode = currentNode.getChild();
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
		searchSuffixTree(x);
		if (t2Node == null){
			System.out.println("ah-ha!");
		}
		//System.out.println(t2Node.getSuffix());
		//System.out.println(t2Node.getSuffix());
		if (t2Node.getChild() != null && t2Node.getSibling() != null){
			System.out.println("Not a LEAF!");
		}
		LinkedList<SuffixTreeNode> leaves = getLeaves(t2Node);
		Task2Info res = new Task2Info();
		System.out.println("Size = " + leaves.size());
		for (SuffixTreeNode node: leaves){
			res.addEntry(node.getSuffix());
		}
		return res;
	}
	
	private LinkedList<SuffixTreeNode> getLeaves(SuffixTreeNode x){
		LinkedList<SuffixTreeNode> currentPath = new LinkedList<SuffixTreeNode>();
		LinkedList<SuffixTreeNode> leaves = new LinkedList<SuffixTreeNode>();
		//SuffixTreeNode currentNode = x.getChild();
		currentPath.add(x); // swapped this statement with the above (x here was also currentNode)
		SuffixTreeNode currentNode = x.getChild();
		boolean visited = false;
		while (!currentPath.isEmpty()){
			currentNode = currentPath.getLast();
			if (currentNode.getChild() != null && !visited){
				currentPath.add(currentNode.getChild());
				System.out.println("Going to child");
			}else{
				//it is a leaf
				leaves.add(currentNode);
				System.out.println("Adding to leaves");
				if (currentNode.getSibling() != null){
					currentPath.removeLast();
					currentPath.add(currentNode.getSibling());
					System.out.println("Going to sibling");
				}else{
					currentPath.removeLast();
					System.out.println("Going up");
				}
			}
			visited = true;
		}
		return leaves;
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
		long start = System.currentTimeMillis();
		//SuffixTreeNode bestNode;
		SuffixTreeNode currentNode = t.getRoot().getChild();
		LinkedList<SuffixTreeNode> bestLsrNodes = new LinkedList<SuffixTreeNode>();
		LinkedList<SuffixTreeNode> currentPath = new LinkedList<SuffixTreeNode>();
		currentPath.addLast(currentNode);
		/*if prevLength is >= to the currentLength we want to 
		remove a node from the tree (and decrement prevLength, otherwise we keep it.
		*/
		
		boolean isLast = false;
		while (currentPath != null || isLast){
			//System.out.println("Current size of path: " + currentPath.size());
			currentNode = currentPath.getLast();
			if (isValidBranch(currentNode)){
				//System.out.println("VALID BRANCH");
			}
			//System.out.println("Current length = " + getLength(currentPath) + 
			//		"\nBest length = " + getLength(bestLsrNodes));

			if (getLength(currentPath) > getLength(bestLsrNodes) && isValidBranch(currentNode)){
				//bestLsrNodes = currentPath;
				bestLsrNodes.clear();
				for (SuffixTreeNode n: currentPath){
					bestLsrNodes.addLast(n);
				}
				//System.out.println("New best - " + getLength(bestLsrNodes));
			}
			currentPath = next(currentPath);
		}
		System.out.println(System.currentTimeMillis() - start);
		Task3Info result = new Task3Info();
		//System.out.println("FINAL BEST IS " + getLength(bestLsrNodes));
		if (bestLsrNodes.isEmpty()){
			return result;
		}else{
			int[] positions = getLeafSuffixes(bestLsrNodes.getLast());
			result.setPos1(positions[0]);
			result.setPos2(positions[1]);
			result.setLen(getLength(bestLsrNodes));
			return result;
		}
		
		/*
		if (getLengthOfNode(currentNode) + prevLength > bestLength){
			if (currentDepth < lsrNodes.size()){
				currentPath.addLast(currentNode);
				currentDepth++;
			}else{
				lsrNodes.removeLast();
				lsrNodes.addLast(currentNode);
			}
		}*/
	}
	
	//Gets the leaf nodes of a branch node (when immediate parent, check with isValidBranch(x) first)
	public int[] getLeafSuffixes(SuffixTreeNode x){
		int[] result = new int[2];
		SuffixTreeNode currentNode = x.getChild();
		result[0] = currentNode.getSuffix();
		currentNode = currentNode.getSibling();
		result[1] = currentNode.getSuffix();
		return result;
	}
	
	public LinkedList<Integer> getAllOccurances(SuffixTreeNode x){
		LinkedList<Integer> result = new LinkedList<Integer>();
		//x = x.getChild();
		while (x != null){
			if (x.getChild() == null)
				result.add(new Integer(x.getSuffix()));
			x = x.getSibling();
		}
		return result;
	}
	
	public int getLength(LinkedList<SuffixTreeNode> x){
		int len = 0;
		if (x.isEmpty()){
			return 0;
		}
		for (SuffixTreeNode node: x){
			len += getLengthOfNode(node);
		}
		return len;
	}
	
	public LinkedList<SuffixTreeNode> next(LinkedList<SuffixTreeNode> x){
		SuffixTreeNode currentNode = x.getLast();
		boolean visited = false;
		boolean isBad = true;
		while (isBad || x.isEmpty()){
			if(currentNode.getChild() != null && !visited){
				x.addLast(currentNode.getChild());
				isBad = false;
			}else if (currentNode.getSibling() != null){
				x.removeLast();
				x.addLast(currentNode.getSibling());
				isBad = false;
			}else{
				if (!x.isEmpty()){
					x.removeLast();
					if (!x.isEmpty())
						currentNode = x.getLast();	
					else{
						isBad = false;
					}
				}else{
					//System.out.println("Arrived at last node");
					break;
				}
				visited = true;
			}
		}
		if (x.isEmpty()){
			//System.out.println("Finished Iterating");
			return null;
		}
		return x;
	}
	
	//returns true if a branch ONLY has leaf nodes (at least 2)
	public boolean isValidBranch(SuffixTreeNode x){
		//if the node is a leaf..
		boolean result = true;
		if (x.getChild() == null){
			return false;
		}else{
			SuffixTreeNode currentNode = x.getChild();
			while (currentNode.getSibling() != null){
				if (currentNode.getSuffix() == -1)
					result = false;
				currentNode = currentNode.getSibling();
			}
		}
		return result;
	}
	
	public int getLengthOfNode(SuffixTreeNode x){
		return x.getRightLabel() - x.getLeftLabel() + 1;
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
