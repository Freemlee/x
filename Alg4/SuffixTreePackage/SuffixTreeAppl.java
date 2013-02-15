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
	private LinkedList<SuffixTreeNode> occurrences = null;

	/**
	 * Default constructor.
	 */
	public SuffixTreeAppl () {
		t = null;
		t2Node = null;
		//occurrences = new LinkedList<SuffixTreeNode>();
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
		
		while (currentNode != null){
			lengthAtNode = currentNode.getRightLabel() - currentNode.getLeftLabel() + 1;
			//checks x with all the values at the currentNode
			nodeIndex = 0;
			match = true;
			while (nodeIndex < lengthAtNode && match && xIndex < x.length){
				int i = (currentNode.getLeftLabel()) + nodeIndex;
				if (x[xIndex] != t.getString()[i]){
					match = false;
					result.setPos(-1);
					startLocation = -1;
				}else{
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
					/*t2Node is a global variable that is used when calulating the Task2Info
					 * this is because most of this code is useful and the redundancy for Task2Info (ie. setting a pos)
					 * does not leave a great overhead (imo this is better than 
					 */
					t2Node = currentNode;
				currentNode = currentNode.getChild();
			}else{
				currentNode = currentNode.getSibling();
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
		occurrences = new LinkedList<SuffixTreeNode>();
		if (t2Node == null){
			//File does not exist
			return new Task2Info();
		}
		getLeafDecendants(t2Node.getChild());
		Task2Info res = new Task2Info();
		for (SuffixTreeNode node: occurrences){
			res.addEntry(node.getSuffix());
		}
		return res;
	}
	
	public void getLeafDecendants(SuffixTreeNode currentNode){
		// was currentNode.getSibling()
		while (currentNode != null){
			if (currentNode.getChild() == null){
				occurrences.add(currentNode);
				currentNode = currentNode.getSibling();
			}else{
				getLeafDecendants(currentNode.getChild());
				//new line
				currentNode = currentNode.getSibling();
				
			}
		}
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
		SuffixTreeNode currentNode = t.getRoot().getChild();
		LinkedList<SuffixTreeNode> bestLsrNodes = new LinkedList<SuffixTreeNode>();
		LinkedList<SuffixTreeNode> currentPath = new LinkedList<SuffixTreeNode>();
		currentPath.addLast(currentNode);
		
		boolean isLast = false;
		while (currentPath != null || isLast){
			currentNode = currentPath.getLast();
			if (isValidBranch(currentNode)){
			}
			if (getLength(currentPath) > getLength(bestLsrNodes) && isValidBranch(currentNode)){
				bestLsrNodes.clear();
				for (SuffixTreeNode n: currentPath){
					bestLsrNodes.addLast(n);
				}
			}
			currentPath = next(currentPath);
		}
		Task3Info result = new Task3Info();
		if (bestLsrNodes.isEmpty()){
			return result;
		}else{
			int[] positions = getLeafSuffixes(bestLsrNodes.getLast());
			result.setPos1(positions[0]);
			result.setPos2(positions[1]);
			result.setLen(getLength(bestLsrNodes));
			return result;
		}
	}
	
	/*Gets 2 the leaf nodes of a branch node, should be checked with 
	isValidBranch(x) first. (when immediate parent, check with isValidBranch(x) first)*/
	private int[] getLeafSuffixes(SuffixTreeNode x){
		int[] result = new int[2];
		SuffixTreeNode currentNode = x.getChild();
		result[0] = currentNode.getSuffix();
		currentNode = currentNode.getSibling();
		result[1] = currentNode.getSuffix();
		return result;
	}
	
	//Gets the collective length of all of the nodes in a list.
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
	
	/*
	 * Iterates through the tree, returning a list representing the "children" 
	 * with each the last node being the current node and the preceding nodes being the parents.
	 * The iteration is done depth first.
	 */
	public LinkedList<SuffixTreeNode> next(LinkedList<SuffixTreeNode> x){
		SuffixTreeNode currentNode = x.getLast();
		//necessary when visiting parents as it prevents them from immediately visiting the child.
		boolean visited = false; 
		
		/*used as a loop escape, once the path is at an acceptable configuration, 
		 * required as sometimes the parents must be visited several times.
		 */
		boolean isBad = true;
		while (isBad || x.isEmpty()){
			
			//Go to the child if not already visited.
			if(currentNode.getChild() != null && !visited){
				x.addLast(currentNode.getChild());
				isBad = false;
				
			//If not, then go to the sibling if possible
			}else if (currentNode.getSibling() != null){
				x.removeLast();
				x.addLast(currentNode.getSibling());
				isBad = false;
				
			//If not, then go up the list as high as required.
			}else{
				if (!x.isEmpty()){
					//Go up one step
					x.removeLast();
					if (!x.isEmpty())
						currentNode = x.getLast();	
					else{
						//stop looping if empty
						isBad = false;
					}
				}else{
				 /* break needed so that items are not continually removed from the list
					after the list is empty, prevents NoSuchElementException */
					break;
				}
				//prevent from going to the child in next iteration.
				visited = true;
			}
		}
		if (x.isEmpty()){
			//returns null when no more paths are available
			return null;
		}
		return x;
		/*NB. Multiple handlers of x.isEmpty() are necessary as items are removed from x
		 * at various stages in its execution. All are to prevent either 
		 * NoSuchElementExceptions or to escape the loop.
		 */
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
		SuffixTreeNode currentNode = t.getRoot().getChild();
		LinkedList<SuffixTreeNode> bestLsrNodes = new LinkedList<SuffixTreeNode>();
		LinkedList<SuffixTreeNode> currentPath = new LinkedList<SuffixTreeNode>();
		LinkedList<SuffixTreeNode> tempLeaves = new LinkedList<SuffixTreeNode>();
		currentPath.addLast(currentNode);
		boolean isLast = false;
		boolean left = false;
		boolean right = false;
		while (currentPath != null || isLast){
			currentNode = currentPath.getLast();
			if (getLength(currentPath) > getLength(bestLsrNodes) && isValidBranch(currentNode)){
				tempLeaves = getImmidiateLeafDescs(currentNode);
				for (SuffixTreeNode node:tempLeaves){
					if (node.getSuffix() < s1Length){
						left = true;
					}else if(node.getSuffix() > s1Length){
						right = true;
					}
				}
				if (left && right){
					bestLsrNodes.clear();
					for (SuffixTreeNode n: currentPath){
						bestLsrNodes.addLast(n);
					};
				}
				left = false;
				right = false;
			}
			currentPath = next(currentPath);
		}
		Task4Info result = new Task4Info();
		if (bestLsrNodes.isEmpty()){
			return result;
		}else{
			int[] positions = getLeafSuffixes(bestLsrNodes.getLast());
			int which = -1;
			if (positions[0] > s1Length){
				positions[0]-= (s1Length + 1);
				which = 0;
			}else if (positions[1] > s1Length){
				positions[1]-= (s1Length + 1);
				which = 1;
			}
			if (which == 0){
				result.setPos1(positions[0]);
				result.setPos2(positions[1]);
			}else if (which == 1){
				result.setPos1(positions[1]);
				result.setPos2(positions[0]);
			}
			result.setLen(getLength(bestLsrNodes));
			return result;
		}
	}
		private LinkedList<SuffixTreeNode> getImmidiateLeafDescs(SuffixTreeNode x){
			LinkedList<SuffixTreeNode> leaves = new LinkedList<SuffixTreeNode>();
			if (x.getChild() == null){
				return null;
			}else{
				x = x.getChild();
				while (x != null){
					if (x.getChild() == null){
						leaves.add(x);
					}
					x = x.getSibling();
				}
				return leaves;
			}
		}
}
