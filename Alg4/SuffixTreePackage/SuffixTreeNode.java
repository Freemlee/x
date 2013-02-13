package SuffixTreePackage;

/**
 * Class for representing a suffix tree node.
 * 
 * Each node of the suffix tree contains a field (child) indicating 
 * the head of the list of its children, and a field (sibling) to its
 * successor in the list of children of its parent.
 *  
 * Each node (apart from the root) contains a pair of indices into the
 * string represented, these indicating an occurrence as a substring of
 * the string that labels the tree edge directly above that node.
 *
 * Each node stores two booleans indicating whether it has two 
 * descendant leaf nodes whose suffix numbers start in different strings
 * (valid for computing Longest Common Substrings only).
 *
 * Each leaf node contains the number of the suffix that it represents -
 * this number is -1 for branch nodes.
 *
 * Ela Hunt, Jan 01.  Modified by David Manlove, Jan 02 and Jan 09.
 */

public class SuffixTreeNode {

	/** Head of list of children */
	private SuffixTreeNode child;

	/** Sibling node */
	private SuffixTreeNode sibling;

	/** i such that full edge label representing tree edge directly above this node is [i..j] */
	private int leftLabel; 

	/** j such that full edge label representing tree edge directly above this node is [i..j] */
	private int rightLabel; 

	/** Suffix number represented by this node 
	 *  >=0 for leaf nodes; -1 for branch nodes
	 */
	private int suffix;

        /** Boolean indicating whether there is a descendant leaf node whose suffix number starts 
         *  in the first string (valid for Longest Common Substring computations only)
         */
        private boolean leafNodeString1;

        /** Boolean indicating whether there is a descendant leaf node whose suffix number starts 
         *  in the second string (valid for Longest Common Substring computations only)
         */
        private boolean leafNodeString2;

        /** Suffix number of a descendant leaf node whose suffix number starts in the first
         *  string, if any (valid for Longest Common Substring computations only)
         */
        private int leafNodeNumString1;

        /** Suffix number of a descendant leaf node whose suffix number starts in the second
         *  string, if any (valid for Longest Common Substring computations only)
         */
        private int leafNodeNumString2;

	/**
	 * Instantiates a new suffix tree node.
	 * 
	 * @param c the head of the list of children
	 * @param sib the sibling node
	 * @param left the leftmost extent of the full edge label
	 * @param right the rightmost extent of the full edge label
	 * @param suffix the suffix number
	 */
	public SuffixTreeNode (SuffixTreeNode c, SuffixTreeNode sib, 
			               int left, int right, int suffNum)
	{
		child = c;
		sibling = sib;
		leftLabel = left;
		rightLabel = right;
		suffix = suffNum;
                leafNodeString1 = false;
                leafNodeString2 = false;
                leafNodeNumString1 = 0;
                leafNodeNumString2 = 0;
	}

	/**
	 * Adds a new child to the list of children
	 * 
	 * @param left the leftmost extent of the full edge label
	 * @param right the rightmost extent of the full edge label
	 * @param suffix the suffix number
	 */
	public void addChild(int left, int right, int suffix) {
		SuffixTreeNode temp = new SuffixTreeNode(null,getChild(),left,right,suffix);
		setChild(temp);
	}

	/**
	 * Gets the head of a linked list of children.
	 * 
	 * @return the head of a linked list of children
	 */
	public SuffixTreeNode getChild() { return child; }

	/**
	 * Sets the head of a linked list of children.
	 * 
	 * @param c the head of a linked list of children
	 */
	public void setChild(SuffixTreeNode c) { child = c; }

	/**
	 * Gets the sibling.
	 * 
	 * @return the sibling
	 */
	public SuffixTreeNode getSibling() { return sibling; }

	/**
	 * Sets the sibling.
	 * 
	 * @param s the new sibling
	 */
	public void setSibling(SuffixTreeNode s) { sibling = s; }

	/**
	 * Gets the leftmost extent of the full edge label.
	 * 
	 * @return the leftmost extent of the full edge label
	 */
	public int getLeftLabel() { return leftLabel; }

	/**
	 * Sets the leftmost extent of the full edge label.
	 * 
	 * @param l the new leftmost extent of the full edge label  
	 */
	public void setLeftLabel(int l) { leftLabel = l; }

	/**
	 * Gets the rightmost extent of the full edge label.
	 * 
	 * @return the rightmost extent of the full edge label
	 */
	public int getRightLabel() { return rightLabel; }

	/**
	 * Sets the rightmost extent of the full edge label.
	 * 
	 * @param r the new rightmost extent of the full edge label  
	 */
	public void setRightLabel(int r) { rightLabel = r; }

	/**
	 * Gets the suffix number.
	 * 
	 * @return the suffix number
	 */
	public int getSuffix() { return suffix; }

	/**
	 * Sets the suffix number.
	 * 
	 * @param s the new suffix number
	 */
	public void setSuffix(int s) { suffix = s; }

	/**
	 * Gets the value of leafNodeString1.
	 * 
	 * @return the value of leafNodeString1
	 */
	public boolean getLeafNodeString1() { return leafNodeString1; }

	/**
	 * Sets the value of leafNodeString1.
	 * 
	 * @param b the boolean value
	 */
	public void setLeafNodeString1(boolean b) { leafNodeString1 = b; }

	/**
	 * Gets the value of leafNodeString2.
	 * 
	 * @return the value of leafNodeString2
	 */
	public boolean getLeafNodeString2() { return leafNodeString2; }

	/**
	 * Sets the value of leafNodeString2.
	 * 
	 * @param b the boolean value
	 */
	public void setLeafNodeString2(boolean b) { leafNodeString2 = b; }

	/**
	 * Gets the value of leafNodeNumString1.
	 * 
	 * @return the value of leafNodeString1
	 */
	public int getLeafNodeNumString1() { return leafNodeNumString1; }

	/**
	 * Sets the value of leafNodeNumString1.
	 * 
	 * @param i the boolean value
	 */
	public void setLeafNodeNumString1(int i) { leafNodeNumString1 = i; }

	/**
	 * Gets the value of leafNodeNumString2.
	 * 
	 * @return the value of leafNodeString2
	 */
	public int getLeafNodeNumString2() { return leafNodeNumString2; }

	/**
	 * Sets the value of leafNodeNumString2.
	 * 
	 * @param i the boolean value
	 */
	public void setLeafNodeNumString2(int i) { leafNodeNumString2 = i; }
}
