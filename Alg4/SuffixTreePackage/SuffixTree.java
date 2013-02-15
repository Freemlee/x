package SuffixTreePackage;

/**
 * Class for construction and manipulation of suffix trees based on a list
 * of children at each node.
 * 
 * Includes naive O(n^2) suffix tree construction algorithm based on
 * repeated insertion of suffixes and node-splitting.
 * 
 * Modifies Ada implementation of naive suffix tree construction algorithm
 * due to Rob Irving, Jan 00.
 * 
 * Also incorporates Java code for naive suffix tree construction algorithm
 * due to Ela Hunt, Jan 01.
 * 
 * Modifications by David Manlove, Apr 02, Jan 03,Jan 07 and Jan 09.
 */

public class SuffixTree {

	/** Root node of the suffix tree. */
	private SuffixTreeNode root;

	/** String (byte array) corresponding to suffix tree. */
	private byte [] s;
	
	/** Length of string corresponding to suffix tree (without termination character). */
	private int stringLen;

	/**
	 * Builds the suffix tree for a given string.
	 * 
	 * @param sInput the string whose suffix tree is to be built
	 * - assumes that '$' does not occur as a character anywhere in sInput
	 * - assumes that characters of sInput occupy positions 0 onwards
	 */
	public SuffixTree (byte [] sInput) {
		root = new SuffixTreeNode(null, null, 0, 0, -1);  // create root node of suffix tree;
		stringLen = sInput.length;
		s = new byte[stringLen + 1]; // create longer byte array ready for termination character
		System.arraycopy(sInput, 0, s, 0, stringLen);
		s[stringLen] = (byte) '$';   // append termination character to original string
		buildSuffixTree();           // build the suffix tree
	}

	/**
	 * Builds a generalised suffix tree for two given strings.
	 * 
	 * @param sInput1 the first string
	 * @param sInput2 the second string
	 * - assumes that '$' and '#' do not occur as a character anywhere in sInput1 or sInput2
	 * - assumes that characters of sInput1 and sInput2 occupy positions 0 onwards
	 */
	public SuffixTree (byte[] sInput1, byte[] sInput2) {
         // to be completed!
		root = new SuffixTreeNode(null, null, 0, 0, -1);
		s = new byte[sInput1.length + sInput2.length + 2];
		System.arraycopy(sInput1, 0, s, 0, sInput1.length);
		s[sInput1.length] = (byte) '#';
		System.arraycopy(sInput2, 0, s, sInput1.length + 1, sInput2.length);
		s[sInput1.length + sInput2.length + 1] = (byte) '$';
		//try changing to 2
		stringLen = sInput1.length + sInput2.length + 1;
		System.out.println(sInput1.length+sInput2.length+2 + "\t" + s.length);
		buildSuffixTree();
	}

	/**
	 * Builds the suffix tree.
	 */
	private void buildSuffixTree() {
		try {		
			for (int i=0; i<= stringLen; i++) {
				// for large files, the following line may be useful for
				// indicating the progress of the suffix tree construction
				//if (i % 10000==0) System.out.println(i);

				// raise an exception if the text file contained a '$'
				if (s[i] == (byte) '$' && i < stringLen)
					throw new Exception();
				else
					insert(i);  // insert suffix number i of z into tree
			}
		} 
		catch (Exception e) {
			System.out.println("Text file contains a $ character!");
			System.exit(-1);
		}
	}

	/**
	 * Given node nodeIn of suffix tree and character ch, search nodeIn, 
	 * plus all sibling nodes of nodeIn, looking for a node whose left 
	 * label x satisfies ch == s[x].
	 * - Assumes that characters of s occupy positions 0 onwards
	 * 
	 * @param nodeIn a node of the suffix tree
	 * @param ch the character to match
	 * 
	 * @return the matching suffix tree node (null if none exists)
	 */
	public SuffixTreeNode searchList (SuffixTreeNode nodeIn, byte ch) { 

		SuffixTreeNode next = nodeIn;
		SuffixTreeNode nodeOut = null;

		while (next != null) {
			if (next.getLeftLabel() < stringLen && s[next.getLeftLabel()] == ch)
			{
				nodeOut = next;
				next = null;
			}
			else
				next = next.getSibling();
		}
		return nodeOut;  // return matching node if successful, or null otherwise
	}

	/**
	 * Inserts suffix number i of s into suffix tree.
     * - assumes that characters of s occupy positions 0 onwards
	 * 
	 * @param i the suffix number of s to insert
	 */
	private void insert(int i) {

		int pos, j, k;
		SuffixTreeNode current, next;
		pos = i;  // position in s
		current = root;

		while (true) {
			// search for child of current with left label x such that s[x]==s[pos]
			next = searchList(current.getChild(), s[pos]);

			if (next == null) {
				// current node has no such child, so add new one corresponding to
				// positions pos onwards of s
				current.addChild(pos, stringLen, i);
				break;
			}
			else {
				// try to match s[node.getLeftLabel()+1..node.getRightLabel()] with 
				// segment of s starting at position pos+1
				j = next.getLeftLabel() + 1;
				k = pos + 1;

				while (j <= next.getRightLabel()) {
					if (s[j] == s[k]) {
						j++;
						k++;
					}
					else
						break;
				}
				if (j > next.getRightLabel()) {
					// succeeded in matching whole segment, so go further down tree
					pos = k;
					current = next;
				}
				else {
					/* succeeded in matching s[next.getLeftLabel()..j-1] with
					 * s[pos..k-1].  Split the node next so that its right label is
					 * now j-1.  Create two children of next: (1) corresponding to
					 * suffix i, with left label k and right label s.length-1,
					 * and (2) with left label j and right label next.getRightLabel(),
					 * whose children are those of next (if any), and whose suffix 
					 * number is equal to that of next. */

					SuffixTreeNode n1 = new SuffixTreeNode(null, null, k, stringLen, i);
					SuffixTreeNode n2 = new SuffixTreeNode(next.getChild(), n1, 
							                               j, next.getRightLabel(), next.getSuffix());
                    // now update next's right label, list of children and suffix number
					next.setRightLabel(j-1);
					next.setChild(n2);
					next.setSuffix(-1); // next is now an internal node
					break;
				}
			}
		}
	}
	
	/**
	 * Gets the root node.
	 * 
	 * @return the root node
	 */
	public SuffixTreeNode getRoot() { return root; }

	/**
	 * Sets the root node.
	 * 
	 * @param node the new root node
	 */
	public void setRoot(SuffixTreeNode node) { root = node; }
	
	/**
	 * Gets the string represented by the suffix tree.
	 * 
	 * @return the string represented by the suffix tree
	 */
	public byte[] getString() { return s; }

	/**
	 * Sets the string represented by the suffix tree.
	 * 
	 * @param sInput the new string represented by the suffix tree
	 */
	public void setString(byte [] sInput) { s = sInput; }
	
	/**
	 * Gets the length of the string represented by the suffix tree.
	 * 
	 * @return the length of the string represented by the suffix tree
	 */
	public int getStringLen() { return stringLen; }

	/**
	 * Sets the length of the string represented by the suffix tree.
	 * 
	 * @param len the new length of the string represented by the suffix tree
	 */
	public void setStringLen(int len) { stringLen = len; }
}
