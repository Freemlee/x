package SuffixTreePackage;

/**
 * Class for representing information to be returned by the
 * searchSuffixTree method (Task 1).
 * David Manlove, Jan 03.  Modified by David Manlove, Jan 09.
 */

public class Task1Info {

	/** Starting position of a matching occurrence.
	 * (-1 if no such occurrence, otherwise >=0)
	 */
	private int pos;

	/** Suffix tree node corresponding to the occurrence (you may find this useful). */
	private SuffixTreeNode matchNode;

	/**
	 * Default constructor.
	 */
	public Task1Info()
	{
		pos = -1;
		matchNode = null;
	}

	/**
	 * Constructor with parameters.
	 * 
	 * @param p the starting position of a matching occurrence
	 * @param node the suffix tree node corresponding to the occurrence
	 */
	public Task1Info(int p, SuffixTreeNode node)
	{
		pos = p;
		matchNode = node;
	}

	/**
	 * Gets the starting position of a matching occurrence.
	 * 
	 * @return the starting position of a matching occurrence
	 */
	public int getPos() { return pos; }

	/**
	 * Sets the starting position of a matching occurrence.
	 * 
	 * @param p the new starting position of a matching occurrence
	 */
	public void setPos(int p) { pos = p; }

	/**
	 * Gets the suffix tree node corresponding to the occurrence.
	 * 
	 * @return the suffix tree node corresponding to the occurrence
	 */
	public SuffixTreeNode getMatchNode() { return matchNode; }

	/**
	 * Sets the suffix tree node corresponding to the occurrence.
	 * 
	 * @param node the new suffix tree node corresponding to the occurrence
	 */
	public void setMatchNode(SuffixTreeNode node) { matchNode = node; }

}