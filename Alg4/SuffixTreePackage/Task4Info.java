package SuffixTreePackage;

/**
 * Class for representing information to be returned by the
 * traverseForLcs method (Task 4).
 * David Manlove, Jan 03.  Modified by David Manlove, Jan 09.
 */

public class Task4Info {

	/** The length of an LCS. */
	private int len;

	/** The starting position of an LCS in s1. */
	private int pos1;

	/** The starting position of an LCS in s2. */
	private int pos2; 

	/** You may also find the following fields useful */
	private boolean string1Leaf;
	private boolean string2Leaf;

	/**
	 * Default constructor.
	 */
	public Task4Info()
	{
		len = 0;
		pos1 = 0;
		pos2 = 0;
		string1Leaf = false;
		string2Leaf = false;
	}

	/**
	 * Constructor with parameters.
	 * 
	 * @param l the length of an LCS
	 * @param p1 the starting position of an LCS in s1
	 * @param p2 the starting position of an LCS in s2
	 * @param s1 the value of string1Leaf
	 * @param s2 the value of string2Leaf
	 */
	public Task4Info(int l, int p1, int p2, boolean s1, boolean s2, int d1, int d2)
	{
		len = l;
		pos1 = p1;
		pos2 = p2;
		string1Leaf = s1;
		string2Leaf = s2;
	}

	/**
	 * Gets the length of an LCS.
	 * 
	 * @return the length of an LCS
	 */
	public int getLen() { return len; }

	/**
	 * Sets the length of an LCS.
	 * 
	 * @param l the new length of an LCS
	 */
	public void setLen(int l) { len = l; }

	/**
	 * Gets the starting position of an LCS in s1.
	 * 
	 * @return the starting position of an LCS in s1
	 */
	public int getPos1() { return pos1; }

	/**
	 * Sets the starting position of an LCS in s1.
	 * 
	 * @param p1 the new starting position of an LCS in s1
	 */
	public void setPos1(int p1) { pos1 = p1; }

	/**
	 * Gets the starting position of an LCS in s2.
	 * 
	 * @return the starting position of an LCS in s2
	 */
	public int getPos2() { return pos2; }

	/**
	 * Sets the starting position of an LCS in s2.
	 * 
	 * @param p2 the new starting position of an LCS in s2
	 */
	public void setPos2(int p2) { pos2 = p2; }

	/**
	 * Gets the value of string1Leaf.
	 * 
	 * @return the value of string1Leaf
	 */
	public boolean getString1Leaf() { return string1Leaf; }

	/**
	 * Sets the value of string1Leaf.
	 * 
	 * @param s1 the new value of string1Leaf
	 */
	public void setString1Leaf(boolean s1) { string1Leaf = s1; }

	/**
	 * Gets the value of string2Leaf.
	 * 
	 * @return the value of string2Leaf
	 */
	public boolean getString2Leaf() { return string2Leaf; }

	/**
	 * Sets the value of string2Leaf.
	 * 
	 * @param s2 the new value of string2Leaf2
	 */
	public void setString2Leaf(boolean s2) { string2Leaf = s2; }
}
