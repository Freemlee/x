package SuffixTreePackage;

/**
 * Class for representing information to be returned by the
 * traverseForLrs method (Task 3).
 * David Manlove, Jan 03.  Modified by David Manlove, Jan 09.
 */

public class Task3Info {

	/** The length of an LRS. */
	private int len;

	/** The starting position of an LRS in s1. */
	private int pos1;

	/**The starting position of an LRS in s2. */
	private int pos2; 

	/**
	 * Default constructor.
	 */
	public Task3Info()
	{
		len = 0;
		pos1 = 0;
		pos2 = 0;
	}

	/**
	 * Constructor with parameters.
	 * 
	 * @param l the length of an LRS
	 * @param p1 the starting position of an LRS in s1
	 * @param p2 the starting position of an LRS in s2
	 */
	public Task3Info(int l, int p1, int p2)
	{
		len = l;
		pos1 = p1;
		pos2 = p2;
	}

	/**
	 * Gets the length of an LRS.
	 * 
	 * @return the length of an LRS
	 */
	public int getLen() { return len; }

	/**
	 * Sets the length of an LRS.
	 * 
	 * @param l the new length of an LRS
	 */
	public void setLen(int l) { len = l; }

	/**
	 * Gets the starting position of an LRS in s1.
	 * 
	 * @return the starting position of an LRS in s1
	 */
	public int getPos1() { return pos1; }

	/**
	 * Sets the starting position of an LRS in s1.
	 * 
	 * @param p1 the new starting position of an LRS in s1
	 */
	public void setPos1(int p1) { pos1 = p1; }

	/**
	 * Gets the starting position of an LRS in s2.
	 * 
	 * @return the starting position of an LRS in s2
	 */
	public int getPos2() { return pos2; }

	/**
	 * Sets the starting position of an LRS in s2.
	 * 
	 * @param p2 the new starting position of an LRS in s2
	 */
	public void setPos2(int p2) { pos2 = p2; }
}