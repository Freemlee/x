package SuffixTreePackage;

import java.util.*;

/**
 * Class for representing information to be returned by the
 * allOccurrences method (Task 2).
 * David Manlove, Jan 03.  Modified by David Manlove, Jan 09.
 */

public class Task2Info {

	/** Linked list of starting positions. */
	private LinkedList<Integer> positions;

	/**
	 * Default constructor.
	 */
	public Task2Info()
	{
		positions = new LinkedList<Integer>();
	}

	/**
	 * Constructor with parameter.
	 * 
	 * @param p the list of starting positions
	 */
	public Task2Info(int p)
	{
		positions = new LinkedList<Integer>();
		positions.addLast(new Integer(p));
	}

	/**
	 * Gets the starting positions.
	 * 
	 * @return the starting positions
	 */
	public LinkedList<Integer> getPositions() { return positions; }

	/**
	 * Sets the starting positions.
	 * 
	 * @param list the new positions
	 */
	public void setPositions(LinkedList<Integer> list) { positions = list; }

	/**
	 * Adds an entry to the list of starting positions.
	 * 
	 * @param p the entry to add to the list of starting positions.
	 */
	public void addEntry(int p) { positions.addLast(new Integer(p)); };

	/**
	 * Adds a list of starting positions to the existing list.
	 * 
	 * @param list the list of starting positions to add
	 */
	public void addList(LinkedList<Integer> list) { positions.addAll(list); };
}