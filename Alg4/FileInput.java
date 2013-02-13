import java.io.*;

/**
 * Class for handling input from a file.
 * 
 * David Manlove, Jan 02. Modified Jan 09.
 */
public class FileInput {

	/** The name of the input file. */
	String fileName;

	/**
	 * Instantiates a new FileInput object.
	 * 
	 * @param s the filename
	 */
	public FileInput (String s) {
		fileName = s;
	}

	/**
	 * Read bytes of data from the file.
	 * 
	 * @return the data in the file represented as an array of bytes
	 */
	public byte [] readFile () {

		byte [] s = null; // store data from input file as an array of bytes,
		                  // since a variable of type char uses 2 bytes
		FileInputStream inputStream = null;
		try {
			try {
				// open input file, read data
				inputStream = new FileInputStream(fileName);
				int len = inputStream.available();
				s = new byte[len];
				int index = 0;
				byte next = (byte) inputStream.read();   // read a byte, represented by an int
				while (next != -1) {                     // -1 represents end of file
					s[index] = next;                     // store byte in array
					next = (byte) inputStream.read();    // read the next byte
					index++;
				}
			}
			finally {
				// executed whether or not an exception is raised above
				// close file assuming it was successfully opened
				if (inputStream  != null) inputStream.close();
			}
		}
		catch (IOException e) {
			System.out.println(e); 
			System.exit(-1);
		}
		return s;
	}
}
