

import java.awt.AWTException;
import java.awt.HeadlessException;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.KeyEvent;
import java.net.*;
import java.util.Scanner;
import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;

import org.json.JSONException;
import org.json.JSONObject;

public class ClipboardListener {
	static Clipboard cp = null;
	static Scanner userInput = null;
	
	public static void main(String[] args) throws UnsupportedFlavorException, IOException, InterruptedException, ClassNotFoundException, AWTException{
		if (cp == null)
			cp = Toolkit.getDefaultToolkit().getSystemClipboard();
		if (userInput == null)
			userInput = new Scanner(System.in);
		ServerSocket ss = null;
		try{
			ss = new ServerSocket(0);
		}catch(IOException e){
			e.printStackTrace();
			System.out.println("Fell down at ServerSocket");
		}
		Socket socket = null;
		if (ss!=null){
			try{
				System.out.println("Waiting for connection to " + InetAddress.getLocalHost().getHostAddress() + " at port " + ss.getLocalPort());
				socket = ss.accept();	
			}catch(IOException e){
				e.printStackTrace();
				System.out.println("Fell down at Socket");
			}
		}
		System.out.println("Connection Received from " + socket.getInetAddress().getHostAddress());
		InputStream is = socket.getInputStream();
		ObjectInputStream ois = new ObjectInputStream(is);
		//Robot robot = new Robot();
		while(true){
			/*
			try{
				String streamString = (String)ois.readObject();
				if (streamString != null){
					if (streamString.equals("tab")){
						robot.keyPress(KeyEvent.VK_ALT);
						robot.keyPress(KeyEvent.VK_TAB);
						robot.keyRelease(KeyEvent.VK_ALT);
						robot.keyRelease(KeyEvent.VK_TAB);
					}else{
						System.out.println(streamString);
						StringSelection stringSelection = new StringSelection(streamString);
						cp.setContents(stringSelection, stringSelection);
					}
				}
				Thread.sleep(1000);
			}catch(EOFException e){
				System.out.println("Devices Disconnected");
				break;
			}
			*/
			try{
				String streamString = (String)ois.readObject();
				JSONObject jsonData = new JSONObject(streamString);
				String code = jsonData.getString("code");
				System.out.println(code);
				if (code.equalsIgnoreCase("CR")){
					System.out.println("Handle Copy");
					StringSelection stringSelection = new StringSelection(jsonData.getString("cargo"));
					cp.setContents(stringSelection, stringSelection);
				}else if (code.equals("SR")){
					System.out.println("Handle Shake");
					ScreenMoveThread smt = new ScreenMoveThread();
					smt.start();
				}
					
				System.out.println(streamString);
			}catch(EOFException e){
				System.out.println("Devices Disconnected");
				break;
			} catch (JSONException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			Thread.sleep(500);
		}
		ss.close();
		System.out.print("Would you like to try an reconnect? y/n  - ");
		String ans = userInput.nextLine();
		if (ans.equalsIgnoreCase("y")){
			main(null);
		}else{
			System.exit(0);
		}
	}
	
	public void init(){
		
	}
}