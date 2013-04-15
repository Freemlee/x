import java.awt.AWTException;
import java.awt.Robot;
import java.awt.event.KeyEvent;

public class ScreenMoveThread extends Thread{
	private Robot robot = null;
	public ScreenMoveThread() throws AWTException{
		robot = new Robot();
	}
	public void run(){
		if (robot != null){
			System.out.println("Handling Screen Change");
			robot.keyPress(KeyEvent.VK_ALT);
			robot.keyPress(KeyEvent.VK_TAB);
			robot.keyRelease(KeyEvent.VK_TAB);
			robot.keyPress(KeyEvent.VK_TAB);
			robot.keyRelease(KeyEvent.VK_TAB);
			
			robot.keyRelease(KeyEvent.VK_ALT);
		}
	}
}
