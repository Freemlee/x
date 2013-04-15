package com.example.copypastesync;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;
import android.os.Bundle;
import android.app.Activity;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.hardware.Sensor;
import android.hardware.SensorManager;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

public class MainActivity extends Activity {
	
	public static final int SHAKE_REQUEST_CODE = '0';
	public static final int CLIPBOARD_REQUEST_CODE = '1';	
	
	public boolean threadHasConnected = false;
	public Socket globalConnection = null;
	ObjectOutputStream oos = null;
	Button sendButton = null;
	private boolean validIp = false;
	private boolean validPort = false;
	int pNum = -1;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
		sendButton = (Button)findViewById(R.id.connectButton);
		sendButton.setEnabled(false);
		final EditText ipText = ((EditText)findViewById(R.id.ipHolder));
		final EditText portText = ((EditText)findViewById(R.id.portHolder));
		portText.setOnKeyListener(new EditText.OnKeyListener(){
			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				Log.d("press","press");
				if (portText.getText().toString().isEmpty()){
					pNum = -1;
					validPort = false;
					buttonValidation();
				}else{
					try{
						pNum = Integer.parseInt(portText.getText().toString());
						Log.d("pNum",(pNum >= 1 && pNum <= 65535)+"");
						if (pNum >= 1 && pNum <= 65535){
							validPort = true;
							buttonValidation();
						}
					}catch(NumberFormatException e){
						validPort = false;
						pNum = -1;
						buttonValidation();
					}
				}
				return false;
			}});
		ipText.setOnKeyListener(new EditText.OnKeyListener(){
			@Override
			public boolean onKey(View arg0, int arg1, KeyEvent arg2) {
				if (ipText.getText().toString().matches("^([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\." +
						"([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\." +
						"([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\." +
						"([01]?\\d\\d?|2[0-4]\\d|25[0-5])$")){
					validIp=true;
					buttonValidation();
				}else{
					validIp=false;
					buttonValidation();
				}
				return false;
			}});
		sendButton.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(View button) {
				if (threadHasConnected == false || globalConnection == null){
					Connect conn = new Connect();
					conn.execute();
					sendButton.setText("Connecting...");
					sendButton.setActivated(false);
				}else{
					try {
						String host = globalConnection.getInetAddress().getHostAddress();
						globalConnection.close();
						Toast.makeText(MainActivity.this, "Disconnected from " + host, Toast.LENGTH_LONG).show();
						threadHasConnected = false;
						globalConnection = null;
						oos=null;
					} catch (IOException e) {
						e.printStackTrace();
					}finally{
						sendButton.setText("Connect");
					}
				}
			}
		});
		
		final ClipboardManager cm = (ClipboardManager)getSystemService(this.CLIPBOARD_SERVICE);
		cm.addPrimaryClipChangedListener(new ClipboardManager.OnPrimaryClipChangedListener() {
			@Override
			public void onPrimaryClipChanged() {
				if (threadHasConnected && (globalConnection != null)){
					SendDataThread sdt;
					try {
						sdt = new SendDataThread(globalConnection, CLIPBOARD_REQUEST_CODE, new JSONObject().accumulate("text", cm.getPrimaryClip().getItemAt(0).getText().toString()));
						sdt.start();
					} catch (JSONException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					//SendNewClipboard snc = new SendNewClipboard(globalConnection, cm.getPrimaryClip().getItemAt(0).getText().toString());
					//snc.start();
				}else{
					//Error message displayed due to not correctly disactivating global connection
					//Toast.makeText(MainActivity.this, "Network Error With Copy", Toast.LENGTH_LONG).show();
				}
			}
		});
		
		SensorManager sm = (SensorManager)getSystemService(SENSOR_SERVICE);
		sm.registerListener(new ShakeListener(new ShakeListener.OnShakeListener() {
			int flipCount = 0;
			@Override
			public void onShake(float x, float y, float z) {
			}
			@Override
			public void onRotation() {
				if (threadHasConnected && (globalConnection != null)){
					SendDataThread sdt = new SendDataThread(globalConnection,SHAKE_REQUEST_CODE,null);
					sdt.start();
				}else{
					//Toast.makeText(MainActivity.this, "Network Error With Shake", Toast.LENGTH_LONG).show();
				}
			}
		}),sm.getDefaultSensor(Sensor.TYPE_ACCELEROMETER),SensorManager.SENSOR_DELAY_NORMAL);
		
	}
	
	public class SendDataThread extends Thread{
		Socket socket = null;
		int code;
		JSONObject data;
		public SendDataThread(Socket s, int c, JSONObject d){
			socket = s;
			code = c;
			data = d;
		}
		public void run(){
			try {
				if (oos == null){
					oos = new ObjectOutputStream(socket.getOutputStream());
				}
				oos.flush();
				oos.writeObject(MakeData.makeData(code, data));
			} catch (IOException e) {
				e.printStackTrace();
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
	}
	
	public class SendShakeReq extends Thread{
		Socket socket = null;
		public SendShakeReq(Socket s){
			socket = s;
		}
		public void run(){
			try {
				if (oos == null){
					oos = new ObjectOutputStream(socket.getOutputStream());
				}
				oos.flush();
				oos.writeObject(SHAKE_REQUEST_CODE+"");
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	public class SendNewClipboard extends Thread{
		Socket socket = null;
		String data;
		public SendNewClipboard(Socket s, String d){
			socket = s;
			data = d;
		}				
		public void run(){
			try {
				if (oos == null){
					oos = new ObjectOutputStream(socket.getOutputStream());
				}
				oos.flush();
				oos.writeObject(CLIPBOARD_REQUEST_CODE+data);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	public class Connect extends AsyncTask<Void,Void,Socket>{
		@Override
		protected Socket doInBackground(Void... arg0) {
			String ipAddressS = ((TextView)findViewById(R.id.ipHolder)).getText().toString();
			String portNumberS = ((TextView)findViewById(R.id.portHolder)).getText().toString();
			int portNo;
			InetAddress ipAddress;
			try{
				portNo = Integer.parseInt(portNumberS);
				ipAddress = InetAddress.getByName(ipAddressS);
				if (ipAddress != null){
					//Body of program
					Socket connection = new Socket();
					connection.connect(new InetSocketAddress(ipAddress,portNo));
					return connection;
				}else{
					//Handle for bad IP
					return null;
				}
				
			}catch(NumberFormatException e){
				e.printStackTrace();
				return null;
			} catch (UnknownHostException e) {
				e.printStackTrace();
				return null;
			} catch (IOException e) {
				e.printStackTrace();
				return null;
			}
		}
		
		protected void onPostExecute(Socket theSocket){
			if (theSocket != null){
				threadHasConnected = true;
				globalConnection = theSocket;
				Toast.makeText(MainActivity.this, "Connection Succeeded", Toast.LENGTH_LONG).show();
				sendButton.setText("Disconnect");
				sendButton.setActivated(true);
			}else{
				threadHasConnected = false;
				Toast.makeText(MainActivity.this, "Connection Failed", Toast.LENGTH_LONG).show();
				sendButton.setText("Connect");
				sendButton.setActivated(true);
			}
		}
	}
	
	public boolean buttonValidation(){
		sendButton.setEnabled(validIp&&validPort);
		return (validIp&&validPort);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.activity_main, menu);
		return true;
	}

}
