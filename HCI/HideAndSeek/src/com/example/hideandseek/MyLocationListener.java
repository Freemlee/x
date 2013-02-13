package com.example.hideandseek;
import android.content.Context;
import android.location.Location;
import android.location.LocationListener;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.ToneGenerator;
import android.os.Bundle;
import android.os.Vibrator;
import android.os.Bundle;
import android.widget.Toast;
import android.app.Activity;
import android.content.Context;

public class MyLocationListener implements LocationListener{
	Vibrator phoneVibrator;
	Location dummyLocation;
	MainActivity ma;
	float dist;
	int updatecount;

	public MyLocationListener(Vibrator v, Location loc, MainActivity mac){
		phoneVibrator = v;
		dummyLocation = loc;
		ma = mac;
		Toast.makeText(ma,"MyLocationListener Initialised",Toast.LENGTH_SHORT).show();
		updatecount = 0;
	}
	/*
	Thread tonethread = new Thread(new Runnable(){
		public void run() {
		try {
			MediaPlayer player = MediaPlayer.create(ma,R.raw.notification_beep);
			for (int i = 0; i<15; i++){
				player.start();
				Thread.sleep((long) (1/dist * 100));
			}
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		}}); */
	
	public void onLocationChanged(Location location) {
		Toast.makeText(ma, "This is update " + updatecount , Toast.LENGTH_LONG);
		updatecount++;
		//ma.mainText.setText("Calculating the bearing to your hider...");
		float bearingOfDevice = location.bearingTo(dummyLocation);
		if (bearingOfDevice < 0)
			bearingOfDevice = 360 + bearingOfDevice;
		float currentBearing = location.getBearing();
		//ma.mainText.setText("Calculating the distance to your hider...");
		dist = location.distanceTo(dummyLocation);
		//tonethread.start();
		MediaPlayer player = MediaPlayer.create(ma,R.raw.notification_beep);
		for (int i = 0; i<15; i++){
			player.start();
			try {
				Thread.sleep((long) ((1/dist) * 100));
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		
		//phoneVibrator.vibrate((long) myBearing * 15);
		Toast.makeText(ma,"Bearing of Device: " + bearingOfDevice +"\nCurrent Bearing: " + currentBearing + "\nDistance: " + dist,Toast.LENGTH_LONG).show();
	}
	
	public void alertMessage(String message){
		Toast.makeText(ma,message,Toast.LENGTH_LONG).show();
	}

	public void onProviderDisabled(String provider) {
		ma.mainText.setText("Oh dear oh dear, it looks like your GPS is switched off. You'll have to turn it back on if you want to keep playing");
		
	}

	public void onProviderEnabled(String provider) {
		ma.mainText.setText("Yey, your GPS is back up.");
		
	}

	public void onStatusChanged(String provider, int status, Bundle extras) {
		// TODO Auto-generated method stub
		
	}

}
