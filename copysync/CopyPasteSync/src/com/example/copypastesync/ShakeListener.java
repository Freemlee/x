package com.example.copypastesync;

import android.app.Service;
import android.content.Intent;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.os.IBinder;
import android.widget.Toast;

 public class ShakeListener implements SensorEventListener {
	 
	private OnShakeListener mOnShakeListener; 
	private long deltaTime;
	private long currTime;
	public ShakeListener(OnShakeListener osl){
		mOnShakeListener = osl;
		deltaTime = System.currentTimeMillis();
	}

	public interface OnShakeListener{
		public void onRotation();
		public void onShake(float x, float y, float z);
	} 
	 
	@Override
	public void onAccuracyChanged(Sensor arg0, int arg1) {
		// TODO Auto-generated method stub
	}

	@Override
	public void onSensorChanged(SensorEvent event) {
		currTime = System.currentTimeMillis();
		if ((currTime - deltaTime) > 700){
			float x = event.values[0]; 
			float y = event.values[1]; 
			float z = event.values[2]; 
			mOnShakeListener.onShake(x, y, z);
			if (x > 5 || x < -5){
				mOnShakeListener.onRotation();
			}
			deltaTime = currTime;
		}
	}

 }