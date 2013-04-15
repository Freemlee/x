package com.example.copypastesync;

import android.hardware.Sensor;
import android.hardware.SensorManager;
import android.os.Bundle;
import android.os.Vibrator;
import android.app.Activity;
import android.view.Menu;
import android.widget.TextView;
import android.widget.Toast;

public class FlipActivity extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_flip);
		Vibrator vibrator = (Vibrator) getSystemService(VIBRATOR_SERVICE);
		final TextView tv = (TextView)findViewById(R.id.vals);
		//Add listener here
		SensorManager sm = (SensorManager)getSystemService(SENSOR_SERVICE);
		sm.registerListener(new ShakeListener(new ShakeListener.OnShakeListener() {
			int flipCount = 0;
			@Override
			public void onShake(float x, float y, float z) {
				//tv.setText("x = " + x + " y = " + y + " z = " + z);
			}
			@Override
			public void onRotation() {
				flipCount++;
				tv.setText("Flipping at rate "+ flipCount);
			}
		}),sm.getDefaultSensor(Sensor.TYPE_ACCELEROMETER),200);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.activity_flip, menu);
		return true;
	}

}
