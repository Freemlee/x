package com.example.studentreminder;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.os.Vibrator;
import android.util.Log;
import android.widget.TextView;

public class ReminderAlertActivity extends Activity{
	
	public String message;
	public TextView messageTextView;
	@Override
	protected void onCreate(Bundle savedInstance){
		super.onCreate(savedInstance);
		setContentView(R.layout.activity_reminder_pop);
		messageTextView = (TextView) findViewById(R.id.reminderList);
		String message = "";
		if (!getIntent().getExtras().isEmpty()){
			if (getIntent().getExtras().containsKey("com.example.studentreminder.message"))
				message = getIntent().getExtras().getString("com.example.studentreminder.message");
		}
		messageTextView.setText(message);
		Vibrator v = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
		v.vibrate(4000);
	}
	
	@Override
	protected void onStart(){
		super.onStart();
		message = getIntent().getExtras().getString("com.example.studentreminder.message");
		messageTextView.setText(message);
		Log.d("Alert", "Message should be " + message);
	}
	
	@Override
	protected void onResume(){
		super.onResume();
		message = getIntent().getExtras().getString("com.example.studentreminder.message");
		messageTextView.setText(message);
		Log.d("Alert", "Message should be " + message);
	}
	
	/*
	@Override 
	protected void onPause(){
		super.onPause();
		onStop();
	}
	*/
}
