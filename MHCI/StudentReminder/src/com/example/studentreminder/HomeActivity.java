package com.example.studentreminder;

import android.os.Bundle;
import android.app.Activity;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Intent;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageButton;

public class HomeActivity extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_home);
        ImageButton goals = (ImageButton) findViewById(R.id.goalIntentButton);
        ImageButton reminders = (ImageButton)findViewById(R.id.reminderIntentButton);
        goals.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(View arg0) {
				Intent goalIntent = new Intent(HomeActivity.this,GoalActivity.class);
				startActivity(goalIntent);
			}
        });
        reminders.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(View arg0) {
				Intent reminderIntent = new Intent(HomeActivity.this, ReminderActivity.class);
				startActivity(reminderIntent);
			}
        });
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.activity_home, menu);
        return true;
    }
    
}
