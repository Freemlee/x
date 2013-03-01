package com.example.studentreminder;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import android.app.ActionBar;
import android.app.AlarmManager;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.app.DatePickerDialog;
import android.app.FragmentTransaction;
import android.app.PendingIntent;
import android.app.TimePickerDialog;
import android.content.Intent;
import android.location.Geocoder;
import android.location.Location;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.NavUtils;
import android.text.format.Time;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.TimePicker;

public class ReminderActivity extends FragmentActivity implements
		ActionBar.TabListener {

	/**
	 * The serialization (saved instance state) Bundle key representing the
	 * current tab position.
	 */
	private String message;
	private String time = null;
	private String date = null;
	private boolean dateSet = false;
	private boolean timeSet = false;
	private long alarmDate;
	private static final String STATE_SELECTED_NAVIGATION_ITEM = "selected_navigation_item";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_reminder);

		// Set up the action bar to show tabs.
		final ActionBar actionBar = getActionBar();
		actionBar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);

		// For each of the sections in the app, add a tab to the action bar.
		actionBar.addTab(actionBar.newTab().setText("Time Based")
				.setTabListener(this));
		actionBar.addTab(actionBar.newTab().setText("Location Based")
				.setTabListener(this));
	}

	@Override
	public void onRestoreInstanceState(Bundle savedInstanceState) {
		// Restore the previously serialized current tab position.
		if (savedInstanceState.containsKey(STATE_SELECTED_NAVIGATION_ITEM)) {
			getActionBar().setSelectedNavigationItem(
					savedInstanceState.getInt(STATE_SELECTED_NAVIGATION_ITEM));
		}
	}

	@Override
	public void onSaveInstanceState(Bundle outState) {
		// Serialize the current tab position.
		outState.putInt(STATE_SELECTED_NAVIGATION_ITEM, getActionBar()
				.getSelectedNavigationIndex());
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.activity_reminder, menu);
		return true;
	}

	@Override
	public void onTabSelected(ActionBar.Tab tab,
			FragmentTransaction fragmentTransaction) {
		Fragment fragment;
		if (tab.getPosition() == 0){
			fragment = new TimeFrag();
		}
		else
			fragment = new LocationFrag();
		
		Bundle args = new Bundle();
		getSupportFragmentManager().beginTransaction()
				.replace(R.id.container, fragment).commit();
	}

	@Override
	public void onTabUnselected(ActionBar.Tab tab,
			FragmentTransaction fragmentTransaction) {
	}

	@Override
	public void onTabReselected(ActionBar.Tab tab,
			FragmentTransaction fragmentTransaction) {
	}

	/**
	 * A dummy fragment representing a section of the app, but that simply
	 * displays dummy text.
	 */
	public static class DummySectionFragment extends Fragment {
		/**
		 * The fragment argument representing the section number for this
		 * fragment.
		 */
		public static final String ARG_SECTION_NUMBER = "section_number";

		public DummySectionFragment() {
		}

		@Override
		public View onCreateView(LayoutInflater inflater, ViewGroup container,
				Bundle savedInstanceState) {
			// Create a new TextView and set its text to the fragment's section
			// number argument value.
			TextView textView = new TextView(getActivity());
			textView.setGravity(Gravity.CENTER);
			textView.setText(Integer.toString(getArguments().getInt(
					ARG_SECTION_NUMBER)));
			return textView;
		}
	}
	
	public class LocationFrag extends Fragment{
		public LocationFrag(){}
		private double lon;
		private double lat;
		
		private static final double boyd_orr_long = -4.292479;
		private static final double lib_long = -4.2886;
		private static final double maths_long = -4.291842;
		private static final double sawb_long = -4.291839;
		
		private static final double boyd_orr_lat = 55.873635;
		private static final double lib_lat = 55.873309;
		private static final double maths_lat = 55.873226;
		private static final double sawb_lat = 55.873923;

		@Override
		public View onCreateView(LayoutInflater inflater, ViewGroup container,
	            Bundle savedInstanceState) {
			final Location loc = new Location("");
			final LinearLayout lv = (LinearLayout) inflater.inflate(R.layout.frag_location, container, false);
			
			Button b = (Button) lv.getChildAt(2);
			b.setOnClickListener(new OnClickListener(){
				@Override
				public void onClick(View v){
					int i = ((Spinner)lv.getChildAt(0)).getSelectedItemPosition();
					switch(i){
						case 0:
							lon = boyd_orr_long;
							lat = boyd_orr_lat;
							break;
						case 1:
							lon = lib_long;
							lat = lib_lat;
							break;
						case 2:
							lon = maths_long;
							lat = maths_lat;
							break;
						case 3:
							lon = sawb_long;
							lat = sawb_lat;
							break;
					}
					Log.d("Button","Clicked");
					Log.d("Long should be",lib_long+"");
					Log.d("Lat should be",lib_lat+"");
					Intent locIntent = new Intent(ReminderActivity.this,LocationService.class);
					LocationService ls = new LocationService();
					ls.addAlarm(lon,lat, ((EditText)lv.getChildAt(1)).getText().toString(),ReminderActivity.this);
					startService(locIntent);
				}
			});
			
			return lv;
		}
	}
	
	public class TimeFrag extends Fragment{
		public TimeFrag(){}
		
		public void setAlarm(){
			Log.d("Set alarm","called");
			if (alarmDate < System.currentTimeMillis())
				new AlertDialog.Builder(ReminderActivity.this).setMessage("Please select a date in the future").setPositiveButton("OK", null).show();
			else{
				Log.d("Alarm","Alarm will be called in " + (alarmDate - System.currentTimeMillis()) + " milliseconds");
				AlarmManager sampleAlarm = (AlarmManager) getSystemService(ALARM_SERVICE);
		        Intent showDialogIntent = new Intent(ReminderActivity.this,ReminderAlertActivity.class);
		        Log.d("Message Passed", message);
		        showDialogIntent.removeExtra("com.example.studentreminder.message");
		        showDialogIntent.putExtra("com.example.studentreminder.message", message);
		        PendingIntent pIntent = PendingIntent.getActivity(ReminderActivity.this, 0, showDialogIntent, PendingIntent.FLAG_UPDATE_CURRENT);
		        //Log.d("PIntent Value",pIntent.)
		        sampleAlarm.set(AlarmManager.RTC,alarmDate,pIntent);
			}
		}	
		
		@Override
		public View onCreateView(LayoutInflater inflater, ViewGroup container,
	            Bundle savedInstanceState) {
		// Inflate the layout for this fragment
			final LinearLayout ll = (LinearLayout) inflater.inflate(R.layout.frag_time, container, false);
			ll.getChildAt(0).setOnClickListener(new OnClickListener(){
				@Override
				public void onClick(View v) {
				Calendar c = Calendar.getInstance();
					new TimePickerDialog(ReminderActivity.this,new TimePickerDialog.OnTimeSetListener() {						
						@Override
						public void onTimeSet(TimePicker view, int hourOfDay, int minute) {
							((TextView)ll.getChildAt(0)).setText(hourOfDay + ":" + minute);
							time = hourOfDay + ":" + minute;
							timeSet = true;
						}
					},c.get(Calendar.HOUR_OF_DAY),c.get(Calendar.MINUTE),true).show();
				}
			});
			ll.getChildAt(1).setOnClickListener(new OnClickListener(){
				@Override
				public void onClick(View v) {
					final Calendar c = Calendar.getInstance();
					new DatePickerDialog(ReminderActivity.this,new DatePickerDialog.OnDateSetListener() {
						@Override
						public void onDateSet(DatePicker arg0, int arg1, int arg2, int arg3) {
							((TextView)ll.getChildAt(1)).setText(arg3 + "/" + (arg2+1) + "/" + arg1);
							date = arg3+"-"+(arg2+1)+"-"+arg1;
							dateSet = true;
						}
					},c.get(Calendar.YEAR),c.get(Calendar.MONTH),c.get(Calendar.DAY_OF_MONTH)).show();
				}
			});
			Button submit = (Button)ll.getChildAt(3);
			submit.setOnClickListener(new OnClickListener(){
				@Override
				public void onClick(View arg0) {
					if (dateSet && timeSet){
						SimpleDateFormat fmt = new SimpleDateFormat("dd-MM-yyyy HH:mm");
						try {
							alarmDate = fmt.parse(date + " " + time).getTime();
							message = ((EditText) ll.getChildAt(2)).getText().toString();
							setAlarm();
							//new AlertDialog.Builder(ReminderActivity.this).setMessage("Reminder Set").setPositiveButton("OK", null).show();
						} catch (ParseException e) {
							Log.d("Parse Error","Mmmmm");
							e.printStackTrace();
						}
						
					}else{
						new AlertDialog.Builder(ReminderActivity.this).setMessage("Either Date or Time is not set.").setPositiveButton("OK", null).show();
					}
				}
			});
			return ll;
		}
	}

}
