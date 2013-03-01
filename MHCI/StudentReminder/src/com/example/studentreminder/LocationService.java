package com.example.studentreminder;

import java.util.ArrayList;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.os.IBinder;
import android.util.FloatMath;
import android.util.Log;

public class LocationService extends Service{

	public static final ArrayList<MyLocationAlarm> alarms = new ArrayList<MyLocationAlarm>();
	public static ReminderActivity rContext = null;
	public boolean isStarted = false;
	
	@Override
	public void onCreate(){
		super.onCreate();
		Log.d("Service", "Created");
		//alarms = new ArrayList<MyLocationAlarm>();
	}
	
	@Override
	public int onStartCommand(Intent intent, int flags, int startId){
		Log.d("Service","Started");
		if (!isStarted){
			LocationManager lm = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
			LocationListener ll = new LocationListener(){
				@Override
				public void onLocationChanged(Location loc) {
					Log.d("Location","Location Changed");
					MyLocationAlarm rm = null;
					for (MyLocationAlarm locAlarm: alarms){
						double dist = gps2m(locAlarm.getLatitude(),locAlarm.getLongitude(),loc.getLatitude(),loc.getLongitude());
						Log.d("String lata",locAlarm.getLatitude()+"");
						Log.d("String longa",locAlarm.getLongitude()+"");
						Log.d("String latb",loc.getLatitude()+"");
						Log.d("String longb",loc.getLongitude()+"");

						Log.d("distance",dist+"");
						if(dist<50){
							Intent alert = new Intent(LocationService.this,ReminderAlertActivity.class);
							alert.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
							alert.putExtra("com.example.studentreminder.message",locAlarm.getMessage());
							startActivity(alert);
							rm = locAlarm;
						}
					}
					alarms.remove(rm);
				}
				@Override
				public void onProviderDisabled(String arg0) {
					Log.d("Provider","Disabled");
				}
				@Override
				public void onProviderEnabled(String arg0) {
					Log.d("Provider","Enabled");
				}
				@Override
				public void onStatusChanged(String arg0, int arg1, Bundle arg2) {}
			};
			lm.requestLocationUpdates(LocationManager.GPS_PROVIDER, 12000, 5, ll);
			Log.d("Location Listener", "Listener Added");
		}
		isStarted = true;
		return 0;
	}
	
	public void addAlarm(double lon, double lat, String message, ReminderActivity ra){
		if (rContext == null)
			rContext = ra;
		alarms.add(new MyLocationAlarm(lon, lat, message));
	}
	
	private double gps2m(double lat_a, double lng_a, double lat_b, double lng_b) {
		double pk = (180/3.14169);
		double a1 = lat_a / pk;
		double a2 = lng_a / pk;
		double b1 = lat_b / pk;
		double b2 = lng_b / pk;
		double t1 = Math.cos(a1)*Math.cos(a2)*Math.cos(b1)*Math.cos(b2);
		double t2 = Math.cos(a1)*Math.sin(a2)*Math.cos(b1)*Math.sin(b2);
		double t3 = Math.sin(a1)*Math.sin(b1);
	    double tt = Math.acos(t1 + t2 + t3);
	    return 6366000*tt;
	}

	@Override
	public IBinder onBind(Intent arg0) {
		return null;
	}
	
	public class MyLocationAlarm{
		private double longitude;
		private double latitude;
		private String message;
		public MyLocationAlarm(double lon, double lat, String alm){
			longitude = lon;
			latitude = lat;
			message = alm;
		}
		public double getLongitude() {return longitude;}
		public double getLatitude() {return latitude;}
		public String getMessage() {return message;}	
	}
}
