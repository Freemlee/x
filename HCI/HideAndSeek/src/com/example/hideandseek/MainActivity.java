package com.example.hideandseek;

import java.util.ArrayList;

import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.os.Vibrator;
import android.app.Activity;
import android.content.Context;
import android.view.Menu;
import android.widget.TextView;

public class MainActivity extends Activity {
	TextView mainText;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        final Vibrator v = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
        Location peerLocation = new Location(LocationManager.GPS_PROVIDER);
        peerLocation.setLatitude(55.868696);
        peerLocation.setLongitude(-4.282449);
        
        mainText = (TextView) findViewById(R.id.MainText);
        LocationManager locationManager = (LocationManager) getSystemService(Context.LOCATION_SERVICE);
        locationManager.requestLocationUpdates(
                LocationManager.GPS_PROVIDER, 8000, 0, new MyLocationListener(v,peerLocation,this)
        );

        }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.activity_main, menu);
        return true;
    }
}
