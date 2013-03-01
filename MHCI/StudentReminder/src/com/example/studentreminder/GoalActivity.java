package com.example.studentreminder;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.app.Activity;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.Spinner;
import android.widget.TextView;

public class GoalActivity extends Activity {

	private ArrayAdapter arrayadapter;
	private ArrayAdapter completedAdapter;
	List<String> descriptions = null;
	List<String> completed = null;
	LinearLayout view;
	DatabaseHandler db = null;
	AlertDialog goal = null;
	float lower = 1;
	float upper = 1;
	List<Goal> gol = null;
	ProgressBar pb = null;
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_goal);
		pb = (ProgressBar) findViewById(R.id.progressBar2);
		Button addGoalButton = (Button) findViewById(R.id.addGoal);
        view = (LinearLayout) LayoutInflater.from(getBaseContext()).inflate(R.layout.dialog_new_goal,null);		
        ListView lv = (ListView) findViewById(R.id.goals);
        ListView lvd = (ListView) findViewById(R.id.goalsCompletedNoList);
        db = new DatabaseHandler(this);
        gol = db.getGoals();
        descriptions = new ArrayList<String>();
        completed = new ArrayList<String>();
        if (goal == null){
        	makeDialog();
        }
        
        lvd.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> arg0, View view, int arg2,
					long arg3) {
				String[] sNumber = ((TextView)view).getText().toString().split("/");
				sNumber[0] = Integer.parseInt(sNumber[0])+1+"";
				((TextView)view).setText((Integer.parseInt(sNumber[0]))+"/"+sNumber[1]);
				lower+=1;
				pb.setProgress((int)((float)(lower/upper)*100));	
				Log.d("Setting progress to",(lower/upper)+"");
				int index = arg0.indexOfChild(view);
				db.increment(arrayadapter.getItem(index).toString());
				if (sNumber[0].equals(sNumber[1])){
					new AlertDialog.Builder(GoalActivity.this).setMessage("Goal Complete!").show();
					db.removeItem(descriptions.get(index));
					descriptions.remove(index);
					completed.remove(index);
					arrayadapter.notifyDataSetChanged();
					completedAdapter.notifyDataSetChanged();
					
				}
				}
		});
        
        for (Goal g: gol){
        	descriptions.add(g.desc);
        	completed.add(g.getLower() + "/" + g.getUpper());
        	lower += g.getLower();
        	upper += g.getUpper();
        }
        if(!descriptions.isEmpty())
        	arrayadapter = new ArrayAdapter(this,android.R.layout.simple_list_item_1,descriptions);
        	completedAdapter = new ArrayAdapter(this,android.R.layout.simple_list_item_1, completed);
        	lv.setAdapter(arrayadapter);
        	lvd.setAdapter(completedAdapter);
			
		addGoalButton.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(View arg0) {
				goal.show();
			}
		});
		pb.setProgress((int)((float)(lower/upper)*100));
		Button clearButton = (Button)findViewById(R.id.clearGoals);
		clearButton.setOnClickListener(new OnClickListener(){
			@Override
			public void onClick(View arg0) {
				db.clearGoals();
				arrayadapter.clear();
				completedAdapter.clear();
				upper =1;
				lower = 1;
				pb.setProgress(100);
			}
		});
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.activity_goal, menu);
		return true;
	}
	
	public void addGoal(String desc, int upper, String type){
		Goal g = new Goal(type,desc,0,upper);
		DatabaseHandler db = new DatabaseHandler(this);
		db.addGoal(g);
	}
	
	private void makeDialog(){
		AlertDialog.Builder goalB = new AlertDialog.Builder(this);
		goalB.setTitle("Add Goal");
		Log.d("set view","been called");
		goalB.setView(view);
		//goal.setNegativeButton("Cancel", null);
		goalB.setCancelable(true);
		goalB.setPositiveButton("Add", new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {
				LinearLayout t = (LinearLayout)view.getChildAt(0);
				EditText et = (EditText) t.getChildAt(1);
				Spinner sp = (Spinner)t.getChildAt(0);
				EditText de = (EditText) view.getChildAt(1);
				if (db.existsAnItem(de.getText().toString())){
					new AlertDialog.Builder(GoalActivity.this).setMessage("Item Already Exists, Please add a different description!").show();
					Log.d("Item already exists","BOOM");
				}else{
					addGoal(de.getText().toString(),Integer.parseInt(et.getText().toString()),String.valueOf(sp.getSelectedItem()));
					arrayadapter.add(de.getText().toString());
					completedAdapter.add("0/"+Integer.parseInt(et.getText().toString()));
					upper += Integer.parseInt(et.getText().toString());
					pb.setProgress((int)((float)(lower/upper)*100));	
					dialog.dismiss();
				}
			}
		});		
		Log.d("goal object id", Integer.toHexString(System.identityHashCode(goal)));
		goal = goalB.create();
	}
}
