package com.example.studentreminder;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

public class DatabaseHandler extends SQLiteOpenHelper{
	
	private static final int DATABASE_VERSION = 1;
    private static final String DATABASE_NAME = "StudentDatabase";
    private static final String TABLE_GOALS = "goals";

    public DatabaseHandler(Context c){
    	super(c,DATABASE_NAME,null,DATABASE_VERSION);
    }
    
	@Override
	public void onCreate(SQLiteDatabase db) {
		String createGoalsDB = "CREATE TABLE " + TABLE_GOALS + " (id INTEGER PRIMARY KEY AUTOINCREMENT, type TEXT, upper INTEGER, lower INTEGER, desc TEXT)";
		db.execSQL(createGoalsDB);
	}

	@Override
	public void onUpgrade(SQLiteDatabase db, int v1, int v2) {
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_GOALS);
		onCreate(db);
	}
	
	public void addGoal(Goal goal){
		SQLiteDatabase db = this.getWritableDatabase();
		ContentValues vals = new ContentValues();
		vals.put("type", goal.type);
		vals.put("desc", goal.desc);
		vals.put("upper", goal.upper);
		vals.put("lower", 0);
		db.insert(TABLE_GOALS, null, vals);
		db.close();
	}
	
	public void removeItem(String desc){
		SQLiteDatabase db = this.getWritableDatabase();
		db.delete(TABLE_GOALS, "desc='" + desc + "'" , null);
		db.close();
	}
	
	public List<Goal> getGoals(){
		SQLiteDatabase db = this.getReadableDatabase();
		Cursor results = db.rawQuery("SELECT * FROM " + TABLE_GOALS, null);
		List<Goal> goals = new ArrayList<Goal>();
		while (results.moveToNext()){
			Goal tGoal = new Goal();
			tGoal.setType(results.getString(1));
			tGoal.setUpper(Integer.parseInt(results.getString(2)));
			tGoal.setLower(Integer.parseInt(results.getString(3)));
			tGoal.setDesc(results.getString(4));
			goals.add(tGoal);
		}
		db.close();
		return goals;
	}
	
	public boolean existsAnItem(String desc){
		SQLiteDatabase db = this.getReadableDatabase();
		Cursor res = db.rawQuery("Select * From " + TABLE_GOALS + " where desc = '" + desc + "'", null);
		boolean ans = false;
		if(res.getCount() == 1){
			ans = true;
		}
		Log.d("Matches",res.getCount()+"");
		db.close();
		return ans;
	}
	
	public void increment(String desc){
		SQLiteDatabase db = this.getWritableDatabase();
		db.execSQL("update " + TABLE_GOALS + " set lower = lower + 1 where desc = '" + desc + "'");
		db.close();
	}
	
	public void clearGoals(){
		SQLiteDatabase db = this.getWritableDatabase();
		db.delete(TABLE_GOALS, null, null);
		db.close();
	}
		
}
