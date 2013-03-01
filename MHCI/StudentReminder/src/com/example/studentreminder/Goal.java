package com.example.studentreminder;


public class Goal {

	String desc;
	int lower;
	int upper;
	String type;
	public String getDesc() {
		return desc;
	}
	public void setDesc(String desc) {
		this.desc = desc;
	}
	public int getLower() {
		return lower;
	}
	public Goal(String type, String desc, int lower, int upper) {
		super();
		this.type = type;
		this.desc = desc;
		this.lower = lower;
		this.upper = upper;
	}
	public Goal() {
	}
	public void setLower(int lower) {
		this.lower = lower;
	}
	public int getUpper() {
		return upper;
	}
	public void setUpper(int upper) {
		this.upper = upper;
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
}
