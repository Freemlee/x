package com.example.copypastesync;

import org.json.JSONException;
import org.json.JSONObject;

public class MakeData {
	private static final String CODE = "code";
	private static final String CARGO = "cargo";
	public static String makeData(int code, JSONObject cargo) throws JSONException{
		switch (code){
			case MainActivity.CLIPBOARD_REQUEST_CODE:
				return new JSONObject().accumulate(CODE, "CR").accumulate(CARGO, cargo.get("text")).toString();
			case MainActivity.SHAKE_REQUEST_CODE:
				return new JSONObject().accumulate(CODE, "SR").accumulate(CARGO, null).toString();
			default:
				return null;
		}
	}
}
