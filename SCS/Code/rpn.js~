$(document).ready(function(){
	var globRpn = 1;
	var globDetr = 1;
	var globSev = 1;

	$('#rpn1').click(function(){
		var roads = $('#exit-roads').val();
		if ($('#team-radio').attr('checked')){
			var detect = 2;
		}else{
			var detect = 4;}
		if (roads >= 5){
			severity = 2
		}
		else if (roads >= 4){
			severity = 3
		}
		else if (roads >= 3){
			severity = 4
		}
		else if (roads >= 2){
			severity = 5
		}
		else if (roads >= 1){
			severity = 6
			
		}else if (roads >= 0){
			severity = 7
		}
		$('#severity').text(severity);
		$('#occurance').text("2");
		$('#detect').text(detect);
		$('#rpn').text(severity * 2 * detect);
		plotMarker(detect, severity);
	});
	
	$('#rpn3').click(function(){
		var severity = 0;
		if ($('#pit-limits').attr('checked')){
			severity += 2;
		}else{severity += 4;}
		
		if ($('#pit-regs').attr('checked')){
			severity += 1;
		}else{severity += 3;}
		
		$('#severity').text(severity);
		$('#occurance').text("2");
		$('#detect').text("2");
		$('#rpn').text(severity * 2 * 2);
		plotMarker(2, severity);

	});
	
	$('#rpn4').click(function(){
		var severity = 0;
		if ($('#pit-limits2').attr('checked')){
			severity += 2;
		}else{severity += 4;}
		
		$('#severity').text(severity);
		$('#occurance').text("2");
		$('#detect').text("2");
		$('#rpn').text(severity * 2 * 2);
		plotMarker(2, severity);

	});
	
	$('#rpn5').click(function(){
		var severity = 0;
		var detect = 0;
		var occur = 0;
		
		if ($('#marshalls-deployed').attr('checked')){
			detect += 3;
		}else{detect += 6;}
		
		if ($('#restricted-zones').attr('checked')){
			occur += 2;
		}else{occur += 5;}
		
		if ($('#spectators-grid').attr('checked')){
			occur += 2;
		}else{occur += 0;}
		
		if ($('#medics-available').attr('checked')){
			severity += 2;
		}else{severity += 6;}
		
		if ($('#track-regs').attr('checked')){
			severity += 1;
		}else{severity += 3;}
		
		$('#severity').text(severity);
		$('#occurance').text(occur);
		$('#detect').text(detect);
		$('#rpn').text(severity * occur * detect);
		plotMarker(detect, severity);
	});
	
	$('#rpn6').click(function(){
		var detect = 0;
		if ($('#ect-check').attr('checked')){
			detect = 1;
		}else{detect = 3;}
		detect = detect + parseInt($('#ect-failures').val(), 10);
		$('#severity').text("N/A");
		$('#occurance').text("N/A");
		$('#detect').text(detect);
		$('#rpn').text(detect);
			
	});
	
	$('#rpn7').click(function(){
		var detect = 0;
		
		if ($('#flags-available').attr('checked')){
			detect += 1;
		}else{detect += 5;}
		
		if ($('#radio-checked').attr('checked')){
			detect += 1;
		}else{detect += 3;}
		$('#severity').text("Depends upon incident");
		$('#occurance').text("Depends upon incident");
		$('#detect').text(detect);
		$('#rpn').text(detect);
		
	});
	
	$('#rpn8').click(function(){
		var crashed = $('#expected-crashes').val();
		var occur = 3;
		if (crashed > 10){
			occur = 5;}
		else if (crashed <= 0){
			occur = 1;
		}else{
			occur = crashed/2
		}
		$('#severity').text("Severity ranging depending on result of manourever");
		$('#occurance').text(occur);
		$('#detect').text("Depends upon other factors (eg. punctured tyre)");
		$('#rpn').text(occur + " with variance");
			
	});
	
	$('#rpn10').click(function(){
		var crashed = $('#expected-crashes').val();
		var occur = 0;
		number = $('#number-safety-cars').val();
		if (number > 3){
			occur += 0;
		}else if (number > 2){
			occur += 1.5;
		}else if (number > 1){
			occur += 3;
		}else if (number >= 0){
			occur += 4.5;
		}
		if ($('#safety-car-checked').attr('checked')){
			occur += 0;
		}else{occur += 4;}
		
		$('#severity').text("Depending on accident");
		$('#occurance').text(occur);
		$('#detect').text("N/A");
		$('#rpn').text(occur);
	});
	
	$('#rpn11').click(function(){
		var severity = 1;
		var occur = 1;
		var detect = 10;
		if ($('#fatigue-hydration').attr('checked')){}else{severity += 4;}
		if ($('#fatigue-radio').attr('checked')){detect = 2;}
		if ($('#race-time').val() > 120){
			occur += 5;
		}
		if ($('#g-force').val() > 5){
			severity += 2.5
		}
		
		$('#severity').text(severity);
		$('#occurance').text(occur);
		$('#detect').text(detect);
		$('#rpn').text(occur * severity * detect);
		plotMarker(detect, severity);
	});
	
	$('#rpn13').click(function(){
		var severity = 1;
		var occur = 1;
		var detect = 8;
		var level = $('#terror-alert').val();
		if (level >= 5){
			occur += 3.5;
		}else if (level >= 4){
			occur += 3;
		}else if (level >= 3){
			occur += 2.5;
		}else if (level >= 2){
			occur += 2;
		}else if (level >= 1){
			occur += 1.5;
		}else{
			occur += 1;
		}
		
		var threats = $('#terror-threats').val();
		
		if (threats >= 5){
			occur += 3.5;
			detect -= 3.5;
		}else if (threats >= 4){
			occur = 3;
			detect -= 3.5;
		}else if (threats >= 3){
			occur = 2.5;
			detect -= 3.5;
		}else if (threats >= 2){
			occur = 2;
			detect -= 3.5;
		}else if (threats >= 1){
			occur = 1.5;
			detect -= 3.5;
		}else{
			occur = 1;
			detect -= 3.5;
		}
		
		if ($('#terror-police').attr('checked')){
		}else{
			detect += 2;
			severity += 2;}
		
		$('#severity').text(severity);
		$('#occurance').text(occur);
		$('#detect').text(detect);
		$('#rpn').text(occur * severity * detect);

		plotMarker(detect, severity);
	});
});