$(document).ready(function(){
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
});