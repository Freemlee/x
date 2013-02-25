import numpy
import math
import matplotlib.pyplot as plt
count = 1
import random

filename = ""

def toInt(x):
	return int(x)

def squares(x):
	return (x*x)

def sign(x):
	if x >= 0:
		return 1
	else:
		return 0

def zcrFunc(theLines):
	i = 1
	theLines = [0] + map(sign, theLines)
	newvals = []
	while i < 2641:
		newvals.append(theLines[i] - theLines[i-1])
		i = i + 1
	newvals = map(abs, newvals)
	return sums(newvals)

def toFloat(x):
	return float(x)

def sums(ll):
	i = 0
	j = 240
	listofsums = []
	while (j < 2640):
		listofsums.append(sum(ll[i:j]))
		i = i + 1
		j = j + 1
	return listofsums

def mainFunc(fileName):
	f = open(fileName,'r')
	lines = f.readlines()
	intLines = [0]*240
	intLines = intLines + map(toInt, lines)
	energy_temp = map(squares, intLines)
	magnitude_temp = map(abs, intLines)
	Z = numpy.mean(zcrFunc(intLines))
	E = math.log(numpy.mean(sums(energy_temp)))
	M = math.log(numpy.mean(sums(magnitude_temp)))
	listofenergy.append(E)
	listofmags.append(M)
	listofzcr.append(Z)
	#Z = math.log(map(toFloat,zcr))
	print filename + "\tE: " + str(E)[0:10] + "\tM: " + str(M)[0:10] + "\tZ: " + str(Z)[0:10]

def foldbreak(k, list_x):
    endlist = []
    count = 100/k
    while (count < 110):
	upper = count
	lower = upper - (100/k)
	endlist.append(list_x[lower:upper])
	count += (100/k)
    return endlist

listofenergy = [] 
listofmags = [] 
listofzcr = []

print "\n"
while (count <= 50):
	if (count < 10):
		filename1 = "silence_speech/silence_0" + str(count) + ".dat"
		filename2 = "silence_speech/speech_0" + str(count) + ".dat"
	else:
		filename1 = "silence_speech/silence_" + str(count) + ".dat"
		filename2 = "silence_speech/speech_" + str(count) + ".dat"
	mainFunc(filename1)
	mainFunc(filename2)
	count += 1

count = 0
while (count < 100):

	plt.figure(0)
	plt.scatter(listofenergy[count],listofmags[count],s=10,c='y')
	plt.scatter(listofenergy[count+1],listofmags[count+1],s=10,c='b')


	plt.figure(1)
	plt.scatter(listofenergy[count],listofzcr[count],s=10,c='y')
	plt.scatter(listofenergy[count+1],listofzcr[count+1],s=10,c='b')

	
	plt.figure(2)
	plt.scatter(listofmags[count],listofzcr[count],s=10,c='y')
	plt.scatter(listofmags[count+1],listofzcr[count+1],s=10,c='b')


	count += 2
plt.show()

k = 10


folded_energy = foldbreak(k,listofenergy)
folded_mags = foldbreak(k,listofmags)
folded_zcr = foldbreak(k,listofzcr)

def getVariance(mean, trnset):
	count = 0
	variance_count = 0
	while (count < 45):
		variance_count += ((trnset[count] - mean) * (trnset[count] - mean))
		count += 1
	return variance_count/45

count = 0
while (count < 10):
	test_energy = folded_energy[count]
	test_mags = folded_mags[count]
	test_zcr = folded_zcr[count]
	training_energy = []
	training_mags = []
	training_zcr = []
	i = 0
	while (i < 10):
		if (i == count):
			i += 1
		else:
			training_energy += folded_energy[i]
			training_mags += folded_mags[i]
			training_zcr += folded_zcr[i]
			i += 1
	
	i = 0
	training_speech_energy = []
	training_silence_energy = []
	training_speech_mags = []
	training_silence_mags = []
	training_speech_zcr = []
	training_silence_zcr = []

	while (i < 90):
		training_silence_energy.append(training_energy[i])
		training_speech_energy.append(training_energy[i+1])
		training_silence_mags.append(training_mags[i])
		training_speech_mags.append(training_mags[i+1])
		training_silence_zcr.append(training_zcr[i])
		training_speech_zcr.append(training_zcr[i+1])
		i += 2
	
	training_speech_mean_energy = numpy.mean(training_speech_energy)
	training_silence_mean_energy = numpy.mean(training_silence_energy)
	training_speech_mean_mags = numpy.mean(training_speech_mags)
	training_silence_mean_mags = numpy.mean(training_silence_mags)
	training_speech_mean_zcr = numpy.mean(training_speech_zcr)
	training_silence_mean_zcr = numpy.mean(training_silence_zcr)

	train_speech_energy_var = getVariance(training_speech_mean_energy, training_speech_energy)
	train_silence_energy_var = getVariance(training_silence_mean_energy, training_silence_energy)
	train_speech_mags_var = getVariance(training_speech_mean_mags, training_speech_mags)
	train_silence_mags_var = getVariance(training_silence_mean_mags, training_silence_mags)
	train_speech_zcr_var = getVariance(training_speech_mean_zcr, training_speech_zcr)
	train_silence_zcr_var = getVariance(training_silence_mean_zcr, training_silence_zcr)

	print "------------------- Test Iteration " + str(count) + "-------------------\n\n"

	print "training speech mean energy: " + str(training_speech_mean_energy)[:9] + "\t\ttraining speech variance energy: " + str(train_speech_energy_var)[:9]
	print "training silence mean energy: " + str(training_silence_mean_energy)[:9] + "\t\ttraining silence variance energy: " + str(train_silence_energy_var)[:9] + "\n"

	print "training speech mean mags: " + str(training_speech_mean_mags)[:9] + "\t\ttraining speech variance mags: " + str(train_speech_mags_var)[:9]
	print "training silence mean mags: " + str(training_silence_mean_mags)[:9] + "\t\ttraining silence variance mags: " + str(train_silence_mags_var)[:9] +"\n"

	print "training speech mean zcr: " + str(training_speech_mean_zcr)[:9] + "\t\ttraining speech variance zcr: " + str(train_speech_zcr_var)[:9]
	print "training silence mean zcr: " + str(training_silence_mean_zcr)[:9] + "\t\ttraining silence variance zcr: " + str(train_silence_zcr_var)[:9] + "\n"
	
	i = 0;
	
	while (i < 10):

		test_val_e = test_energy[i]
		test_val_m = test_mags[i]
		test_val_z = test_zcr[i]

		#ENERGY
		diff_speech_e = abs(test_val_e - training_speech_mean_energy)
		diff_silence_e = abs(test_val_e - training_silence_mean_energy)
		diff_percent_similar_speech_e = (diff_speech_e/train_speech_energy_var)
		diff_percent_similar_silence_e = (diff_silence_e/train_silence_energy_var)

		#MAGNITUDE
		diff_speech_m = abs(test_val_m - training_speech_mean_mags)
		diff_silence_m = abs(test_val_m - training_silence_mean_mags)
		diff_percent_similar_speech_m = (diff_speech_m/train_speech_mags_var)
		diff_percent_similar_silence_m = (diff_silence_m/train_silence_mags_var)

		#ZCR
		diff_speech_z = abs(test_val_z - training_speech_mean_zcr)
		diff_silence_z = abs(test_val_z - training_silence_mean_zcr)
		diff_percent_similar_speech_z = (diff_speech_z/train_speech_zcr_var)
		diff_percent_similar_silence_z = (diff_silence_z/train_silence_zcr_var)

		guess = 0;
		energy_guess = ""
		mags_guess = ""
		zcr_guess = ""

	#ENERGY
		if diff_percent_similar_speech_e < diff_percent_similar_silence_e and diff_percent_similar_speech_e < 1:
			energy_guess = "speech"
		elif ((diff_percent_similar_silence_e < diff_percent_similar_speech_e) and (diff_percent_similar_silence_e < 1)):
			energy_guess = "silence"
		else:
			energy_guess = "unknown"
		print "Guess for ENERGY \t" + str(count) + "-" + str(i) + " is \t" + energy_guess.upper()

	#MAGNITUDE
		if diff_percent_similar_speech_m < diff_percent_similar_silence_m and diff_percent_similar_speech_m < 1:
			mags_guess = "speech"
		elif ((diff_percent_similar_silence_m < diff_percent_similar_speech_m) and (diff_percent_similar_silence_m < 1)):
			mags_guess = "silence"
		else:
			mags_guess = "unknown"
		print "Guess for MAGNITUDE \t" + str(count) + "-" + str(i) + " is \t" + mags_guess.upper()
	#ZCR
		if diff_percent_similar_speech_z < diff_percent_similar_silence_z and diff_percent_similar_speech_z < 1:
			zcr_guess = "speech"
		elif ((diff_percent_similar_silence_z < diff_percent_similar_speech_z) and (diff_percent_similar_silence_z < 1)):
			zcr_guess = "silence"
		else:
			zcr_guess = "unknown"
		print "Guess for ZCR \t\t" + str(count) + "-" + str(i) + " is \t" + zcr_guess.upper()
		
	#FINAL
		final_guess = "FAIL"
		if (energy_guess == mags_guess and energy_guess == "unknown"):
			final_guess = zcr_guess
		elif (energy_guess == zcr_guess and energy_guess == "unknown"):
			final_guess = mags_guess
		elif (mags_guess == zcr_guess and mags_guess == "unknown"):
			final_guess = energy_guess
		elif (energy_guess == mags_guess or energy_guess == zcr_guess):
			final_guess = energy_guess
		elif (mags_guess == zcr_guess):
			final_guess = mags_guess
		else:
			final_guess = "unknown"
		print "Final Guess is \t\t" + str(count) + "-" + str(i) + " is \t" + final_guess.upper() + "\n"
		i += 1
	count += 1



# Calculate the probability of it being one or the other. 








