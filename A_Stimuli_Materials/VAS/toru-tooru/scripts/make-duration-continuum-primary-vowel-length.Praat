#########################
## Make Duration continuum for vowel length contrast
## Chisom Obasih
## January 2025
## Praat version: 6.4.12 (May 2, 2024)




## This script is adapted from the script below that makes a duration continuum
## Original script by Matthew B. Winn (U. Minnesota) from August 2014
## (see original comments below)


## This script is intended to make a 7-step duration continuum of vowel duration
## in order to make a 7 x 5 matrix of vowel duration & V1-to-V2 pitch drop
## for the vowel length contrast of /o/ vs. /oo/ in the Japanese words "toru" vs. "tooru"



#####
## NOTABLE CHANGES
#####


## I changed the naming scheme such that the name of the output file is simply the words supplied in the form plus the interpolation step

## changed form defaults for duration, and changed names to make them more readable on the form
## default duration mininum set to vowel duration of toru_2_mod.wav file (104 ms)
## default duration maximumn set to vowel duration of tooru_1_mod.wav file (329 ms)
## changed min and max pitch defaults (50 and 500 respectively)


## (created this option for the 5-step vowel duration continuum in combo with the 7 step spectral continuum)
## added option to form to be able to change naming scheme such that the longest duration is Step 1 and the shortest duration is the max number of steps
## this is so the most "risen"-like tokens are at the ends of each continuum (Step 7 for spectral and Step 5 for duration)
## in the form, added boolean variable reverse
## added if statement to procedure makeDurationContinuum and added variable thisStep_rev
## thisStep_rev is first set as the number of steps and then reduces by 1 at the end of each iteration of the for loop
## if reverse was checked, using thisStep_rev for the naming scheme when sound file is renamed, if not, using the default thisStep

## changed default option for boolean reverse to be unchecked (I want the shortest duration to be Step 1 for this continuum)
## changed prefix and number of steps to 7
## changed the options for writing to the information window to show the durations of each step with 3 decimal places

#####

###### Orignal comments
###### Source: http://www.mattwinn.com/praat/Make_Duration_Continuum.txt
###### See also: http://www.mattwinn.com/praat.html

# # ## ### ##### ########  #############  ##################### 
# Praat Script
# Calculate continuum steps
#
### creates a duration continuum of all sounds in a directory that you choose
## important! ensure that the sounds all have the same time boundaries!
##	 you set the boundaries once, and then it uses those to act on all the remaining sounds in the directory 
# Matthew Winn
# August 2014
##################################
##################### 
############# 
######## 
#####
###
##
#
#
form Input Enter specifications for Formant settings
    comment shortest duration (ms): 
    real short_dur_ms 104
    comment longest duration (ms): 
    real long_dur_ms 329
    comment how many steps in the continuum? 
    integer steps 7
	comment Reverse naming scheme of steps? (i.e. longest duration is Step 1)
	boolean reverse 0
    comment enter minimum pitch
    real minpitch 50
    comment enter maximum pitch
    real maxpitch 500
    comment enter duration name prefix (insert underscores between words)
    word namingScheme toru_origin_tooru_dur_

endform

clearinfo
shortdur = short_dur_ms/1000
longdur = long_dur_ms/1000

call printHeader

call makeContinuum steps shortdur longdur dur_ 1

call enumerateSounds

call identifyLandmarks


for thisSound from 1 to numberOfSelectedSounds
    select sound'thisSound'
	name$ = selected$("Sound")
	call makeDurationContinuum 'name$' start end shortdur longdur steps 'namingScheme$'

endfor

### Procedures

procedure makeDurationContinuum .name$ .start .end .shortdur .longdur .steps .suffix$
	select Sound '.name$'
	.endTime = Get end time
	To Manipulation... 0.01 minpitch maxpitch
	Extract duration tier
	
	
	# create a variable that starts with the maximum number of steps
	# as the for loop iterates, this variable will decrease by 1
	# and can be used as an alternative naming scheme
	thisStep_rev = .steps

	for thisStep from 1 to .steps
		
		ratio = (dur_'thisStep')/(.end - .start)

		select DurationTier '.name$'
			Remove points between... 0 .endTime
			Add point... (.start-0.0001) 1
			Add point... .start ratio
			Add point... .end ratio
			Add point... (.end+0.0001) 1

		select Manipulation '.name$'
		plus DurationTier '.name$'
		Replace duration tier

        select Manipulation '.name$'
        Edit
		editor Manipulation '.name$'
			beginPause: "Check pulse points"
            	comment: "Adjust pulse points to make sure pitch is not wonky."
            	comment: "Click Continue when finished."
        	endPause: "Continue", 1
		Close
		endeditor

        select Manipulation '.name$'
		Get resynthesis (overlap-add)

		# if the reverse order option was chosen in the form, use thisStep_rev for naming scheme
		# if not, use normal naming scheme
		if reverse == 1
			Rename... '.suffix$''thisStep_rev'
		else
			Rename... '.suffix$''thisStep'
		endif

		# reduce thisStep_rev by 1 - should end at Step 1 for the last iteration of the for loop
		thisStep_rev = thisStep_rev - 1
	endfor
	
	select Manipulation '.name$'
	plus DurationTier '.name$'
	Remove
endproc



procedure identifyLandmarks
	select sound1
	firstName$ = selected$("Sound")
	Edit
		editor Sound 'firstName$'
		# prompts user to click on vowel beginning and end, create variables with values at points clicked

		 pause Click Get start of segment to be manipulated, click Continue when done
		 Move cursor to nearest zero crossing
		 start = Get cursor

		 pause Click Get end of segment to be manipulated, click Continue when done
		 Move cursor to nearest zero crossing
		 end = Get cursor
	Close
	endeditor
endproc















procedure enumerateSounds
	pause select all sounds to be used for this operation
	numberOfSelectedSounds = numberOfSelected ("Sound")

	for thisSelectedSound to numberOfSelectedSounds
		sound'thisSelectedSound' = selected("Sound",thisSelectedSound)
	endfor
endproc




procedure printHeader
	# creates simple header		
	print Step 'tab$' Duration 'tab$' 'newline$'
endproc


procedure makeContinuum .steps .low .high .prefix$ printvalues
	for thisStep from 1 to .steps

		temp = (('thisStep'-1)*('.high'-'.low')/('.steps'-1))+'.low'

		'.prefix$''thisStep' = temp
		check = '.prefix$''thisStep'
		if printvalues = 1
		print '.prefix$''thisStep''tab$''check:3' 'newline$'
		endif

	endfor
endproc