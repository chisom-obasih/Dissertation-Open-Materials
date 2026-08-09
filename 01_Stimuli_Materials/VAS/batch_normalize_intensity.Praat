#########################
## Batch processing of all sounds in a directory (intensity, save)
## Chisom Obasih
## January 2025
## Praat version: 6.4.12 (May 2, 2024)

## This script is adapted from the script below that scales intensity of all sounds in a directory
## Original script by Matthew B. Winn from March 2020
## (see original comments below)

## This script additionally adds options for other batch processing 
## such as adjusting filenames and saving as both wav and mp3 files

#####
## NOTABLE CHANGES
#####

## added output directory to form to save files in a different place than the retrieval folder
## changed saving procedure to save to the user suppplied output folder
## added option in pause window of userSelectSave to also save as mp3 
## added if statement in save sounds to save wav files and mp3 files into separate subfolders if user selected to save mp3 files
## otherwise if not, the output files are saved in the top level of the output folder the user supplied


###### Orignal header comments
###### Source: https://github.com//ListenLab/Praat/blob/master/Scale_intensity_all_sounds_in_folder_v1.txt

# # ## ### ##### ########  #############  ##################### 
## Scale intensity of all sounds in a Directory
##### Check for clipped extrema
# Give an option to lower the output level 
# until none of the sounds are clipped
#
## Matthew B. Winn
## March 2020
#
## NOTE: It helps to have the Praat info window open as you run this script,
###### as it will update info for you as it proceeds. 
#
## Once the script is done and you choose to save the files, 
#### Look in the original sound directory - 
#### there you will find a new sub-folder of normalized sounds. 
##################################
##################### 
############# 
######## 
#####
###
##
#
#
form Input Enter specifications for Intensity scaling
    comment Enter desired intensity (dB):
    	real intensity 67
    comment Enter directory where the ORIGINAL sound files will be retrieved:
    	sentence soundDir /Users/chisomobasih/Desktop/continua/D_T_VOT_F0/Stimuli
	comment Enter directory to save the output files (full path)
		sentence output_folder /Users/chisomobasih/Desktop/continua/final_VAS_stim
    comment Enter filename to which the info should be written (without extension)
    	sentence outFile D_T_intensity_info
endform



call changeIntensities


procedure changeIntensities
	clearinfo
	clippedSounds = 0

	# Reads in a list of files
	Create Strings as file list... list 'soundDir$'/*.wav
	numberOfFiles = Get number of strings

	# make a list of each sound in the directory
	for thisFile to numberOfFiles
    	select Strings list
    	fileName$ = Get string... thisFile
    	name$ = fileName$ - ".wav"

    	# Reads in all sound (wav) files
		Read from file... 'soundDir$'/'name$'.wav

    	# get intensity of the original sound
		old_Int = Get intensity (dB)
		print 'name$' was 'old_Int:1'   

    	# scale it to the user-defined intensity
		Scale intensity... 'intensity'
		int_diff = 'intensity' - 'old_Int'
		print & adjusted by 'int_diff:2' to get 'intensity:1' 'tab$'

    	# Check to see if it clips
		max = Get absolute extremum... 0 0 Sinc70
		if max >= 1
			clippedSounds = clippedSounds + 1
			print CLIPPED
		endif

		# wrap to the next line
		print 'newline$'
	
		# Remove the sound object from the objects list 
		Remove
	endfor

	# decide whether you want to save the files
	call userSelectSave
endproc

procedure userSelectSave
	choice = 1

	beginPause ("'clippedSounds' sounds have been clipped (see info window for details)")
	comment ("'clippedSounds' sounds have been clipped")
	comment ("Do you want to save the files?")
       optionMenu ("Choice", choice)
          option ("Yes - save the sounds")
          option ("No - Quit")
          option ("Lower level by 1 dB and measure again")
          option ("Lower level by 2 dB and measure again")
          option ("Lower level by 3 dB and measure again")
          option ("Lower level by 4 dB and measure again")
          option ("Lower level by 5 dB and measure again")
	
	comment ("If choosing to save, do you want to save mp3 files in addition to wav files?")
		boolean: "save_as_mp3_also", 1

       comment ("Then click Continue.")

endPause ("Continue", 1)
 	if choice = 1
 		call saveSounds

 	elif choice = 2
 		select all
 		Remove
 	else
 	 	select all
 		Remove
 		# lower by 5 is the 7th choice, lower by 4 is the 6th choice;
 		# adjustment is the choice # minus 2 
	 	intAdjustment = choice-2
	 	intensity = intensity-(intAdjustment)
	 	call changeIntensities
	endif
endproc

procedure saveSounds

	if save_as_mp3_also = 1
		# make two subfolders
		wav_directory$ = "'output_folder$'" + "/wav"
		mp3_directory$ = "'output_folder$'" + "/mp3"

		createDirectory: wav_directory$
		createDirectory: mp3_directory$
	else
		# otherwise, the output will just be the supplied output_folder
		wav_directory$ = "'output_folder$'"
	endif

	# first deletes any existing output file
	filedelete 'wav_directory$'/'outFile$'.txt

	# creates simple header		
	fileappend Step 'tab$' Duration 'tab$' 'newline$'

	# make a list of each sound in the directory
	for thisFile to numberOfFiles
		select Strings list
		fileName$ = Get string... thisFile
		name$ = fileName$ - ".wav"

		# Reads in the sound (wav) file
		Read from file... 'soundDir$'/'name$'.wav

		# scale it to the user-defined intensity (updated to reflect clip-adjustment changes)
		Scale intensity... 'intensity'

		# save it
		Save as WAV file... 'wav_directory$'/'name$'.wav

		# also save it as mp3 if that was selected
		if save_as_mp3_also = 1
			Save as highest quality MP3 file... 'mp3_directory$'/'name$'.mp3
		endif
	
		Remove
	endfor
	
	# this saves the file in the top level folder whether or not wav and mp3 subfolders have been created
	call saveInfoWindow "'output_folder$'" 'outFile$'

	select Strings list
	Remove
endproc

procedure saveInfoWindow outputDirectory$ outputFileName$
	filedelete 'outputDirectory$'/'outputFileName$'.txt
	fappendinfo 'outputDirectory$'/'outputFileName$'.txt
endproc

# 
# 
## 
### 
##### 
######## 
############# 
##################### 
################################## DONE