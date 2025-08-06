#########################
## Save selected files to specified directory
## Chisom Obasih
## December 2024

###### Adapted from script by Matt Winn 
###### Source: http://www.mattwinn.com/praat/Save_all_selected_sounds.txt
###### See also: http://www.mattwinn.com/praat.html

### Original header comments

# # ## ### ##### ########  #############  ##################### 
# Praat Script
# save all selected sounds
#    Note: this script might fail for networked computers, 
#       or some operating systems.
#       I don't use a Mac, so I'm not sure if it works on a Mac. 
#
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

form Enter path to directory to save files
	comment Enter directory path (a folder that already exists)
	sentence save_directory /Users/chisomobasih/Desktop/continua/I_IH_spec_dur/spec_dur
endform

clearinfo
pause select all sounds to be used for this operation
numberOfSelectedSounds = numberOfSelected ("Sound")

for thisSelectedSound to numberOfSelectedSounds
	sound'thisSelectedSound' = selected("Sound",thisSelectedSound)
endfor

for thisSound from 1 to numberOfSelectedSounds
    select sound'thisSound'
	name$ = selected$("Sound")

    Save as WAV file... 'save_directory$'/'name$'.wav

endfor

#re-select the sounds
select sound1
for thisSound from 2 to numberOfSelectedSounds
    plus sound'thisSound'
endfor