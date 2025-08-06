#########################
## Save labeled TextGrid intervals as individual wav files for all wav + TextGrid files in a directory
## Chisom Obasih
## January 2025
## Praat version: 6.4.12 (May 2, 2024)

## This script was originally written by Eric Doty in 2011 (see original comments below)
## This script takes all .wav sounds and their corresponding TextGrid files
## and extracts all labeled intervals in the first tier of the TextGrid and saves them as individual wav files
## the individual wav files are named after their original TextGrid interval label

#####
## NOTABLE CHANGES
#####

## I added messages to the info window to show when the script starts and ends

## I added a variable to the form to add a prefix to the output filename
## originally, the output files were named from the original sound file name, not the interval label
## (because the original script writer was using this for textgrid labels where it was the same word multiple times)
## (and that's why the interval number was appended to the file output)

## I removed the interval number from the output filename
## and I changed the output of the filename to just be the interval label (with the prefix)
## since the way I labeled the intervals already includes ways to distinguish files that are the same word
## so now the output filename is [prefix]_[interval_label].wav
## I updated the .TextGrid and .lab output names to match this, even though I personally am not saving these extra files


###### Orignal comments
###### Source: https://www.ling.upenn.edu/courses/Fall_2013/ling520/chop_soundfile.Praat

## chop_for_alignment.Praat
##
## This script takes a sound file and text grid with several labelled sections
## and extracts every labelled segment into its own sound file
##
## Eric Doty - 2011
## University of Pennsylvania - Linguistics Department

form Make text file from an IntervalTier in the selected TextGrid object
	comment Directory of TextGrids and wav files: (Note: Include final slash in directories)
	comment e.g. Mac: /User/docs/filedirectory/	PC: C:\Users\user\filedirectory\
	text Path_in /Users/chisomobasih/Exp-Research/Dissertation/stimuli-collection/discrimination-dictation-stimuli/tagged-speech/japanese/dictation-sentences/
	comment Tier number used for chopping
	integer Tier_num 1
	comment Directory to save chopped up sound files: (Note: Include final slash in directories)
	text Path_out /Users/chisomobasih/Exp-Research/Dissertation/stimuli-collection/discrimination-dictation-stimuli/tagged-speech/japanese/chopped-speech/dictation_sentences/
	comment Filename prefix (Note: include underscore after prefix)
	text File_prefix F_
	comment Do you want corresponding TextGrids for outputted .wav files?
	boolean Output_textgrids 0
	comment Do you want corresponding .lab files for outputted .wav files? (e.g. Used for some forced aligners)
	boolean Output_labfiles 0
endform

clearinfo

appendInfoLine: "Starting script..."

overwrite = 0

# text grids
 	Create Strings as file list... filelist 'path_in$'*.TextGrid

# select each file from the textgrid string to work on one at a time
	number_of_files = Get number of strings

 	for t from 1 to number_of_files
		select Strings filelist
		current_file$ = Get string... t

# open the text grid and wav files
		Read from file... 'path_in$''current_file$'
		object_name$ = selected$ ("TextGrid")
		Read from file... 'path_in$''object_name$'.wav

# for counting the number of extracted intervals		
		int = 0

# find the relevant intervals
		select TextGrid 'object_name$'
		number_of_intervals = Get number of intervals... tier_num

		for y from 1 to number_of_intervals
# find the label
			select TextGrid 'object_name$'
			interval_label$ = Get label of interval... tier_num y

# if the interval is labeled (not null)
			if interval_label$ <>"" and interval_label$ <>" " and interval_label$ <>"  " and interval_label$ <> "xxx"

# count it
				int = int+1

# find the start and end
				begin = Get starting point... tier_num y
				end = Get end point... tier_num y

# open the sound file and extract the utterance
				select Sound 'object_name$'
				Edit
				editor Sound 'object_name$'
				Select... begin end
				Extract selected sound (time from 0)
				Close
				select Sound untitled
# save it

				Write to WAV file... 'path_out$''file_prefix$''interval_label$'.wav
				Remove
				
# make the new TextGrid
				
				if output_textgrids == 1
					select TextGrid 'object_name$'
					
					Edit
					editor TextGrid 'object_name$'
					Select... begin end
					Extract selected TextGrid (time from 0)
					Close

					select TextGrid 'object_name$'
					Write to text file... 'path_out$''file_prefix$''interval_label$'.TextGrid
					Remove
				endif

# make the lab file
				if output_labfiles == 1
					fileappend "'path_out$''file_prefix$''interval_label$'.lab" 'interval_label$'
				endif
			endif
		endfor
		
# clean up
		select Sound 'object_name$'
		plus TextGrid 'object_name$'
		Remove

	endfor
select Strings filelist
Remove

appendInfoLine: "Done! Check your output folder."