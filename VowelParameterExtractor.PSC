## VowelParameterExtractor (C) SILVA JR., Leonidas (UEPB)
## Script for extracting f1-f4 acoustic parameters (individual, mean, st.dev. and derivative values), 
## raw and Lobanov's normalized duration,
## the cartesian distances (under development)
## and plot vowels in the f2xf1 acoustic space
##------ HOW TO CITE THIS SCRIPT ------##
## SILVA JR., Leonidas. (2022) VowelParameterExtractor (version 1.0). Computer program for Praat.
## Available in: <https://github.com/leonidasjr/VowelCode>.

form Input parameters
	word Output_file 00_formants
	word Output_file_2 00_duration
	integer Tier 2
	positive Number_of_formants 5.5
	positive Window_length 0.025
	real Min_F1 200
	real Max_F1 1000
	real Min_F2 500
	real Max_F2 3000
	boolean Plot_vowel_space 1
	integer IPA_font_size 20
	choice IPA_color 1
		button Black
		button Red
	boolean Draw_ellipse 1
	choice Ellipse_color 1
		button Black
		button Red
endform

## cleaning up Praat's objects window and appended information before workflow
numberOfSelectedObjects = numberOfSelected()
if numberOfSelectedObjects > 0
	select all
	Remove
endif

Create Strings as file list... audioDataList *.wav
numberOfFiles = Get number of strings
fileOut$ = output_file$ + ".txt"
filedelete 'fileOut$'
fileOut2$ = output_file_2$ + ".txt"
filedelete 'fileOut2$'

## returnring two output files for the values of the formants and the duration respectively
filedelete 'fileOut$'
fileappend 'fileOut$' IPA 'tab$' f1 'tab$' f2 'tab$' f3 'tab$' f4 'tab$' 
...f1norm 'tab$' f2norm 'tab$' f3norm  'tab$' f4nrom 'tab$'
...f1sd 'tab$' f2sd 'tab$' f3sd 'tab$' f4sd 'tab$' 
...d1f1 'tab$' d1f2 'tab$' d1f3 'tab$' d1f4 'tab$' raw_dur 'tab$' durnorm 'newline$'

filedelete 'fileOut2$'
fileappend 'fileOut2$' IPA 'tab$' raw_dur 'tab$' norm_dur 'newline$'

for y from 1 to numberOfFiles
    select Strings audioDataList
    filename$ = Get string... y
    Read from file... 'filename$'
    soundname$ = selected$ ("Sound")
    grid$ = soundname$ + ".TextGrid"
	Read from file... 'grid$'
	select TextGrid 'soundname$'
	gridname$ = selected$("TextGrid")
	countlabels = Count intervals where... 'tier' "is not equal to" ""

	## calculating raw and normalized duration (Lobanov's method)
	meandur = 0
	sdDur = 0

	select Sound 'soundname$'
		plus TextGrid 'gridname$'
	Extract non-empty intervals... 'tier' "yes"
		durID = selected ("Sound")
		objID = selected ("Sound", -1)
	for k from durID to objID
		select 'k'
		dur = Get total duration
		dur = dur *1000
		meandur = 'dur' + meandur
	endfor
	meandur = meandur / countlabels

	for l from durID to objID
		select 'l'
		dur = Get total duration
		dur = dur * 1000
		sdDur = sdDur + ('dur' - meandur) * ('dur' - meandur)
	endfor
	sdDur = sqrt(sdDur / (countlabels - 1))
		
	for m from durID to objID
		select 'm'
		dur = Get total duration
		dur = dur * 1000
		interval_label$ = selected$ ("Sound")
		durnorm = ('dur' - meandur) / sdDur
		fileappend 'fileOut2$' 'interval_label$' 'tab$' 'dur:2' 'tab$' 'durnorm:2' 'newline$'
	endfor

	## calculating formant mean, st.dev. raw and normalized formant values (Lobanov's method),
	## and dynamic formant trajectories (first drivative values)
	select TextGrid 'gridname$'
	numberOfIntervals = Get number of intervals... 'tier'
	sound_counter = 0

	for j from 2 to numberOfIntervals
		label_IPA$ = Get label of interval... 'tier' 'j'
  		if label_IPA$ = "i" or label_IPA$ = "\ic"  or label_IPA$ = "e" or label_IPA$ = "\ef" 
  		... or label_IPA$ = "\ae" or label_IPA$ = "a"  or label_IPA$ = "vt"  or label_IPA$ = "\sw" 
  		... or label_IPA$ = "\as" or label_IPA$ = "\ab"  or label_IPA$ = "\ct"  or label_IPA$ = "o"
  		... or label_IPA$ = "\hs"  or label_IPA$ = "u"
			start = Get starting point... 'tier' 'j'
			end = Get end point... 'tier' 'j'
			midpoint = (end - start)/2
			midpoint = start + midpoint
			dur = (end - start) * 1000
			dur_raw = round((end - start) * 1000)
				
			select Sound 'soundname$'
			Extract part... start end rectangular 1.0 yes
			part_sound$ = selected$("Sound")
			select Sound 'part_sound$'
			To Formant (burg)... 0 'number_of_formants' 5500 0.025 50
			select Formant 'part_sound$'
				f1 = Get value at time... 1 'midpoint' Hertz Linear
				f2 = Get value at time... 2 'midpoint' Hertz Linear
				f3 = Get value at time... 3 'midpoint' Hertz Linear
				f4 = Get value at time... 4 'midpoint' Hertz Linear
		
				f1mean = Get mean: 1, 0, 0, "hertz"
				f2mean = Get mean: 2, 0, 0, "hertz"
				f3mean = Get mean: 3, 0, 0, "hertz"
				f4mean = Get mean: 4, 0, 0, "hertz"
		
				f1sd = Get standard deviation: 1, 0, 0, "hertz"
				f2sd = Get standard deviation: 2, 0, 0, "hertz"
				f3sd = Get standard deviation: 3, 0, 0, "hertz"
				f4sd = Get standard deviation: 4, 0, 0, "hertz"
			
				f1norm = (f1 - f1mean)/f1sd
				f2norm = (f2 - f2mean)/f2sd
				f3norm = (f3 - f3mean)/f3sd
				f4norm = (f4 - f4mean)/f4sd

			## alculating formant dynamic trajectory
			select Sound 'soundname$'
				plus TextGrid 'gridname$'
			Extract intervals where: 'tier', "no", "is not equal to", ""
			sound_counter = sound_counter + 1
			select Sound 'soundname$'__'sound_counter'
			To Formant (burg)... 0 'number_of_formants' 5500 0.025 50
				start_time = 'window_length'
				end_time = Get end time

				## df1
				f1onset = Get value at time... 1 'start_time' Hertz Linear
				time_f1onset = start_time
				current_time = start_time
				f1prev = f1onset

				formant_step = Get time step
				
				repeat
					current_time = current_time + 'formant_step'
					currentF1 = Get value at time... 1 'current_time' Hertz Linear
					deltaF1 = currentF1 - f1prev
					f1prev = currentF1
					endloop = 0

					if abs(deltaF1 / f1onset) < 0.05
						endloop = 1
					endif
				until endloop or current_time >= end_time
				
				## df2
				f2onset = Get value at time... 2 'start_time' Hertz Linear
				time_f2onset = start_time
				current_time = start_time
				f2prev = f2onset

				formant_step = Get time step
				
				repeat
					current_time = current_time + 'formant_step'
					currentF2 = Get value at time... 2 'current_time' Hertz Linear
					deltaF2 = currentF2 - f2prev
					f2prev = currentF2
					endloop = 0

					if abs(deltaF2 / f2onset) < 0.05
						endloop = 1
					endif
				until endloop or current_time >= end_time
				
				## df3
				f3onset = Get value at time... 3 'start_time' Hertz Linear
				time_f3onset = start_time
				current_time = start_time
				f3prev = f3onset

				formant_step = Get time step
				
				repeat
					current_time = current_time + 'formant_step'
					currentF3 = Get value at time... 3 'current_time' Hertz Linear
					deltaF3 = currentF3 - f3prev
					f3prev = currentF3
					endloop = 0

					if abs(deltaF3 / f3onset) < 0.05
						endloop = 1
					endif
				until endloop or current_time >= end_time

				## df4
				f4onset = Get value at time... 4 'start_time' Hertz Linear
				time_f4onset = start_time
				current_time = start_time
				f4prev = f4onset

				formant_step = Get time step
				
				repeat
					current_time = current_time + 'formant_step'
					currentF4 = Get value at time... 4 'current_time' Hertz Linear
					deltaF4 = currentF4 - f4prev
					f4prev = currentF4
					endloop = 0
					if abs(deltaF4 / f4onset) < 0.05
						endloop = 1
					endif
				until endloop or current_time >= end_time

				fileappend 'fileOut$' 'label_IPA$' 'tab$' 'f1:0' 'tab$' 'f2:0' 'tab$' 'f3:0' 'tab$' 'f4:0' 'tab$' 
				...'f1norm:2' 'tab$' 'f2norm:2' 'tab$' 'f3norm:2' 'tab$' 'f4norm:2' 'tab$' 
				...'f1sd:2' 'tab$' 'f2sd:2' 'tab$' 'f3sd:2' 'tab$' 'f4sd:2' 'tab$' 
				...'deltaF1:2' 'tab$' 'deltaF2:2' 'tab$' 'deltaF3:2' 'tab$' 'deltaF4:2' 'tab$' 'dur:2' 'tab$' 'durNorm' 'newline$'
			select TextGrid 'gridname$'
		endif
	endfor
endfor

select all
Remove

## concatenating sound and TextGrid files of all tokens
Create Strings as file list... audioDataList *.wav
numberOfFiles = Get number of strings
for y from 1 to numberOfFiles
    select Strings audioDataList
    filename$ = Get string... y
    Read from file... 'filename$'
    soundname$ = selected$ ("Sound")
endfor

select all
	minus Strings audioDataList
Concatenate
selectObject: "Sound chain"
Rename: "all_files"
soundname$ = "Sound all_files"
select all
	minus 'soundname$'
Remove

Create Strings as file list... tgDataList *.TextGrid
numberOfFiles = Get number of strings
for y from 1 to numberOfFiles
    select Strings tgDataList
    filename$ = Get string... y
    Read from file... 'filename$'
    gridname$ = selected$ ("TextGrid")
endfor

gridname$ = selected$ ("TextGrid")
select all
	minus Strings tgDataList
	minusObject: soundname$
Concatenate
selectObject: "TextGrid chain"
Rename: "all_files"
gridname$ = "TextGrid all_files"
select all
	minusObject: soundname$
	minusObject: gridname$
Remove

## PLOTTING PROCEDURES ##
#select TextGrid 'gridname$'

## plotting the acoustic space for vowels
if plot_vowel_space = 1
	call grid
elsif plot_vowel_space = 0
	call just_ipa_symbol
endif

procedure grid
	Erase all
	#Viewport... 'min_horizontal_range' 'max_horizontal_range' 'min_vertical_range' 'max_vertical_range'
	Viewport... 0 6 0 4
	Line width... 1
	Font size... 14
	#Font size... 'axes_font_size'
	#Black
	'iPA_color$'
	Plain line
	Times
	Axes... max_F2 min_F2 max_F1 min_F1
	Draw inner box
	Marks bottom... 5 yes yes yes
	Marks left... 5 yes yes yes
	Text left... yes %F_%1 %(%H%e%r%t%z%)
	Text bottom... yes %F_%2 %(%H%e%r%t%z%)
	Axes... -max_F2 -min_F2 -max_F1 -min_F1
endproc

procedure just_ipa_symbol
	Erase all
	#Viewport... 'min_horizontal_range' 'max_horizontal_range' 'min_vertical_range' 'max_vertical_range'
	Viewport... 0 6 0 4
	Line width... 1
	Font size... 14
	##Font size... 'axes_font_size'
	#Black
	'iPA_color$'
	Plain line
	Times
	Axes... max_F2 min_F2 max_F1 min_F1
	Draw inner box
	Marks bottom... 5 yes no no
	Marks left... 5 yes no no
	Text left... yes %F_%1 %(%H%e%r%t%z%)
	Text bottom... yes %F_%2 %(%H%e%r%t%z%)
	Axes... -max_F2 -min_F2 -max_F1 -min_F1
endproc

## calculating formant grand means and st.dev. for all vowels
selectObject: gridname$
numberOfIntervals = Get number of intervals... 'tier'
vowel_counter = 0
sf1 = 0
sf2 = 0
sf1sd = 0
sf2sd = 0

for j from 2 to numberOfIntervals
	selectObject: gridname$
	#select TextGrid 'gridname$'
	label$ = Get label of interval... 'tier' 'j'
  	if label$ == "i"
  		call vowel_plot
  	endif
 endfor
f1gmean_i = sf1 / vowel_counter
f2gmean_i = sf2 / vowel_counter
f1sd_gmean_i = sf1sd / vowel_counter
f2sd_gmean_i = sf2sd / vowel_counter

for j from 2 to numberOfIntervals
	selectObject: gridname$
	#select TextGrid 'gridname$'
	label$ = Get label of interval... 'tier' 'j' 
  	if label$ == "\ic" 
  		call vowel_plot
  	endif
endfor
f1gmean_ic = sf1 / vowel_counter
f2gmean_ic = sf2 / vowel_counter
f1sd_gmean_ic = sf1sd / vowel_counter
f2sd_gmean_ic = sf2sd / vowel_counter

for j from 2 to numberOfIntervals
	selectObject: gridname$
	#select TextGrid 'gridname$'
	label$ = Get label of interval... 'tier' 'j'
  	if label$ == "e"
  		call vowel_plot
  	endif
 endfor
f1gmean_e = sf1 / vowel_counter
f2gmean_e = sf2 / vowel_counter
f1sd_gmean_e = sf1sd / vowel_counter
f2sd_gmean_e = sf2sd / vowel_counter

for j from 2 to numberOfIntervals
	selectObject: gridname$
	#select TextGrid 'gridname$'
	label$ = Get label of interval... 'tier' 'j'
  	if label$ == "\ef"
  		call vowel_plot
  	endif
 endfor
f1gmean_ef = sf1 / vowel_counter
f2gmean_ef = sf2 / vowel_counter
f1sd_gmean_ef = sf1sd / vowel_counter
f2sd_gmean_ef = sf2sd / vowel_counter

for j from 2 to numberOfIntervals
	selectObject: gridname$
	#select TextGrid 'gridname$'
	label$ = Get label of interval... 'tier' 'j'
  	if label$ == "\ae"
  		call vowel_plot
  	endif
 endfor
f1gmean_ae = sf1 / vowel_counter
f2gmean_ae = sf2 / vowel_counter
f1sd_gmean_ae = sf1sd / vowel_counter
f2sd_gmean_ae = sf2sd / vowel_counter

procedure vowel_plot
	vowel_counter = vowel_counter + 1
	start = Get starting point... 'tier' 'j'
	end = Get end point... 'tier' 'j'
  	selectObject: soundname$
	#select Sound 'soundname$'
	Extract part... start end rectangular 1.0 yes
	part_sound$ = selected$("Sound")
	select Sound 'part_sound$'
	To Formant (burg)... 0 'number_of_formants' 5500 0.025 50
	select Formant 'part_sound$'
		f1'vowel_counter' = Get value at time... 1 'midpoint' Hertz Linear
		f2'vowel_counter' = Get value at time... 2 'midpoint' Hertz Linear
		f1mean'vowel_counter' = Get mean: 1, 0, 0, "hertz"
		f2mean'vowel_counter' = Get mean: 2, 0, 0, "hertz"
		f1sd'vowel_counter' = Get standard deviation: 1, 0, 0, "hertz"
		f2sd'vowel_counter' = Get standard deviation: 2, 0, 0, "hertz"
		sf1 = f1mean'vowel_counter' + sf1 
		sf2 = f2mean'vowel_counter' + sf2
		sf1sd = sf1sd + f1sd'vowel_counter'
		sf2sd = sf1sd + f2sd'vowel_counter'
endproc

## calculating confidence intervals (ci) for st.dev. ellipses
f1ci_pos_i = ('f1gmean_i' + 'f1sd_gmean_i')
f1ci_neg_i = ('f1gmean_i' - 'f1sd_gmean_i')
f2ci_pos_i = ('f2gmean_i' + 'f2sd_gmean_i')
f2ci_neg_i = ('f2gmean_i' - 'f2sd_gmean_i')

f1ci_pos_ic = ('f1gmean_ic' + 'f1sd_gmean_ic')
f1ci_neg_ic = ('f1gmean_ic' - 'f1sd_gmean_ic')
f2ci_pos_ic = ('f2gmean_ic' + 'f2sd_gmean_ic')
f2ci_neg_ic = ('f2gmean_ic' - 'f2sd_gmean_ic')

f1ci_pos_ef = ('f1gmean_ef' + 'f1sd_gmean_ef')
f1ci_neg_ef = ('f1gmean_ef' - 'f1sd_gmean_ef')
f2ci_pos_ef = ('f2gmean_ef' + 'f2sd_gmean_ef')
f2ci_neg_ef = ('f2gmean_ef' - 'f2sd_gmean_ef')

f1ci_pos_ae = ('f1gmean_ae' + 'f1sd_gmean_ae')
f1ci_neg_ae = ('f1gmean_ae' - 'f1sd_gmean_ae')
f2ci_pos_ae = ('f2gmean_ae' + 'f2sd_gmean_ae')
f2ci_neg_ae = ('f2gmean_ae' - 'f2sd_gmean_ae')

select all
	minusObject: soundname$
	minusObject: gridname$
Remove

## start plotting
#select TextGrid 'gridname$'
selectObject: gridname$
numberOfIntervals = Get number of intervals... 'tier'

for j from 2 to numberOfIntervals
	selectObject: gridname$
	#select TextGrid 'gridname$'
	label_IPA$ = Get label of interval... 'tier' 'j'
  	if label_IPA$ == "i"
  	Text special... -'f2gmean_i' Centre -'f1gmean_i' Half Times 'iPA_font_size' 0 'label_IPA$'
	Line width: 2.0
		if draw_ellipse = 1
			'ellipse_color$'
			#Red
			Draw ellipse: -'f2ci_neg_i', -'f2ci_pos_i', -'f1ci_neg_i', -'f1ci_pos_i'
		elsif draw_ellipse = 0
		Line width: 1.0
		selectObject: gridname$
		#select TextGrid 'gridname$'
		endif

	elsif label_IPA$ == "\ic"
	Text special... -'f2gmean_ic:2' Centre -'f1gmean_ic:2' Half Times 'iPA_font_size' 0 'label_IPA$'
	Line width: 2.0
		if draw_ellipse = 1
			'ellipse_color$'
			#Red
			Draw ellipse: -'f2ci_neg_ic:2', -'f2ci_pos_ic:2', -'f1ci_neg_ic:2', -'f1ci_pos_ic:2'
		elsif draw_ellipse = 0
		Line width: 1.0
		selectObject: gridname$
		#select TextGrid 'gridname$'
		endif

	elsif label_IPA$ == "\ef"
	Text special... -'f2gmean_ef:2' Centre -'f1gmean_ef:2' Half Times 'iPA_font_size' 0 'label_IPA$'
	Line width: 2.0
		if draw_ellipse = 1
			'ellipse_color$'
			#Red
			Draw ellipse: -'f2ci_neg_ef:2', -'f2ci_pos_ef:2', -'f1ci_neg_ef:2', -'f1ci_pos_ef:2'
		elsif draw_ellipse = 0
		Line width: 1.0
		selectObject: gridname$
		#select TextGrid 'gridname$'
		endif
	
	elsif label_IPA$ == "\ae"
	Text special... -'f2gmean_ae:2' Centre -'f1gmean_ae:2' Half Times 'iPA_font_size' 0 'label_IPA$'
	Line width: 2.0
		if draw_ellipse = 1
			'ellipse_color$'
			#Red
			Draw ellipse: -'f2ci_neg_ae:2', -'f2ci_pos_ae:2', -'f1ci_neg_ae:2', -'f1ci_pos_ae:2'
		elsif draw_ellipse = 0
		Line width: 1.0
		selectObject: gridname$
		#select TextGrid 'gridname$'
		endif
	endif
endfor

Read Table from tab-separated file... 'fileOut$'
Read Table from tab-separated file... 'fileOut2$'