## Script for vowel plot procedure                    ##
## and L2 pronunciation teaching puposes              ##
## Copyright (C) Leônidas SILVA JR, UEPB, 2021, 2022. ##
##----##----##----##----##----##----##----##----##----##
##-------------------------------------------------------------##
##----##----##--- HOW TO CITEE THIS PROGRAM ----##----##
## SILVA JR., L. (2022). Vowelplot. [Computer program for Praat].
#################################################################

form Enter formants
	word Folder Folder's path
	word Student_name WriteYourName
	comment Write F1-F2 values, and the IPA vowel to plot
	comment This is the praat code for IPA vowels: 
	comment Front vowels: i = [i]; \ic = [ɪ]; \ef = [ɛ]; e = [e]; \ae = [æ]; 
	comment Central vowels: \sw = [ə]; \vt = [ʌ]; 
	comment Back vowels: \as = [ɑ]; \ab = [ɒ]; \ct = [ɔ]; o = [o]; \hs = ʊ; u = [u]
	real F1 900
	real F2 2300
	sentence IPA \ae
	integer IPA_font_size 20
	comment Set up X-axis (for F2), and Y-axis (for F1)
	real Min_F1 200
	real Max_F1 1200
	real Min_F2 500
	real Max_F2 3000
  optionmenu IPA_colour: 1
	option Black
	option Red
	option Green
	option Blue
  boolean Plot_vowel_space 1
endform

if plot_vowel_space = 1
	call grid
elsif plot_vowel_space = 0
	iPA_font_size
endif
call plot

procedure plot
	'iPA_colour$'
	Text special... -'f2:2' Centre -'f1:2' Half Times 
	...'iPA_font_size' 0 'iPA$'
endproc

procedure grid
	Erase all
	Viewport... 0 6 0 4
	Line width... 1
	Font size... 12
	Times
	'iPA_colour$'
	Plain line
	
	## set up X- and Y-axis for F2 and F1 respectively  
	Axes... max_F2 min_F2 max_F1 min_F1
	Draw inner box
	Marks bottom... 6 yes yes yes
	Marks left... 5 yes yes yes
	Text left... yes %F_1 (Hz)
	Text bottom... yes %F_2 (Hz)
	Axes... -max_F2 -min_F2 -max_F1 -min_F1
endproc
Font size... 12

pauseScript: "Save picture?"
	Font size... 14
	Text top: "yes", student_name$
	Font size... 12
	Save as 600-dpi PNG file: folder$ + "\" + student_name$ + ".jpg"
	writeInfoLine: "Picture " + student_name$ + ".jpg" + " saved successfully"


