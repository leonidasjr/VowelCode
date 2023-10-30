## Script para plotagem maunal de vogias no espaço acústico f2xf1  ##
## 
## Copyright (C) Leônidas Silva Jr., UEPB, 2023. 
##---------------------------------------------------------------------------------##
##----##----##--- COMO CITAR ESTE PROGRAMA ----##----##
## Silva Jr., L. (2023). Vowelplot. [Computer program for Praat], Portuguese version.
#####################################################################################

form Inserir parâmetros
	word Pasta Endereço da pasta
	word Nome_do_estudante EscrevaSeuNome
	comment Escreva os valores de F1/F2 values e o símbolo fonético da vogal (IPA) para plotar
	comment Este é o código do Praat para os símbolos fonéticos (IPA) das vogais do protuguês brasileiro: 
	comment Vogais anteriores: i = [i]; e = [e]; \ef = [ɛ]; 
	comment Vogais centrais: a = [a]; \at = [ɐ]
	comment Vogais posteriores: \ct = [ɔ]; o = [o]; u = [u]
	real F1 
	real F2 
	sentence IPA
	integer Tamanho_da_fonte_IPA 20
	#comment Ajustar eixos F2xF1
	#real Min_F1 200
	#real Max_F1 1200
	#real Min_F2 500
	#real Max_F2 2500
  optionmenu IPA_cor: 1
	option Black
	option Red
	option Green
	option Blue
  boolean Plotar_espaco_vocalico 1
endform

if plotar_espaco_vocalico = 1
	call espaco_vocalico
elsif plotar_espaco_vocalico = 0
	tamanho_da_fonte_IPA
endif
call plot

procedure plot
	'iPA_cor$'
	Text special... -'f2' Centre -'f1' Half Times 
	...'tamanho_da_fonte_IPA' 0 'iPA$'
endproc

procedure espaco_vocalico
	beginPause: "Desmarcando a opção: 'Plotar espaco vocalico'"
	comment: "Se for sua PRIMEIRA vogal, clique em 'Continue'" 
	comment: "Se não, clique em 'Stop' e desmarque a opção: 'Plotar espaco vocalico'"
	endPause: "Continue", 1
	Erase all
	Viewport... 0 6 0 4
	Line width... 1
	Font size... 12
	Times
	'iPA_cor$'
	Plain line
	
	## ajuste os eixos X e Y para F2 e F1 respectivamente  
	#Axes... max_F2 min_F2 max_F1 min_F1
	Axes... 2500 500 1000 200
	Draw inner box
	Marks bottom... 6 yes yes yes
	Marks left... 5 yes yes yes
	Text left... yes %F_1 (Hz)
	Text bottom... yes %F_2 (Hz)
	#Axes... -max_F2 -min_F2 -max_F1 -min_F1
	Axes... -2500 -500 -1000 -200
endproc
Font size... 12

pauseScript: "Salvar figura?"
	Font size... 14
	Text top: "yes", nome_do_estudante$
	Font size... 12
	Save as 600-dpi PNG file: pasta$ + "\" + nome_do_estudante$ + ".jpg"
	writeInfoLine: "Figura " + nome_do_estudante$ + ".jpg" + " foi salva com sucesso"
