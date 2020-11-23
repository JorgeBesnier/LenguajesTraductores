import ply.lex as lex
import ply.yacc as yacc
import re
import codecs
import os
import numpy as np
from sys import stdin


#identificar en donde se reducen las cosas
def printProduction(p):
	for i in range(len(p)):
		print(p[i], end=" ")
	print()




#definicion para tabla de simbolos de variables simples
tablaSimbolos = [] 
op = []
valores=[]
tipos=[]
avail = 0
tempavail = 0


#saber cuantas varialbes se definieron
contdef = 0

#cosas para el if
salto = []
saltoif = []
saltowhile = []
#lista de modulos
modulos = []
saltoModule = []
returnIns = []

#identificar donde incicia la definicion de variables
varstart = 0

#para realizar el print
generalPrint = []
printlist = []

#para realizar hacer el codigo ejectuble
pila = {}
pilaMat = {}

#token del legunaje de programacion
tokens = [
	'INT',		#listo
	'FLOAT',	#listo
	'NULL',		#listo
	'UINPUT',	#listo
	'PRINT', 	#listo
	'COMIIZQ', 	#listo
	'COMIDER', 	#listo
	'IF',		#listo
	'ELSE',		#listo
    'ELSEIF',		#listo
	'WHILE',	#listo
	'DosPuntos',#listo
	'DO', 		#listo
	'FAIL',		#listo
	'CONTINUE', #listo
	#'UPDATE',	#listo
	'FOR',		#listo
	'DEF',		#listo
	'CALL',		#listo
	'BEGIN',	#listo
	'RETURN', 	#listo
	'PARIZQ', 	#listo
	'PARDER', 	#listo
	#'EXP', 		#listo
	'MUL', 		#listo
	'DIV', 		#listo
	'ADD', 		#listo
	'EQUAL', 	#listo
	'LESS', 	#listo
	'GREATER', 	#listo
	'EQUALEQUAL', 	#listo
	'EQUALLESS', 	#listo
	'EQUALGREATER', #listo
	'DIFEQUAL', #listo
	'SUB', 		#listo 
	'NOT',		#listo
	'AND',		#listo
	'OR',		#listo
	'COMMENT', 	#listo
	'NOM',		#listo 
	'VAR',		#lsito
	'MAT',		#listo
	'COMA',		#listo
	'END',		#listo
	'ESCRIBIR'
]

t_ignore = '\t\r'

#definicion de los tokens del leguaje de programacion
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

##########################################################################################
def t_UINPUT(t):
	r'input'
	t.type = 'UINPUT'
	#print("input")
	return t

#palabra reservado	
def t_MAT(t):
	r'mat'
	t.type = 'MAT'
	#print("MAT")
	return t

#el valor de regreso tiene que ser float x.x
def t_FLOAT(t):
	r'\d+\.\d+'
	t.value = float(t.value)
	#print("float")
	tipos.append("FLOAT")
	return t

#el valor de regreso tiene que ser de tipo int
def t_INT(t):
	r'\d+'
	t.value = int(t.value)
	#print("int")
	tipos.append("INT")
	return t


 #########################################################################################

def t_PRINT(t):
	r'print'
	t.type = 'PRINT'
	#print("print")
	return t

#palabra reservada
def t_IF(t):
	r'if'
	t.type = 'IF'
	#print("if")
	return t

#palabra reservada
def t_ELSE(t):
	r'else'
	t.type = 'ELSE'
	#print("else")
	return t

#palabra reservada
def t_ELSEIF(t):
	r'elif'
	t.type = 'ELSEIF'
	#print("elseif")
	return t


#palabra reservada
def t_WHILE(t):
	r'while'
	t.type = 'WHILE'
	#print("while")
	return t

#palabra reservada
def t_FOR(t):
	r'for'
	t.type = 'FOR'
	#print("for")
	return t

#palabra reservada
def t_DEF(t):
	r'def'
	t.type = 'DEF'
	#print("def")
	return t

#palabra reservada
def t_CALL(t):
	r'call'
	t.type = 'CALL'
	#print("call")
	return t

#palabra reservada
def t_BEGIN(t):
	r'begin'
	t.type = 'BEGIN'
	#print("begin")
	return t

#palabra reservada
def t_NOT(t):
	r'not'
	t.type = 'NOT'
	#print("not")
	return t

#palabra reservada
def t_AND(t):
	r'and'
	t.type = 'AND'
	#print("and")
	return t

#palabra reservada
def t_OR(t):
	r'or'
	t.type = 'OR'
	#print("or")
	return t

#palabra reservada
def t_END(t):
	r'end'
	t.type = 'END'
	#print("end")
	return t

#simbolo reservado
def t_COMIIZQ(t):
	r'“'
	t.type = 'COMIIZQ'
	#print("“")
	return t

#simbolo reservado
def t_COMIDER(t):
	r'p'
	t.type = 'COMIDER'
	#print("”")
	return t

#simbolo reservado
def t_DosPuntos(t):
	r':'
	t.type = 'DosPuntos'
	#print(":")
	return t

#palabra reservada
def t_DO(t):
	r'do'
	t.type = 'DO'
	#print("do")
	return t

#palabra reservada
def t_FAIL(t):
	r'fail'
	t.type = 'FAIL'
	#print("fail")
	return t

#palabra reservada
def t_CONTINUE(t):
	r'continue'
	t.type = 'CONTINUE'
	#print("continue")
	return t

def t_RETURN(t):
	r'return'
	t.type = 'RETURN'
	#print("return")
	return t

#palabra reservada	
def t_COMMENT(t):
	r'//'
	t.type = 'COMMENT'
	#print("//")
	return t

#simbolo reservado
def t_PARIZQ(t):
	r'\('
	t.type = 'PARIZQ'
	#print("(")
	return t

#simbolo reservado
def t_PARDER(t):
	r'\)'
	t.type = 'PARDER'
	#print(")")
	return t

#simbolo reservado
def t_MUL(t):
	r'\*'
	t.type = 'MUL'
	#print("*")
	return t

#simbolo reservado	
def t_DIV(t):
	r'/'
	t.type = 'DIV'
	#print("/")
	return t

#simbolo reservado
def t_ADD(t):
	r'\+'
	t.type = 'ADD'
	#print("+")
	return t

def t_EQUALEQUAL(t):
	r'==='
	t.type = 'EQUALEQUAL'
	#print("==")
	return t

def t_DIFEQUAL(t):
	r'!='
	t.type = 'DIFEQUAL'
	#print("==")
	return t

#simbolo reservado
def t_EQUALLESS(t):
	r'=<'
	t.type = 'EQUALLESS'
	#print("=<")
	return t

#simbolo reservado
def t_EQUALGREATER(t):
	r'=>'
	t.type = 'EQUALGREATER'
	#print("=>")
	return t

#simbolo reservado
def t_EQUAL(t):
	r'='
	t.type = 'EQUAL'
	#print("=")
	return t

#simbolo reservado
def t_LESS(t):
	r'<'
	t.type = 'LESS'
	#print("<")
	return t

#simbolo reservado
def t_GREATER(t):
	r'>'
	t.type = 'GREATER'
	#print(">")
	return t

#simbolo reservado	
def t_SUB(t):
	r'-'
	t.type = 'SUB'
	#print("-")
	return t

#palabra reservado	
def t_VAR(t):
	r'var'
	t.type = 'VAR'
	#print("var")
	return t

#palabra reservado	
def t_COMA(t):
	r','
	t.type = 'COMA'
	#print(",")
	return t

def t_NULL(t):
	r'NULL'
	t.type = 'NULL'
	#print("null")
	tipos.append("NULL")
	return t

def t_ESCRIBIR(t):
	r'escribir'
	t.type = 'ESCRIBIR'
	#print("null")
	return t

#clases de caracteres para definir que es lo que se acepta como nombre valido
def t_NOM(t):
	r'[a-zA-Z][a-zA-Z_0-9]*'
	t.type = 'NOM'
	#print("nom")
	return t

#definicion de condicion de error de sintaxis
def t_error(t):
	#print("Illegal characters!")
	t.lexer.skip(1)

lexer = lex.lex()

#lexico del lenguaje de programacion
###############################################
###############################################
###############################################

def p_S(p):
	'''
	S : M AUXVAR V I
		| V I
		|
	'''
	print("##############################################")
	print("SINTAXIS CORRECTA")	

def p_AUXVAR(p):
	'''
	AUXVAR :
	'''	
	global varstart, tablaSimbolos
	varstart = len(tablaSimbolos)
	#print(varstart)
	
def p_M(p):
	'''
	M : MAUX M 
		|
	'''

def p_MAUX(p):
	'''
	MAUX : DEF NOM PARIZQ PARDER DosPuntos MAUX2 CODE RETURN
	'''
	global avail, tablaSimbolos, tempavail, salto, modulos
	s = saltoModule.pop()
	aux = [p[2], s]
	modulos.append(aux)
	aux2 = ["returnto"]
	tablaSimbolos.append(aux2)
	tempavail = len(tablaSimbolos)
	returnIns.append(tempavail-1)
	
def p_MAUX2(p):
	'''
	MAUX2 : 
	'''
	global avail, tablaSimbolos, tempavail, salto, modulos,saltoModule
	tempavail = len(tablaSimbolos)
	saltoModule.append(tempavail)
	#print("entro aqui2", saltoModule)

def p_V(p):
	'''
	V : VAR PARIZQ PARDER DosPuntos T RETURN 
	'''

def p_T2(p):
	'''
	T2 :	NOM EQUAL MAT PARIZQ VALOR2 COMA VALOR2 COMA VALOR2 PARDER
		|	NOM EQUAL VALOR 
	'''	
	global tablaSimbolos, valores, tipos, contdef
	#printProduction(p)
	#codigo intermedio
	#valores se pone en vacio para generar lista de listas
	valores = []
	if len(p) > 7:
		#print (tipos)
		valores.append(p[1])
		valores.append(p[3])
		valores.append(p[5])
		valores.append(p[7])
		valores.append(p[9])
		duplicado = 0
		conttabla = 0	
		for l in tablaSimbolos:
			if l[0] == valores[0]:
				print("aqui", l)
				tablaSimbolos[conttabla] = valores
				print("aqui", l)
				duplicado = 1
			conttabla+=1
		if duplicado == 0:
			tablaSimbolos.append(valores)
			contdef = contdef + 1
		if type(p[5]) == int: 
			tipos.pop()
		if type(p[7]) == int: 
			tipos.pop()
		if type(p[9]) == int: 
			tipos.pop()
		

	elif len(p) == 4:
		valores.append(p[1])
		valores.append(tipos[-1])
		valores.append(p[3])
		#print("aqui", p[3])
		#verifcar que no existe variable con eedse nombre
		duplicado = 0
		conttabla = 0
		for l in tablaSimbolos:
			if l[0] == valores[0]:
				print("aqui", l)
				tablaSimbolos[conttabla] = valores
				print("aqui", l)
				duplicado = 1
			conttabla+=1
		if duplicado == 0:
			tablaSimbolos.append(valores)
			contdef = contdef + 1
		tipos.pop()
	valores = []

def p_T(p):
	'''
	T :		T2 T
		|
	'''	
	global tablaSimbolos, valores, tipos, contdef

	#printProduction(p)
	#codigo intermedio
	#valores se pone en vacio para generar lista de listas
	
def p_VALOR(p):
	'''
	VALOR :   INT
		    | FLOAT
		    | NULL
		    | NOM
			| NOM PARIZQ VALOR2 COMA VALOR2 COMA VALOR2 PARDER
	'''
	
	if len(p) == 2:
		p[0] = p[1]
	if len(p) == 9:
		aux = [p[1], p[3], p[5], p[7]]
		p[0] = aux	
		print(p[0])

def p_I(p):
	'''
	I : BEGIN DosPuntos CODE END
	'''
	tablaSimbolos.append("$")
	
#falta el user comment y el print case	
def p_CODE(p):
	'''
	CODE : 	  IFCASE CODE 
		    | FORCASE CODE
			| WHILECASE CODE
			| INPUTCASE CODE
			| PRINTCASE CODE
			| USERCOM CODE 
			| CALLCASE CODE
			| ASIG CODE 
			| ASIGMAT CODE 
			| IMPRIMIR CODE
			|
	'''

def p_IMPRIMIR(p):
	'''
	IMPRIMIR : ESCRIBIR DosPuntos VALOR 
	'''
	#print("estamos imprimiendo", len(p))
	if len(p) > 3:
		aux = [p[1], p[3]]
		#print("aqui esta aux", aux)
		tablaSimbolos.append(aux)
		
######POSIBLE ERROR AQUI se corrigo que se editara valor incial de las varaible 
def p_ASIG(p):
	'''
	ASIG : NOM EQUAL ARITH
	'''
	global avail, tablaSimbolos, varstart, contdef, op, valores
	#print(varstart,contdef)
	if len(p) == 4:
		#print("aqui", p[1], valores[-1])
		#print(tablaSimbolos[0:contdef])
		#for l in tablaSimbolos[varstart:contdef]:
		#	if l[0] == p[1]:
				#print("aqui")
		#		l[2] = valores[-1]
		valores.append(p[1])
		op.append(p[2])
		#print(avail)
		aux = [op[-1], valores[-2] , valores[-1]]
		#print("aux",aux)
		op.pop()
		valores.pop()
		valores.pop()
		tablaSimbolos.append(aux)

def p_ASIGMAT(p):
	'''
	ASIGMAT : NOM PARIZQ VALOR2 COMA VALOR2 COMA VALOR2 PARDER EQUAL ARITH
	'''
	global avail, tablaSimbolos, varstart, contdef, op, valores
	if len(p) == 11:
		aux = [p[1], p[3], p[5], p[7]]
		valores.append(aux)
		op.append(p[9])
		aux = [op[-1], valores[-2] , valores[-1]]
		op.pop()
		valores.pop()
		valores.pop()
		tablaSimbolos.append(aux)

def p_VALOR2(p):
	'''
	VALOR2 :  INT
		    | NOM
	'''
	if len(p) > 1:
		p[0] = p[1]


def p_IFCASE(p):
	'''
	IFCASE : IF COND AUXIF DosPuntos CODE END ELSECASE ELSECASE2 AUXIF3 
	'''

#generar el go to falso despues de hacer el cuadruplo de 
def p_AUXIF(p):
	'''
	AUXIF : 
	'''
	global avail, tablaSimbolos, tempavail, saltoif, valores
	tempavail = len(tablaSimbolos)
	#print(tempavail)
	R = valores.pop()
	gif = ["gotof", R]
	tablaSimbolos.append(gif)
	tempavail+=1
	saltoif.append(tempavail-1)

#al entrar el else generar los gotos para saber a donde saltar al terminar la condicion donde es verdadera
def p_AUXIF2(p):
	'''
	AUXIF2 : 
	'''
	global avail, saltoif, tablaSimbolos, tempavail
	tempavail = len(tablaSimbolos)
	#print(tempavail)
	D = saltoif.pop()
	gif = ["goto"]
	tablaSimbolos.append(gif)
	tempavail+=1
	saltoif.append(tempavail-1)
	tablaSimbolos[D].append(tempavail)
	
#rellenar al salir de if todos los gotos
def p_AUXIF3(p):
	'''
	AUXIF3 : 
	'''
	global avail, tablaSimbolos, tempavail,saltoif
	tempavail = len(tablaSimbolos)
	#print(tempavail)

	while saltoif:
		D = saltoif.pop()
		#print(salto)
		tablaSimbolos[D].append(tempavail)

def p_ELSECASE(p):
	'''
	ELSECASE : 	ELSEIF AUXIF2 COND AUXIF DosPuntos CODE END ELSECASE
				| 
	'''

def p_ELSECASE2(p):
	'''
	ELSECASE2 :  ELSE AUXIF2 DosPuntos CODE END
				| 
	'''

def p_FORCASE(p):
	'''
	FORCASE : FOR PARIZQ ASIG DosPuntos AUXFOR1 COND DosPuntos AUXFOR2 ASIG PARDER DosPuntos AUXFOR3 CODE END AUXFOR4
	'''

def p_AUXFOR1(p):
	'''
	AUXFOR1 :
	'''
	global avail, salto, tablaSimbolos, tempavail
	tempavail = len(tablaSimbolos)
	salto.append(tempavail)

def p_AUXFOR2(p):
	'''
	AUXFOR2 :
	'''
	global avail, salto, tablaSimbolos, tempavail, valores
	tempavail = len(tablaSimbolos)
	R = valores.pop()
	gif = ["gotof", R]
	tablaSimbolos.append(gif)
	gif = ["goto"]
	tablaSimbolos.append(gif)
	tempavail+=1
	salto.append(tempavail-1)
	salto.append(tempavail)

def p_AUXFOR3(p):
	'''
	AUXFOR3 :
	'''
	global avail, salto, tablaSimbolos, tempavail
	tempavail = len(tablaSimbolos)
	salto.append(tempavail+1)
	l4 = salto.pop()
	l3 = salto.pop()
	l2 = salto.pop()
	l1 = salto.pop()
	tempavail = len(tablaSimbolos)
	gif = ["goto", l1]
	tablaSimbolos.append(gif)
	tempavail+=1
	salto.append(l2)
	salto.append(l3)
	salto.append(l4)
	
def p_AUXFOR4(p):
	'''
	AUXFOR4 :
	'''
	global avail, salto, tablaSimbolos, tempavail
	#print("aqui aux", salto)
	l3 = salto.pop()
	l2 = salto.pop()
	tablaSimbolos[l2].append(l3)
	l1 = salto.pop() #llena el goto falso
	tempavail = len(tablaSimbolos)
	gif = ["goto", l2+1]
	tablaSimbolos.append(gif)
	tablaSimbolos[l1].append(tempavail+1)

def p_WHILECASE(p):
	'''
	WHILECASE : WHILE AUXW1 COND DosPuntos AUXW2 CODE END AUXW3
	'''

def p_AUXW1(p):
	'''
	AUXW1 :
	'''
	global avail, saltowhile, tablaSimbolos, tempavail
	tempavail = len(tablaSimbolos)
	saltowhile.append(tempavail)
	#print("aqui", salto)

def p_AUXW2(p):
	'''
	AUXW2 :
	'''
	global avail, saltowhile, tablaSimbolos, tempavail, valores
	tempavail = len(tablaSimbolos)
	R = valores.pop()
	gif = ["gotof", R]
	tablaSimbolos.append(gif)
	tempavail+=1
	saltowhile.append(tempavail-1)

def p_AUXW3(p):
	'''
	AUXW3 :
	'''
	global avail, saltowhile, tablaSimbolos, tempavail
	tempavail = len(tablaSimbolos)
	f = saltowhile.pop()
	retorno = saltowhile.pop()
	gotoReturn = ["goto", retorno]
	tablaSimbolos.append(gotoReturn)
	tablaSimbolos[f].append(tempavail+1)

def p_COND(p):
	'''
	COND :    ARITH
			| ARITH OPREL ARITH
	'''
	#print("Ëntro por aca \n")
	#printProduction(p)
	global tablaSimbolos, valores, tipos, contdef, avail, op
	if len(p) > 2:
		#printProduction(p)
		op.append(p[2])		
		aux = [op[-1], valores[-2] , valores[-1], "T"+str(avail)] #incluir el AVAIL'
		avail +=1
		#print(aux)
		op.pop()
		valores.pop()
		valores.pop()
		tablaSimbolos.append(aux)
		valores.append(tablaSimbolos[-1][3])

def p_OPREL(p):
	'''
	OPREL :   EQUALEQUAL
			| DIFEQUAL
			| EQUALLESS	
			| EQUALGREATER
			| EQUAL
			| LESS	
			| GREATER
	'''
	if len(p) > 1:
		p[0] = p[1]

def p_ARITH(p):  
	'''
	ARITH :   ARITH ADD MULLDIV
			| ARITH SUB MULLDIV
			| ARITH OR MULLDIV		
			| MULLDIV
	'''
	global avail, op, valores, tablaSimbolos
	if len(p) > 2:
		op.append(p[2])		
		aux = [op[-1], valores[-2] , valores[-1], "T"+str(avail)] #incluir el AVAIL'
		avail +=1
		#print(aux)
		op.pop()
		valores.pop()
		valores.pop()
		tablaSimbolos.append(aux)
		valores.append(tablaSimbolos[-1][3])
	else:
		p[0] = p[1] #creo que esto no sirve para nada	

def p_MULLDIV(p):
	'''
	MULLDIV :  MULLDIV MUL NOTL
			 | MULLDIV DIV NOTL
			 | MULLDIV AND NOTL
			 | NOTL
	'''
	global avail, op, valores, tablaSimbolos
	if len(p) > 3: ##quiza es un dos...
		op.append(p[2])	
		aux = [op[-1], valores[-2] , valores[-1], "T"+str(avail)] 
		avail +=1
		op.pop()
		valores.pop()
		valores.pop()
		tablaSimbolos.append(aux)
		valores.append(tablaSimbolos[-1][3])
	else:
		p[0] = p[1] #creo que esto no sirve para nada

def p_NOTL(p):
	'''
	NOTL : 	   NOT F
			 | F
	'''	
	global avail, op, valores, tablaSimbolos
	if len(p) > 2:
		#printProduction(p)
		op.append(p[1])
		aux = [op[-1], valores[-1], "T"+str(avail)]
		avail +=1
		op.pop()
		valores.pop()
		tablaSimbolos.append(aux)
		valores.append(tablaSimbolos[-1][2])
	else:
		p[0] = p[1] #creo que esto no sirve para nada

def p_F(p):
	'''
	F : 	   VALOR
			 | PARIZQ COND PARDER
			 | PARIZQ  PARDER			 
	'''
	global valores
	if len(p) == 2:
		valores.append(p[1]) 
			
def p_INPUTCASE(p):
	'''
	INPUTCASE : NOM EQUAL UINPUT PARIZQ  VALOR  PARDER
				| NOM PARIZQ VALOR2 COMA VALOR2 COMA VALOR2 PARDER EQUAL UINPUT PARIZQ  VALOR  PARDER
	'''
	global tablaSimbolos, printlist
	if len(p) == 7:
		gif = ["escribir", p[5]]
		tablaSimbolos.append(gif)
		printlist = []
		gif = [p[3], p[1]]
		tablaSimbolos.append(gif)
	if len(p) == 14:
		gif = ["escribir", p[12]]
		tablaSimbolos.append(gif)
		printlist = []
		aux = [p[1], p[3], p[5], p[7]] 
		gif = [p[10], aux]
		tablaSimbolos.append(gif)

def p_PRINTCASE(p):
	'''
	PRINTCASE : PRINT PARIZQ PRINTOP PARDER
	'''
	global generalPrint, tablaSimbolos
	if len(p) == 5:
		#print("aqui mero",generalPrint)
		aux = ["printg", generalPrint]
		tablaSimbolos.append(aux)
		generalPrint = []

def p_PRINTOP(p):
	'''
	PRINTOP :	 AUXPRINT  
				| AUXPRINT DosPuntos PRINTOP 	
				| 
	'''

def p_AUXPRINT(p):
	'''
	AUXPRINT :	  NOM 
				| PARIZQ TEXT PARDER
	'''
	global printlist, generalPrint
	printProduction(p)
	if len(p) == 2:
		aux = [p[1]]
		generalPrint.append(aux)
	elif len(p) == 4:
		aux = [printlist]
		generalPrint.append(printlist)
		printlist = []

def p_TEXT(p):
	'''
	TEXT : NOM TEXT
			|
	'''
	global printlist
	if len(p) == 3:
		gif = [p[1]]
		printlist.append(p[1])
	
#no se considera para tabla de simbolos
def p_USERCOM(p):
	'''
	USERCOM : COMMENT 
	'''

def p_CALLCASE(p):
	'''
	CALLCASE : CALL NOM
	'''
	global avail, salto, tablaSimbolos, tempavail, returnIns, modulos
	for l in modulos:
			if l[0] == p[2]:
				#print("aqui")
				gotomodule = l[-1]
	aux = ["call", gotomodule]
	tablaSimbolos.append(aux)
	#l1 = returnIns.pop(0)
	tempavail = len(tablaSimbolos)
	#print(tempavail, "aquimerp")
	#tablaSimbolos[l1].append(tempavail)

def p_error(p):
	print("Error de Sintaxis")
	print("Error en la linea: "+str(p.lineno), ", columna: "+str(p.lexpos))

parser = yacc.yacc()

print("##############################################")
#input
while True:
	try:
		s = input('')
	except EOFError:
		break
	parser.parse(s)
	#print (valores)
	#print (tablaSimbolos)
	print("##############################################")
	print("TABLA DE SIMBOLOS/CUADRUPLOS")
	i = 0
	# Print the rows
	for item in tablaSimbolos:
		print(i, " ", *item)
		i += 1
	print("##############################################")
	print("DIR DE MODULOS DEFINIDOS")
	i = 0
	for item in modulos:
		print(i, " ", *item)
		i += 1
	print("##############################################")
	#print(tablaSimbolos)
	print("VALORES RESTANTES: ", valores)
	print("OPERADORES RESTANTES: ", op)
	print("SALTOS RESTANTES: ", salto)
	print("Variables Definidas: ", contdef)
	print("Start de Variables: ", varstart)
	
	dicMatSize = {}
	auxrange = varstart + contdef
	for i in range(varstart, auxrange):
		if tablaSimbolos[i][1] != "mat":
			pila[tablaSimbolos[i][0]] = tablaSimbolos[i][2]
		else:
			nombre = tablaSimbolos[i][0]
			if type (tablaSimbolos[i][2]) == str:
				z = pila[tablaSimbolos[i][2]]
			else:
				z = tablaSimbolos[i][2]
			if type (tablaSimbolos[i][3]) == str:
				y = pila[tablaSimbolos[i][3]]
			else:
				y = tablaSimbolos[i][3]
			if type (tablaSimbolos[i][4]) == str:
				x = pila[tablaSimbolos[i][4]]
			else:
				x = tablaSimbolos[i][4]

			mat = [0] * x
			auxsize = 1
			if (y != 0):
				auxsize += 1
				for ii, _ in enumerate(mat):
					mat[ii] = [0] * y
			if (z != 0):
				auxsize += 1
				for ii, _ in enumerate(mat):
					for j, _ in enumerate(mat[ii]):
						mat[ii][j] = [0] * z		
			pila[nombre] = mat
			dicMatSize[nombre] = auxsize


	#ejecucion del codig
	PC = varstart + contdef
	i = len(tablaSimbolos)
	#print(PC,i)
	#aqui inicia la ejecucion de la tabla de simbolos
	pilaretorno = [] 
	print("aqui mero", tablaSimbolos[0][1])
	while (PC < i):
		#call
		if (tablaSimbolos[PC][0]=='call'):
			aux = PC+1
			#print("aqui esta aux", aux)
			pilaretorno.append(aux)
			PC = tablaSimbolos[PC][1]
		elif (tablaSimbolos[PC][0]=='returnto'):
			PC = pilaretorno.pop()
			#print("aqui esta valor de retorno", PC)

		#aritmeticas e igual
		elif (tablaSimbolos[PC][0]=='+'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			#print("pila0", pila)
			#print("op10", op1)
			#print("op20", op2)
			resultado = op1+op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1
		elif (tablaSimbolos[PC][0]=='-'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			resultado = op1-op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1
		elif (tablaSimbolos[PC][0]=='*'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			resultado = op1*op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1			
		elif (tablaSimbolos[PC][0]=='/'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			if type(op1) == int and type(op2) == int:
				resultado = op1/op2
				pila[tablaSimbolos[PC][3]] = int(resultado)
			else:
				resultado = op1/op2
				pila[tablaSimbolos[PC][3]] = resultado
			PC+=1	
		elif (tablaSimbolos[PC][0]=='='):
			if (type(tablaSimbolos[PC][1]) == int or type(tablaSimbolos[PC][1]) == float) and type(tablaSimbolos[PC][2]) != list:
				pila[tablaSimbolos[PC][2]] = tablaSimbolos[PC][1]
				#print("esta mal")
			elif type(tablaSimbolos[PC][1]) == str  and type(tablaSimbolos[PC][2]) == list:
				#print("hola", tablaSimbolos[PC][2])
				aux = tablaSimbolos[PC][2]
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					auxpila[num3]= pila[tablaSimbolos[PC][1]]
				elif arraysize == 2:
					auxpila[num2][num3]= pila[tablaSimbolos[PC][1]]
				else:
					auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			elif type(tablaSimbolos[PC][2]) == str  and type(tablaSimbolos[PC][1]) == list:
				#print("hola", tablaSimbolos[PC][2])
				aux = tablaSimbolos[PC][1]
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print("auxpila", auxpila[0][1])
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					pila[tablaSimbolos[PC][2]] = auxpila[num3]
				elif arraysize == 2:
					pila[tablaSimbolos[PC][2]] = auxpila[num2][num3]
				else:
					pila[tablaSimbolos[PC][2]] = auxpila[num1][num2][num3]
			elif (type(tablaSimbolos[PC][1]) == int or type(tablaSimbolos[PC][1]) == float) and type(tablaSimbolos[PC][2]) == list:
				#print("hola", tablaSimbolos[PC][2])
				aux = tablaSimbolos[PC][2]
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					auxpila[num3]= tablaSimbolos[PC][1]
				elif arraysize == 2:
					auxpila[num2][num3]= tablaSimbolos[PC][1]
				else:
					auxpila[num1][num2][num3]= tablaSimbolos[PC][1]
			else:
				pila[tablaSimbolos[PC][2]] = pila[tablaSimbolos[PC][1]]
			PC+=1	

		#relacionales
		elif (tablaSimbolos[PC][0]=='<'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			#print("pila", pila)
			#print("op1", op1)
			#print("op2", op2)
			resultado = op1 < op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1
		elif (tablaSimbolos[PC][0]=='==='):
			#print("FLORO IGUAL")
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			if op1 == op2:
				resultado = True
			else:
				resultado = False
			#print("pila", pila)
			#print("op1", op1)
			#print("op2", op2)
			#resultado = op1 == op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1
		elif (tablaSimbolos[PC][0]=='>'):
			
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			
			resultado = op1 > op2
			#print("hola", resultado)
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1
		elif (tablaSimbolos[PC][0]=='=<'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			resultado = op1 <= op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1
		elif (tablaSimbolos[PC][0]=='=>'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			resultado = op1 >= op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1	
		elif (tablaSimbolos[PC][0]=='!='):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			resultado = op1 != op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1				

		#and or not
		elif (tablaSimbolos[PC][0]=='and'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			#print("pila", pila)
			#print("op1", op1)
			#print("op2", op2)
			resultado = op1 and op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1		
		elif (tablaSimbolos[PC][0]=='or'):
			op1 = tablaSimbolos[PC][1]
			op2 = tablaSimbolos[PC][2]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op2) == str:
				op2 = pila[op2]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			if type(op2) == list:
				aux = op2
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op2 = auxpila[num3]
				elif arraysize == 2:
					op2 = auxpila[num2][num3]
				else:
					op = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]

			#print("pila", pila)
			#print("op1", op1)
			#print("op2", op2)
			resultado = op1 or op2
			pila[tablaSimbolos[PC][3]] = resultado
			PC+=1		
		elif (tablaSimbolos[PC][0]=='not'):
			op1 = tablaSimbolos[PC][1]
			if type(op1) == str:
				op1 = pila[op1]
			if type(op1) == list:
				aux = op1
				nom = aux[0]
				if type(aux[1])== str:
					num1 = aux[1]
					num1 = pila[num1]
				else: 
					num1 = aux[1]
				if type(aux[2])== str:
					num2 = aux[2]
					num2 = pila[num2]
				else: 
					num2 = aux[2]
				if type(aux[3])== str:
					num3 = aux[3]
					num3 = pila[num3]
				else: 
					num3 = aux[3]
				auxpila = pila[nom]
				#print(auxpila)
				arraysize = dicMatSize[nom]
				if arraysize == 1:
					op1 = auxpila[num3]
				elif arraysize == 2:
					op1 = auxpila[num2][num3]
				else:
					op1 = auxpila[num1][num2][num3]= pila[tablaSimbolos[PC][1]]
			#print("pila", pila)
			#print("op1", op1)
			#print("op2", op2)
			resultado = not op1
			pila[tablaSimbolos[PC][2]] = resultado
			PC+=1	

		#CONDICIONAL IF y Ciclo while/for
		elif (tablaSimbolos[PC][0]=='gotof' and not pila[tablaSimbolos[PC][1]]):
			PC = tablaSimbolos[PC][2]
		elif (tablaSimbolos[PC][0]=='goto'):
			PC = tablaSimbolos[PC][1]		

		#para imprimir cosas
		elif (tablaSimbolos[PC][0]=='print'):
			listaprint = tablaSimbolos[PC][1]
			while listaprint:
				d = listaprint.pop()
				print(d, " ", end='')
			print("")
			PC+=1
		elif (tablaSimbolos[PC][0]=='input'):
			auxinput = input()
			aux2 = "."
			if aux2 in auxinput:
				if type(tablaSimbolos[PC][1]) == str:
					pila[tablaSimbolos[PC][1]] = float(auxinput)
				elif type(tablaSimbolos[PC][1]) == list:
					aux = tablaSimbolos[PC][1]
					nom = aux[0]
					if type(aux[1])== str:
						num1 = aux[1]
						num1 = pila[num1]
					else: 
						num1 = aux[1]
					if type(aux[2])== str:
						num2 = aux[2]
						num2 = pila[num2]
					else: 
						num2 = aux[2]
					if type(aux[3])== str:
						num3 = aux[3]
						num3 = pila[num3]
					else: 
						num3 = aux[3]
					auxpila = pila[nom]
					#print(auxpila)
					arraysize = dicMatSize[nom]
					if arraysize == 1:
						auxpila[num3] = float(auxinput)
					elif arraysize == 2:
						auxpila[num2][num3] = float(auxinput)
					else:
						auxpila[num1][num2][num3] = float(auxinput)

			else:	
				if type(tablaSimbolos[PC][1]) == str:
					pila[tablaSimbolos[PC][1]] = int(auxinput)
				elif type(tablaSimbolos[PC][1]) == list:
					aux = tablaSimbolos[PC][1]
					nom = aux[0]
					if type(aux[1])== str:
						num1 = aux[1]
						num1 = pila[num1]
					else: 
						num1 = aux[1]
					if type(aux[2])== str:
						num2 = aux[2]
						num2 = pila[num2]
					else: 
						num2 = aux[2]
					if type(aux[3])== str:
						num3 = aux[3]
						num3 = pila[num3]
					else: 
						num3 = aux[3]
					auxpila = pila[nom]
					#print(auxpila)
					arraysize = dicMatSize[nom]
					if arraysize == 1:
						auxpila[num3] = int(auxinput)
					elif arraysize == 2:
						auxpila[num2][num3] = int(auxinput)
					else:
						auxpila[num1][num2][num3] = int(auxinput)

			PC+=1
		elif (tablaSimbolos[PC][0]=='printg'):
			listaprint = tablaSimbolos[PC][1]
			#print("aqui esta la lista", listaprint)
			while listaprint:
				d = listaprint.pop(0)
				#print("aqui d", d)
				daux = d[0]
				#print("aqui daux", daux)
				if len(d) == 1 and daux in pila:
					print(pila[daux], " ", end='')
				elif len(d) >= 1:
					#print("aqui esta ddddd", d)
					while d:
						dd = d.pop()
						print(dd, " ", end='')
			print("")
			PC+=1
		
		elif (tablaSimbolos[PC][0]=='escribir'):
			encontrado = True
			aux = tablaSimbolos[PC][1]
			for index in pila:
				if index == aux:
					print(pila[aux])
					encontrado = False
			if encontrado == True:
				print(tablaSimbolos[PC][1])
			print("")
			PC+=1
		#salir del programa
		else:
			PC+=1
	print("LA PILA DE VALORES ES", pila)
