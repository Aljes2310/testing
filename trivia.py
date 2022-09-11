BLACK = '\033[30m'
RED = '\033[31m'
GREEN = '\033[32m'
YELLOW = '\033[33m'
BLUE = '\033[34m'
MAGENTA = '\033[35m'
CYAN = '\033[36m'
WHITE = '\033[37m'
RESET = '\033[39m'

import random
import time

puntaje = 0
iniciar_trivia = True
intentos = 0 


print("Bienvenido a mi trivia sobre", BLUE + "EL MAR PERUANO" + RESET)
print(RED + "\nQue tanto sabes?" + RESET)
nombre = input("\nIngresa tu nombre: ")
print("\nHola", GREEN + nombre + RESET,"responde las siguientes preguntas escribiendo la letra de la alternativa que crees correcta y presionar ENTER ")
  
while iniciar_trivia == True:
  intentos += 1
  puntaje = 0
#print("\nTienes ", puntaje, "puntos")
  print("\n1) El", CYAN + "oceano" + RESET,
      "en el cual el mar peruano se encuentra se llama...? ")
  print("\na)Oceano Atlantico")
  print("b)Oceano Grau")
  print("c)Oceano Pacifico")
  print("d)Oceano indico")
  respuesta_1 = input("\nTu respuesta es? : ").lower()
  time.sleep(1)
  
  while respuesta_1 not in ("a", "b", "c", "d"):
    respuesta_1 = input(
        "Debes responder a, b,c o d. Introduce nuevamente tu respuesta : ")
  if respuesta_1 == "d":
    puntaje += 5
    print(GREEN + "Correcto!" + RESET)
  else:
    puntaje += 0
    print(RED + "Incorrecto!" + RESET)
      
  print("\n2) El mar peruano posee una amplitud de..? ")
  print("\na)20 millas nauticas")
  print("b)200 millas nauticas")
  print("c)100 millas nauticas")
  print("d)NA")
  
  respuesta_2 = input("\nTu respuesta es? :").lower()
  time.sleep(1)
  
  while respuesta_2 not in ("a", "b", "c", "d"):
    respuesta_2 = input(
        "Debes responder a, b,c o d. Introduce nuevamente tu respuesta : ")
  if respuesta_2 == "b":
    puntaje += 5
    print(GREEN + "Correcto!" + RESET)
  else:
    puntaje += 0
    print(RED + "Incorrecto!" + RESET)
      
  print("\n3) Cual es el recurso hidrobiologico mas extraido del mar peruano? ")
  print("\na)Calamar gigante o pota")
  print("b)Anchoveta")
  print("c)Merluza")
  print("d)Jurel")
      
  respuesta_3 = input("\nTu respuesta es? :").lower()
  time.sleep(1)
  
  while respuesta_3 not in ("a", "b", "c", "d"):
    respuesta_3 = input(
        "Debes responder a, b,c o d. Introduce nuevamente tu respuesta : ")
  if respuesta_3 == "b":
    puntaje += 5
    print(GREEN + "Correcto!" + RESET)
  else:
    puntaje += 0
    print(RED + "Incorrecto!" + RESET)
    
  print("\n4)En el mar peruano la temperatura del agua suele ser mayor frente a ....? ")
  print("\na)Tumbes")
  print("b)Pisco")
  print("c)Chimbote")
  print("d)Piura")
  
  respuesta_4 = input("\nTu respuesta es? :").lower()
  time.sleep(1)
  while respuesta_4 not in ("a", "b", "c", "d"):
    respuesta_4 = input(
        "Debes responder a, b,c o d. Introduce nuevamente tu respuesta : ")
  if respuesta_4 == "a":
    puntaje += 5
    print(GREEN + "Correcto!" + RESET)
  else:
    puntaje += 0
    print(RED + "Incorrecto!" + RESET)
    
    
  print("\nGracias por participar", nombre, ",obtuviste ", GREEN + str(puntaje),
      " puntos"+ RESET)
    
  print(MAGENTA+"\n¿Deseas intentar la trivia nuevamente?")
  repetir_trivia = input("Ingresa 'si' para repetir, o cualquier tecla para finalizar: "+RESET).lower()

  print(GREEN+"\nIntento N°:", (intentos+1),""+RESET)

  if repetir_trivia != "si":  # != significa "distinto"
   print("\nEspero {nombre} que lo hayas pasado bien, hasta pronto!")
   iniciar_trivia = False

