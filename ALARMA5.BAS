' VERSION 5 validacion de hora de alarmas con 0 termina el programa
' con cualqueircosa que no sea valida continua, acepta formatos validos
'  las alarmas grabadas en ALARMAS.TXT se editan en un notepad
' antes de agregar una nueva en el programa; Con Notepad se podra modificar 
' y/o agregar nuevas sin usar el programa. Pero si se modifica con notepad 
' debera volver a arrancar el programita para que lea las alarmas del archivo modificado.

#INCLUDE "WINDOW9.BI"
'#Include once "win/winuser.bi"
#include "string.bi"
#include "DATETIME.bi"
#include "file.bi"
#include "fbthread.bi"
#include "vbcompat.bi"
#include "crt/ctype.bi"
 
'#Include "win/winbase.bi"
'''declare function GetProcessIdOfThread(byval Thread as HANDLE) as UInteger
' AUTOR JOSE GALEANO 14-12-2025 COMO HOBBY Y HERRAMIENTA DE TRABAJAO EN PC 
' fix Alarma4 18-12-2025 despues de un mensaje de intervalo solia perder el focus
Sub retardo()
 Dim As Integer d1=0
   Do
     d1=d1+1 
     Sleep 1000
     If d1=8 Then
        Exit Do
     EndIf    
  Loop   

End Sub
Sub sub1 (MENSAJE As string)
MessBox("MENSAJE DE INTERVALO " ,UCase (MENSAJE)+" "+Time,MB_TOPMOST + MB_SETFOREGROUND)
End Sub

Sub sub2( mensaje As String)
MessBox("MENSAJE DE ALARMA " , UCase(mensaje) +" "+Time,MB_TOPMOST + MB_SETFOREGROUND)
End Sub

Sub SUB_notepad()
 Dim As String windir=Environ("windir")
 Var P= Exec (windir+"\notepad.EXE",".\ALARMAS.TXT") 'HAY QUE IR CERRANDO EL NOTEPAD

' OTRA FORMA SIN NOTEPAD
' Dim As String MENSAJE,BUFFER
'Open "ALARMAS.TXT" For Input As 8
'Do While Not Eof(8)
'Line Input #8, MENSAJE
'BUFFER=BUFFER+MENSAJE+Chr(13)+Chr(10)
'Loop 
'Close 8
'MessBox("MENSAJE DE ALARMA GUARDADOS " , BUFFER )
'----------AMBAS FORMAS NECESITAN LA INTERVENCION DEL USUARIO PARA CERRAR
' LA VENTANA DE LAS ALARMAS MOSTRADAS, NOTEPAD Y MESSBOX NECESITAN CERRARSE
' FLATA HACER UNA VENTANA QUE MUESTRE Y SE MATE SOLA OTRO OPEN WINDOW O UN 
' GADGET DE LISTBOX! 
End Sub

Sub sub_HORA (ByRef HORA As String,default As String,hwnd As hwnd)
 
 HORA=InputBox("CONFIG ALARMAS FIJAS O SALE" ,"HORA EN FORMATO 00:00:00, CON 0 TERMINA", default, ES_MULTILINE + ES_AUTOVSCROLL,hwnd )
End Sub

Sub submensaje3(ByRef MENSAJE3 As String,HWND As hwnd)
MENSAJE3=InputBox("MENSAJE DE ALARMA UNICA " ,"MENSAJE A ", "", ES_MULTILINE + ES_AUTOVSCROLL,hwnd )
End Sub
Function diaw(result As Long) As String
Select Case result
       Case 1
       diaw ="Do"
       Case 2
       diaw ="Lu"
       Case 3
       diaw ="Ma"
       Case 4
       diaw ="Mi"
       Case 5
       diaw ="Ju"
       Case 6
       diaw ="Vi"
       Case 7
       diaw ="Sa"
End Select

End Function 

Function esnumerico(esa As string) As Integer
'retorna 1 si no es numerico sino retorna 0
Dim a(10) As Integer
Dim As Integer b,i

For i=1 To Len (esa)
a(i)=Val(Mid(esa,i,1))
If a(i) = 0 And Mid(esa,i,1) <> "0" Then

  esnumerico=1
Else

  esnumerico=0
EndIf
Next i

If esa="" Then
esnumerico=1
EndIf

End Function

Function entradavalida(esa As string) As Integer
' entrada valida 00:00:00  ó 01:20:18  
' Si 3 y 6 no numericas =":"
' y  1 2 4 5 7 8 numericos  -> retorna 0 sino retorna 1 no valido

Dim As Integer i
Dim  As String * 8 resultadovalido="00100100", resultado="        "

If Len(Trim(esa)) > 8 Then
  entradavalida=1  ' no es valida len mayor a 8
EndIf

Dim a(8) As Integer

For i=1 To 8
a(i)=Val(Mid(esa,i,1)) 
If a(i) = 0 And Mid(esa,i,1) <> "0" Then
  If Mid(esa,i,1)=":" Then
    Mid(resultado,i,1)="1"
  Else
    Mid(resultado,i,1)="2"  
  EndIf
Else
  Mid(resultado,i,1)="0"
EndIf

next i

If resultado=resultadovalido Then
   entradavalida=0
Else
   entradavalida=1
EndIf

End Function
'///////////////////////////////////////////////////////////
Dim As string alarmas(1 To 20), HORA="00:00:00" 
For X As Integer=1 To 20
    alarmas(x)="0"
Next X
Dim As Integer elfont=30, INTERVALO1=0,INTERVALO2=0,cntalarmas=0,HAYINTERVALO=0
Dim As String MINUTOS="0" ,TITULO=Format(NOW,"dd/mm/yyyy")
Dim As Long result = Weekday( Now) ''' [, firstdayofweek ] )
Dim As String dia = diaw(result)
TITULO=TITULO+" "+dia
Dim As String MENSAJE=" ACEPTE, LUEGO ENTRE INTERVALO Y MENSAJE.",MENSAJE1
Dim As String MENSAJE2 ="DESCANSE UN POCO ..",windir, default=Str("0")
Dim As String MENSAJES_FIJOS (1 To 20), MENSAJE3,INTER,OLDALARM
 
Dim As long ALTO = GetSystemMetrics(SM_CYSCREEN)
Dim As Long ANCHO= GetSystemMetrics(SM_CXSCREEN)
MessBox("INFO" ,"ALARMA DIGITAL version 5, SE PROGRAMA TODAS LAS VECES QUE SE DESEE PARA CORTAR " _
" EL TRABAJO O RECORDAR ALGO, PULSANDO LA HORA DEJA CONFIGURAR MINUTOS Y MENSAJE DEL INTERVALO. " _
" SI NO SE CONFIGURA O LOS MINUTOS VALEN 0, ES UN RELOJ SIN ALARMA, SI SE PULSA cierre  de vantana [X], " _ 
" MUESTRA LAS ALARMAS GRABADAS EN DISCO Y CONFIGURA UNA ALARMA EN FORMATO 00:00:00 (hasta 20 alarmas una por vez), CON 0 TERMINA EL PROGRAMA") 
TITULO=InputBox("AJUSTE" ,"TITULO DE LA ALARMA2?  ", TITULO, ES_MULTILINE + ES_AUTOVSCROLL,0 )
 Dim As Integer x, y, buttons, res ,SALE=0
ANCHO=ANCHO-252
ALTO=ALTO-132
''300,10,260,100,,WS_EX_TOPMOST
var hwnd= OpenWindow(TITULO,ANCHO,ALTO,260,100,,WS_EX_TOPMOST)


 
SetTimer(hwnd,1,1000,0) ' cada segundo
ButtonGadget(3,10,10,250,50,"",BS_LEFT  ) ' 240 60
SetGadgetFont(3,CINT(LoadFont("arial",elfont))) 
Var TIEMPO1=Time
Var TIEMPO2=Time
Var TIEMPO3=Time
Open "INTERVALO.TXT" For APPEND As 1 ' SINO EXISTE LO CREA
Sleep 10
Close 1
Sleep 10

Open "INTERVALO.TXT" For Input As 2 '
Input #2, INTER, MENSAJE2

If MENSAJE2="" Then
   MENSAJE2=MENSAJE1
Else
MENSAJE1=MENSAJE2
EndIf
Close 2

Sleep 100
MINUTOS=Trim(INTER)
INTERVALO1=CInt(MINUTOS)*60

Open "ALARMAS.TXT" For Append As 3 ' SINO EXISTE LO CREA
Sleep 10
Close 3
Sleep 10
Open "ALARMAS.TXT" For Input As 4 ' SINO EXISTE LO CREA
cntalarmas=0
Do While Not Eof(4)
  cntalarmas=cntalarmas+1
  Input #4, alarmas(cntalarmas), MENSAJES_FIJOS(cntalarmas)
  
Loop
'SI CNTALARMAS >0 TENEMOS ALGUNAS ALARMAS DEL DIA ANTERIOR
''''''''
Close 4
 
 
Do 

  Var EV=WaitEvent
   Select Case EV
     Case EventClose
             
 default=Str("00:00:00")
       
Var H=THREADCALL SUB_notepad()

'HORA=InputBox("CONFIG ALARMAS FIJAS O SALE" ,"HORA EN FORMATO 00:00:00 ", default, ES_MULTILINE + ES_AUTOVSCROLL,hwnd )
Sleep 20
HORA=default
Var H2=THREADCALL sub_HORA(HORA,default,hwnd)
setfocus(hwnd)
''''Dim As HWND wnd = FindWindow("notepad.exe",NULL)
''' Dim As UInteger pd1
'''pd1= GetProcessIdOfThread(wnd) el linker no lo encuentra UNA MIERCOLES
''TerminateProcess (wnd,0) 
'''ThreadDetach(wnd)
ThreadDetach(H)
ThreadWait(H2)
  Dim As String t1
        t1=Mid(HORA,1,1)
        If esnumerico(t1)=1 And t1="" And Len(Trim(HORA))=1 Then
           Continue  Do ' sigue no hace nada
        EndIf 
        If esnumerico(t1)=0 And t1="0" And Len(Trim(HORA))=1 Then
           End
        EndIf
        If  Len(Trim(t1))=8 Then
        Else
           Continue Do
        EndIf 
 
' validamos
        If entradavalida(HORA)=1 Then
           Continue Do
        EndIf 
          cntalarmas=cntalarmas+1
          If cntalarmas=21 Then
             cntalarmas=20
             For t1 As Integer=1 To 19
             alarmas(t1+1)=alarmas(t1)  
             Next t1
            alarmas(1)=Trim(HORA)
          Else 
            alarmas(cntalarmas)=Trim(HORA) 
          EndIf
         Var h3 = threadcall submensaje3(MENSAJE3,hwnd)      
         ThreadWait (h3)    
         MENSAJES_FIJOS(cntalarmas)=Trim(MENSAJE3)
          Open "ALARMAS.TXT" For Output As 6
          For J1 As Integer=1 To cntalarmas
            Print #6, alarmas(J1);",";MENSAJES_FIJOS(J1) 
          Next j1
          Close 6
          Sleep 100
     Case EventTimer 
        TIEMPO1=Time
        If INTERVALO1 > 0 Then
           HAYINTERVALO=1
           If INTERVALO2=INTERVALO1 Then
  ' UNO DE LOS RING DE WINDOWS 10, si se pone en el inicio alarma2 y el wav, windows
  ' ejecuta el wav incha pelota, cambiamos la extension del wav a ver si funciona aca
  ' y si win se deja de inchar las pelotas...jaja
              PlaySound(".\Ring05.wav", 0, SND_FILENAME+SND_NODEFAULT +SND_ASYNC)
             ''Beep
              
              Var t1= threadCall sub1 (MENSAJE2)
              setfocus(hwnd)
              INTERVALO2=0
           Else
              INTERVALO2=INTERVALO2+1
              SetGadgetText (3,Str(TIEMPO1)+" "+dia)
           EndIf
        Else
           HAYINTERVALO=0
           TIEMPO2=Time 
             SetGadgetText (3,Str(TIEMPO2)+" "+dia) 
        EndIf
            
''         SetGadgetText (3,Str(TIEMPO))
        If TIEMPO1="00:00:01" Or TIEMPO2="00:00:01" Or TIEMPO3="00:00:01"  Then
           result=weekday (Now)           
           dia=diaw(result)
           TITULO=Format(NOW,"dd/mm/yyyy")+dia
 
           SetWindowText(hwnd, TITULO)
        Else
           If alarmas(1)<>"" Then
              For i1 As Integer=1 To cntalarmas

                If TIEMPO1=alarmas(i1) Or TIEMPO2=alarmas(i1) Or TIEMPO3=alarmas(i1)Then
                   PlaySound(".\Alarm06.wav", 0, SND_FILENAME+SND_NODEFAULT +SND_ASYNC)
                   If OLDALARM <> alarmas(i1) Then ' SOLO UN MSGBOX SONIDO PUEDE SER MAS
                      Var t2= ThreadCall  sub2 (MENSAJES_FIJOS(i1))
                   EndIf
                   OLDALARM=alarmas(i1)
                   Exit For  
                EndIf
                
              Next i1 
             TIEMPO3=Time 
             SetGadgetText (3,Str(TIEMPO3)+" "+dia) 
           EndIf
           
        EndIf
  
     Case EventSize 
      ''   ? SizeX & "  " & SizeY
      elfont=SizeX * 30 / 260      
      ButtonGadget(3,10,10,sizex-10,sizey-10,"",BS_LEFT  ) ' 240 60
      SetGadgetFont(3,CINT(LoadFont("arial",elfont)))
     CASE EventGadget
       Select Case EventNumber
  ''       Case 1
   ''         MessBox("","It's WHAT")
         Case 3  'boton solo cambia intervalo
         MessBox( "",MENSAJE)
         Dim As String default=Str("0")
         If INTERVALO1 > 0 Then
            default=Str(MINUTOS)
         EndIf
            
         MINUTOS=InputBox("CONFIG ALARMA" ,"INTERVALO EN MINUTOS ", default, ES_MULTILINE + ES_AUTOVSCROLL,hwnd )
         If INTERVALO1=0 Then
            MENSAJE2=MENSAJE1
         EndIf
         MENSAJE2=InputBox("MENSAJE" ,"CAMBIE O ACEPTE EL MENSAJE ", MENSAJE2, ES_MULTILINE + ES_AUTOVSCROLL,hwnd )     
         INTERVALO1=CInt(MINUTOS)*60 'en  segundo
         INTERVALO2=0
   Open "INTERVALO.TXT" For Output As 7
   Print #7, MINUTOS;",";MENSAJE2 ' EL Unico y ULTIMO 
   Close 7    
     Sleep 100        
       END Select
   
   End Select
   
Loop 
End
