' VERSION 3 MUESTRA EL MENSAJE COMPLETO COMO ANTES Y NO SOBRE EL RELOJ
' SINO EL RELOJ SE RETRASA, SE PROBÓ CON 20 ALARMAS DE LAS CUALES EJECUTO 14
' Y 3 INTERVALOS , NINGUN MENSAJE SE CERROR PODRIA SE E LLIMITE DE THREAD?
' PERO SI SE CIERRE CADA MENSAJE TAL VEZ NO HAYA ESE LIMITE?
' UNA PRUEBA LARGA HACE FALTA ALGUN DIA ,,,POR AHORA ES LO QUE HAY... 
' graba alarmas solo toca la 1er alarma 
#INCLUDE "WINDOW9.BI"
#include "string.bi"
#include "DATETIME.bi"
#include "file.bi"

' AUTOR JOSE GALEANO 09-12-2025 COMO HOBBY Y HERRAMIENTA DE TRABAJAO EN PC 
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

Dim As string alarmas(1 To 20), HORA 
For X As Integer=1 To 20
    alarmas(x)="0"
Next X
Dim As Integer ancho, alto, elfont=30, INTERVALO1=0,INTERVALO2=0,cntalarmas=0,HAYINTERVALO=0
Dim As String MINUTOS="0" ,TITULO=Format(NOW,"dd/mm/yyyy")
Dim As String MENSAJE=" ACEPTE, LUEGO ENTRE INTERVALO Y MENSAJE.",MENSAJE1
Dim As String MENSAJE2 ="DESCANSE UN POCO .."
Dim As String MENSAJES_FIJOS (1 To 20), MENSAJE3,INTER,OLDALARM
MessBox("INFO" ,"ALARMA3 DIGITAL, SE PROGRAMA TODAS LAS VECES QUE SE DESEE PARA CORTAR EL TRABAJO O RECORDAR ALGO, PULSANDO LA HORA DEJA CONFIGURAR MINUTOS Y MENSAJE DEL INTERVALO. SI NO SE CONFIGURA O LOS MINUTOS VALEN 0, ES UN RELOJ SIN ALARMA, SI SE PULSA X, CONFIGURA UNA HORA DE ALARMA EN FORMATO 00:00:00, CON 0 TERMINA EL PROGRAMA") 
TITULO=InputBox("AJUSTE" ,"TITULO DE LA ALARMA2?  ", TITULO, ES_MULTILINE + ES_AUTOVSCROLL,0 )
 Dim As Integer x, y, buttons, res ,SALE=0

var hwnd= OpenWindow(TITULO,300,10,260,100,,WS_EX_TOPMOST )
 
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
MENSAJE1=MENSAJE2
Open "INTERVALO.TXT" For Input As 2 '
Input #2, INTER, MENSAJE2

If MENSAJE2="" Then
   MENSAJE2=MENSAJE1
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
       Dim As String default=Str("0")
       HORA=InputBox("CONFIG ALARMAS FIJAS O SALE" ,"HORA EN FORMATO 00:00:00 ", default, ES_MULTILINE + ES_AUTOVSCROLL,0 )
        If HORA="0" Then 
           End
        Else
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
          
   
         MENSAJE3=InputBox("MENSAJE DE ALARMA UNICA " ,"MENSAJE A ", "", ES_MULTILINE + ES_AUTOVSCROLL,0 )
         MENSAJES_FIJOS(cntalarmas)=Trim(MENSAJE3)
          Open "ALARMAS.TXT" For Output As 6
          For J1 As Integer=1 To cntalarmas
            Print #6, alarmas(J1);",";MENSAJES_FIJOS(J1) 
          Next j1
          Close 6
          Sleep 100
        EndIf
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
              
              INTERVALO2=0
           Else
              INTERVALO2=INTERVALO2+1
              SetGadgetText (3,Str(TIEMPO1))
           EndIf
        Else
           HAYINTERVALO=0
           TIEMPO2=Time 
             SetGadgetText (3,Str(TIEMPO2)) 
        EndIf
            
''         SetGadgetText (3,Str(TIEMPO))
        If TIEMPO1="00:00:01" Or TIEMPO2="00:00:01" Or TIEMPO3="00:00:01"  Then
           TITULO=Format(NOW,"dd/mm/yyyy")
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
             SetGadgetText (3,Str(TIEMPO3)) 
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
            
         MINUTOS=InputBox("CONFIG ALARMA" ,"INTERVALO EN MINUTOS ", default, ES_MULTILINE + ES_AUTOVSCROLL,0 )
         If INTERVALO1=0 Then
            MENSAJE2=MENSAJE1
         EndIf
         MENSAJE2=InputBox("MENSAJE" ,"CAMBIE O ACEPTE EL MENSAJE ", MENSAJE2, ES_MULTILINE + ES_AUTOVSCROLL,0 )     
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
