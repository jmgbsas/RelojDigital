' VERSION 2 PRIMERAS LETRAS DEL MENSAJE REEMPLAZA LA HORA AL CUMPLIRSE EL INTERVALO 
' A LDAR CLICK SOBRE LA HORA CON MENSSJAE SE INICIA DENUEVO OTRO CICLO CON EL MISMO MENSAJE, 
' SE PROGRAMA TODAS LAS VECES QUE SE DESEE ES REPETITIVA  
' PARA CORTAR EL TRABAJO O RECORDAR ALGO UNA SOLA COSA POR AHORA
' RELOJ1 ..se puede usar varias instancias por ahora para tener varias alarmas
' el asunto es como saber cual es cual
#INCLUDE "WINDOW9.BI"
#include "string.bi"
#include "DATETIME.bi"
' AUTOR JOSE GALEANO
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

Dim As string alarmas(1 To 20), HORA
For X As Integer=1 To 20
    alarmas(x)="0"
Next X
Dim As Integer ancho, alto, elfont=30, INTERVALO1=0,INTERVALO2=0,cntalarmas=0,HAYINTERVALO=0
Dim As String MINUTOS ,TITULO=Format(NOW,"dd/mm/yyyy"),MENSAJE=" ACEPTE, LUEGO ENTRE INTERVALO Y MENSAJE.", MENSAJE2 ="DESCANSE UN POCO .."
Dim As String  MENSAJES_FIJOS (1 To 100), MENSAJE3
MessBox("INFO" ,"ALARMA2 DIGITAL, SE PROGRAMA TODAS LAS VECES QUE SE DESEE PARA CORTAR EL TRABAJO O RECORDAR ALGO, PULSANDO LA HORA DEJA CONFIGURAR MINUTOS Y MENSAJE, SI NO SE CONFIGURA O LOS MINUTOS VALEN 0, ES UN RELOJ SIN ALARMA, SI SE PULSA X, CONFIGURA UNA HORA DE ALARMA EN FORMATO 00:00:00, CON 0 TERMINA EL PROGRAMA")
TITULO=InputBox("AJUSTE" ,"TITULO DE LA ALARMA2?  ", TITULO, ES_MULTILINE + ES_AUTOVSCROLL,0 )
 Dim As Integer x, y, buttons, res ,SALE=0

var hwnd= OpenWindow(TITULO,300,10,260,100,,WS_EX_TOPMOST )
 
SetTimer(hwnd,1,1000,0) ' cada segundo
ButtonGadget(3,10,10,250,50,"",BS_LEFT  ) ' 240 60
SetGadgetFont(3,CINT(LoadFont("arial",elfont))) 
Var TIEMPO1=Time
Var TIEMPO2=Time
Var TIEMPO3=Time

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
             cntalarmas=1
          EndIf
          alarmas(cntalarmas)=Trim(HORA)
MENSAJE3=InputBox("MENSAJE DE ALARMA UNICA " ,"MENSAJE A ", "", ES_MULTILINE + ES_AUTOVSCROLL,0 )
MENSAJES_FIJOS(cntalarmas)=Trim(MENSAJE3)
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
              SetGadgetText (3, MENSAJE)
              retardo()
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
           If alarmas(1)<>"0" Then
              For i1 As Integer=1 To 20
                If TIEMPO1=alarmas(i1) Or TIEMPO2=alarmas(i1) Or TIEMPO3=alarmas(i1)Then
                   PlaySound(".\Alarm06.wav", 0, SND_FILENAME+SND_NODEFAULT +SND_ASYNC)
                   SetGadgetText (3,Str(MENSAJES_FIJOS(cntalarmas)))
                   retardo()
                 EndIf
                Exit For
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
'         Case 1
'            MessBox("","It's WHAT")
         Case 3
         MessBox( "",MENSAJE)
         Dim As String default=Str("0")
         MINUTOS=InputBox("CONFIG ALARMA" ,"INTERVALO EN MINUTOS ", default, ES_MULTILINE + ES_AUTOVSCROLL,0 )
         If INTERVALO1=0 Then
            MENSAJE=MENSAJE2
         EndIf
         MENSAJE=InputBox("MENSAJE" ,"CAMBIE O ACEPTE EL MENSAJE ", MENSAJE, ES_MULTILINE + ES_AUTOVSCROLL,0 )     
         INTERVALO1=CInt(MINUTOS)*60 'en  segundo
         INTERVALO2=0
        ''Print " INTERVALO1 ",INTERVALO1
             
       END Select   
   End Select
 
Loop 
End
