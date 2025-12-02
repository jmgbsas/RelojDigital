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
Dim As Integer ancho, alto, elfont=30, INTERVALO1=0,INTERVALO2=0
Dim As String MINUTOS ,TITULO=Format(NOW,"dd/mm/yyyy"),MENSAJE=" ACEPTE, LUEGO ENTRE INTERVALO Y MENSAJE.", MENSAJE2 ="DESCANSE UN POCO .."
MessBox("INFO" ,"ALARMA2 DIGITAL, SE PROGRAMA TODAS LAS VECES QUE SE DESEE ES REPETITIVA PARA CORTAR EL TRABAJO O RECORDAR ALGO UNA SOLA COSA POR AHORA, PULSANDO LA HORA DEJA CONFIGURAR MINUTOS Y MENSAJE, SI NO SE CONFIGURA O LOS MINUTOS VALEN 0, ES UN RELOJ SIN ALARMA")
TITULO=InputBox("AJUSTE" ,"TITULO DE LA ALARMA2?  ", TITULO, ES_MULTILINE + ES_AUTOVSCROLL,0 )
 Dim As Integer x, y, buttons, res ,SALE=0

var hwnd= OpenWindow(TITULO,300,10,260,100,,WS_EX_TOPMOST )
 
SetTimer(hwnd,1,1000,0) ' cada segundo
ButtonGadget(3,10,10,250,50,"",BS_LEFT  ) ' 240 60
SetGadgetFont(3,CINT(LoadFont("arial",elfont))) 
Var TIEMPO=Time
Do 
  Var EV=WaitEvent
   Select Case EV
     Case EventClose 
        End
     Case EventTimer 
        TIEMPO=Time
        If INTERVALO1 > 0 Then
            
           If INTERVALO2=INTERVALO1 Then
  ' UNO DE LOS RING DE WINDOWS 10
    PlaySound(".\Ring05.wav", 0, SND_FILENAME+SND_NODEFAULT +SND_ASYNC)
      
             ''Beep
             SetGadgetText (3, MENSAJE)
              INTERVALO2=INTERVALO2+1
           Else
             If INTERVALO2 > INTERVALO1 Then
               SetGadgetText (3,Str(MENSAJE))
             Else
                 SetGadgetText (3,Str(TIEMPO)) 
                 INTERVALO2=INTERVALO2+1
             EndIf

           EndIf
        Else
          SetGadgetText (3,Str(TIEMPO))
          If TIEMPO="00:00:01" Then
             TITULO=Format(NOW,"dd/mm/yyyy")
             SetWindowText(hwnd, TITULO)
          Else
             TIEMPO=Time  
          EndIf
        EndIf 

     Case EventSize 
      ''   ? SizeX & "  " & SizeY
      elfont=SizeX * 30 / 260      
      ButtonGadget(3,10,10,sizex-10,sizey-10,"",BS_LEFT  ) ' 240 60
      SetGadgetFont(3,CINT(LoadFont("arial",elfont)))
   CASE EventGadget
       Select Case EventNumber
         Case 1
            MessBox("","It's WHAT")
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
