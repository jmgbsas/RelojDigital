' ALARMA DIGITAL , SE PROGRAMA TODAS LAS VECES QUE SE DESEE ES REPETITIVA  
' PARA CORTAR EL TRABAJO O RECORDAR ALGO UNA SOLA COSA POR AHORA
' RELOJ1 ..se puede usar varias instancias por ahora para tener varias alarmas
' el asunto es como saber cual es cual
#INCLUDE "WINDOW9.BI"
' AUTOR JOSE GALEANO
Dim As Integer ancho, alto, elfont=30, INTERVALO1=0,INTERVALO2=0
Dim As String MINUTOS ,TITULO="ALARMA",MENSAJE=" DESCANSA UN POCO VAMOS.."
MessBox("INFO" ,"ALARMA DIGITAL, SE PROGRAMA TODAS LAS VECES QUE SE DESEE ES REPETITIVA PARA CORTAR EL TRABAJO O RECORDAR ALGO UNA SOLA COSA POR AHORA, PULSANDO LA HORA DEJA CONFIGURAR MINUTOS Y MENSAJE, SI NO SE CONFIGURA O LOS MINUTOS VALEN 0, ES UN RELOJ SIN ALARMA")
TITULO=InputBox("AJUSTE" ,"TITULO DE LA ALARMA?  ", TITULO, ES_MULTILINE + ES_AUTOVSCROLL,0 )
 
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
             Beep
              MessBox("PASARON " + MINUTOS ,MENSAJE)
              INTERVALO2=0  
           Else 
             INTERVALO2=INTERVALO2 + 1

           EndIf
       
        EndIf     
      SetGadgetText (3,Str(TIEMPO))
     Case EventSize 
      ''   ? SizeX & "  " & SizeY
      elfont=SizeX * 30 / 260      
      ButtonGadget(3,10,10,sizex-10,sizey-10,"",BS_LEFT  ) ' 240 60
      SetGadgetFont(3,CINT(LoadFont("arial",elfont)))
   CASE EventGadget 
Dim As String default=Str("0")
 
 MINUTOS=InputBox("CONFIG ALARMA" ,"INTERVALO EN MINUTOS ", default, ES_MULTILINE + ES_AUTOVSCROLL,0 )
 MENSAJE=InputBox("MENSAJE" ,"CAMBIE O ACEPTE EL MENSAJE ", MENSAJE, ES_MULTILINE + ES_AUTOVSCROLL,0 )     
       Print "MINUTOS  ",MINUTOS 
        INTERVALO1=CInt(MINUTOS)*60 'en  segundo
        INTERVALO2=0  
        ''Print " INTERVALO1 ",INTERVALO1
   
   End Select
 
Loop 
End
