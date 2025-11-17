' RELOJ DIGITAL NO ENCONTRE UN GADGET DIGITAL EN WINDOWS Y ME HICE UNO
' EL DIGITAL DE WINDOWS EN MUYYYYY CHICO NO SE VE UN CARAJO 
#INCLUDE "WINDOW9.BI"
' AUTOR JOSE GALEANO
Dim As Integer ancho, alto, elfont=30

var hwnd= OpenWindow("DIGITAL",300,10,260,100,,WS_EX_TOPMOST )
SetTimer(hwnd,1,1000,0) ' cada segundo
ButtonGadget(3,10,10,250,50,"",BS_LEFT  ) ' 240 60
SetGadgetFont(3,CINT(LoadFont("arial",elfont))) 
Var TIEMPO=Time
Do 
Var ev=WaitEvent
   If ev=EventClose Then
      End
   ElseIf ev=EventTimer Then
   TIEMPO=Time
    SetGadgetText (3,Str(TIEMPO))
   ElseIf ev=EventSize then
         ? SizeX & "  " & SizeY
      elfont=SizeX * 30 / 260      
      ButtonGadget(3,10,10,sizex-10,sizey-10,"",BS_LEFT  ) ' 240 60
      SetGadgetFont(3,CINT(LoadFont("arial",elfont)))
   EndIf
 
Loop 
End

