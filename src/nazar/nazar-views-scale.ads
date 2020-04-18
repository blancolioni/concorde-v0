package Nazar.Views.Scale is

   type Nazar_Scale_View_Interface is interface and Nazar_View_Interface;

   type Nazar_Scale_View is access all Nazar_Scale_View_Interface'Class;

   type Change_Value_Callback is access
     procedure (New_Value : Nazar_Float;
                User_Data : Nazar.Signals.User_Data_Interface'Class);

   procedure On_Change_Value
     (View      : in out Nazar_Scale_View_Interface'Class;
      Handler   : Change_Value_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class);

   function Signal_Change_Value return Nazar.Signals.Signal_Type;

   procedure Emit_Change_Value_Signal
     (View      : Nazar_Scale_View_Interface'Class;
      New_Value : Nazar_Float);

   type Change_Value_Signal_Data is
     new Nazar.Signals.Signal_Data_Interface with private;

   function New_Value
     (Signal_Data : Change_Value_Signal_Data'Class)
      return Nazar_Float;

private

   type Change_Value_Signal_Data is
     new Nazar.Signals.Signal_Data_Interface with
      record
         New_Value : Nazar_Float;
      end record;

   function New_Value
     (Signal_Data : Change_Value_Signal_Data'Class)
      return Nazar_Float
   is (Signal_Data.New_Value);

end Nazar.Views.Scale;
