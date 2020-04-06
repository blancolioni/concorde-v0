package body Nazar.Views.Scale is

   type Change_Value_Signal_Handler is
     new Nazar.Signals.Signal_Handler_Interface with
      record
         Callback : Change_Value_Callback;
      end record;

   overriding function Handle
     (Handler     : Change_Value_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean;

   ------------------------------
   -- Emit_Change_Value_Signal --
   ------------------------------

   procedure Emit_Change_Value_Signal
     (View      : Nazar_Scale_View_Interface'Class;
      New_Value : Nazar_Float)
   is
   begin
      View.Emit (View, Signal_Change_Value,
                 Change_Value_Signal_Data'
                   (New_Value => New_Value));
   end Emit_Change_Value_Signal;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Change_Value_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Source);
      Data : Change_Value_Signal_Data'Class renames
        Change_Value_Signal_Data'Class (Signal_Data);
   begin
      Handler.Callback (Data.New_Value, User_Data);
      return True;
   end Handle;

   ---------------------
   -- On_Change_Value --
   ---------------------

   procedure On_Change_Value
     (View      : in out Nazar_Scale_View_Interface'Class;
      Handler   :        Change_Value_Callback;
      User_Data :        Nazar.Signals.User_Data_Interface'Class)
   is
      Id : constant Nazar.Signals.Handler_Id :=
        View.Add_Handler
          (Signal    => Signal_Change_Value,
           Source    => View,
           User_Data => User_Data,
           Handler   => Change_Value_Signal_Handler'(Callback => Handler));
   begin
      pragma Unreferenced (Id);
   end On_Change_Value;

   -------------------------
   -- Signal_Change_Value --
   -------------------------

   function Signal_Change_Value return Nazar.Signals.Signal_Type is
   begin
      return Nazar.Signals.Signal ("signal-change-value");
   end Signal_Change_Value;

end Nazar.Views.Scale;
