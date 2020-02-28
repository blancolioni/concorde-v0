package body Nazar.Views.Console is

   -------------------------
   -- Emit_Command_Signal --
   -------------------------

   procedure Emit_Command_Signal
     (View    : Console_View_Interface'Class;
      Command : String)
   is
   begin
      View.Emit (View, Signal_Command,
                 Command_Signal_Data'
                   (Command =>
                      Ada.Strings.Unbounded.To_Unbounded_String (Command)));
   end Emit_Command_Signal;

   --------------------
   -- Signal_Command --
   --------------------

   function Signal_Command return Nazar.Signals.Signal_Type is
   begin
      return Nazar.Signals.Signal ("signal-command");
   end Signal_Command;

end Nazar.Views.Console;
