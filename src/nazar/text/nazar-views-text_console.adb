with Ada.Text_IO;

with Nazar.Interfaces.Text_Writer;

package body Nazar.Views.Text_Console is

   type Command_Signal_Handler is
     new Nazar.Signals.Signal_Handler_Interface with
      record
         Callback : Nazar.Views.Console.Command_Callback;
      end record;

   overriding function Handle
     (Handler     : Command_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean;

   procedure Put_Class_Line
     (Class : Nazar.Interfaces.Text_Writer.Text_Class;
      Line  : String);

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Handler     : Command_Signal_Handler;
      Source      : Nazar.Signals.Signal_Source_Interface'Class;
      Signal_Data : Nazar.Signals.Signal_Data_Interface'Class;
      User_Data   : Nazar.Signals.User_Data_Interface'Class)
      return Boolean
   is
      pragma Unreferenced (Source);
      Command_Data : Nazar.Views.Console.Command_Signal_Data'Class renames
        Nazar.Views.Console.Command_Signal_Data'Class (Signal_Data);
   begin
      Handler.Callback (Command_Data.Command_Line, User_Data);
      return True;
   end Handle;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed
     (View : in out Root_Text_Console_View)
   is
   begin
      View.Writer_Model.Iterate_Lines
        (Start   => View.Last_Line,
         Process => Put_Class_Line'Access);
   end Model_Changed;

   ----------------
   -- On_Command --
   ----------------

   overriding procedure On_Command
     (View      : in out Root_Text_Console_View;
      Handler   : Nazar.Views.Console.Command_Callback;
      User_Data : Nazar.Signals.User_Data_Interface'Class)
   is
      Id : constant Nazar.Signals.Handler_Id :=
        View.Add_Handler
          (Signal    => Nazar.Views.Console.Signal_Command,
           Source    => View,
           User_Data => User_Data,
           Handler   => Command_Signal_Handler'(Callback => Handler));
   begin
      pragma Unreferenced (Id);
   end On_Command;

   --------------------
   -- Put_Class_Line --
   --------------------

   procedure Put_Class_Line
     (Class : Nazar.Interfaces.Text_Writer.Text_Class;
      Line  : String)
   is
      use all type Nazar.Interfaces.Text_Writer.Text_Class;
   begin
      case Class is
         when Standard_Text =>
            Ada.Text_IO.Put_Line (Line);
         when Error_Text =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, Line);
      end case;
   end Put_Class_Line;

   ---------------------
   -- Set_Prompt_Text --
   ---------------------

   overriding procedure Set_Prompt_Text
     (View        : in out Root_Text_Console_View;
      Prompt_Text : String)
   is
   begin
      View.Prompt := Ada.Strings.Unbounded.To_Unbounded_String (Prompt_Text);
   end Set_Prompt_Text;

   ----------
   -- Show --
   ----------

   overriding procedure Show
     (View : in out Root_Text_Console_View)
   is
   begin
      loop
         Ada.Text_IO.Put
           (Ada.Strings.Unbounded.To_String (View.Prompt));
         Ada.Text_IO.Flush;
         declare
            Line : constant String := Ada.Text_IO.Get_Line;
         begin
            if Line = "" then
               null;
            elsif Line = "exit" then
               exit;
            else
               View.Emit_Command_Signal
                 (Command => Line);
            end if;
         end;
      end loop;
   end Show;

   -----------------------
   -- Text_Console_View --
   -----------------------

   function Text_Console_View
     (Model : not null access Nazar.Models.Text_Writer
      .Root_Text_Writer_Model'Class)
      return Nazar_Text_Console_View
   is
   begin
      return View : constant Nazar_Text_Console_View :=
        new Root_Text_Console_View
      do
         View.Set_Model (Model);
         View.Last_Line := Model.First_Line;
         View.Set_Prompt_Text (">");
      end return;
   end Text_Console_View;

end Nazar.Views.Text_Console;
