with Nazar.Interfaces.Text_Writer;

package body Nazar.Views.Gtk_Views.Console is

   ----------------------
   -- Gtk_Console_View --
   ----------------------

   function Gtk_Console_View
     (Model : not null access
        Nazar.Models.Text_Writer.Root_Text_Writer_Model'Class)
      return Nazar_Gtk_Console_View
   is
   begin
      return Result : constant Nazar_Gtk_Console_View :=
        new Root_Gtk_Console_View
      do
         Result.Text_View :=
           Gtk.Text_View.Gtk_Text_View_New;
         Result.Text_Buffer := Result.Text_View.Get_Buffer;
         Result.Set_Model (Model);
         Result.Initialize (Result.Text_View);
      end return;
   end Gtk_Console_View;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed (View : in out Root_Gtk_Console_View) is

      procedure Put_Class_Line
        (Class : Nazar.Interfaces.Text_Writer.Text_Class;
         Line  : String);

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
               View.Text_Buffer.Insert_At_Cursor (Line & Character'Val (10));
            when Error_Text =>
               View.Text_Buffer.Insert_At_Cursor (Line & Character'Val (10));
         end case;
      end Put_Class_Line;

   begin
      View.Writer_Model.Iterate_Lines
        (Start   => View.Last_Line,
         Process => Put_Class_Line'Access);
   end Model_Changed;

   ---------------------
   -- Set_Prompt_Text --
   ---------------------

   overriding procedure Set_Prompt_Text
     (View : in out Root_Gtk_Console_View; Prompt_Text : String)
   is
   begin
      View.Prompt := Ada.Strings.Unbounded.To_Unbounded_String (Prompt_Text);
   end Set_Prompt_Text;

end Nazar.Views.Gtk_Views.Console;
