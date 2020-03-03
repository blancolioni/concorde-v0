with Gtk.Style_Context;

with Nazar.Interfaces.Text_Writer;

package body Nazar.Views.Gtk_Views.Console is

   ----------------------
   -- Gtk_Console_View --
   ----------------------

   function Gtk_Console_View
     (Model : not null access Nazar.Models.Console.Root_Console_Model'Class)
      return Nazar_Gtk_Console_View
   is
      Text_View : constant Gtk.Text_View.Gtk_Text_View :=
        Gtk.Text_View.Gtk_Text_View_New;
   begin
      Gtk.Style_Context.Get_Style_Context (Text_View).Add_Class
        ("nazar-console");

      return Result : constant Nazar_Gtk_Console_View :=
        new Root_Gtk_Console_View
      do
         Result.Text_View := Text_View;
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
      View.Console_Model.Iterate_Lines
        (Start   => View.Last_Line,
         Process => Put_Class_Line'Access);
      View.Text_Buffer.Insert_At_Cursor
        (View.Console_Model.Get_Prompt_Text);
   end Model_Changed;

end Nazar.Views.Gtk_Views.Console;
