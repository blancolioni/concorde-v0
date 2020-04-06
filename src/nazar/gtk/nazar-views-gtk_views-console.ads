private with Ada.Strings.Unbounded;
private with Gtk.Text_Buffer;
private with Gtk.Text_View;

with Nazar.Models.Console;
with Nazar.Models.Text_Writer;

with Nazar.Views.Console;

package Nazar.Views.Gtk_Views.Console is

   type Root_Gtk_Console_View is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Console.Console_View_Interface
   with private;

   type Nazar_Gtk_Console_View is access all Root_Gtk_Console_View'Class;

   function Gtk_Console_View
     (Model : not null access Nazar.Models.Console.Root_Console_Model'Class)
      return Nazar_Gtk_Console_View;

   function Nazar_Gtk_Console_View_Create
      return Nazar_View;

private

   type Root_Gtk_Console_View is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Console.Console_View_Interface with
      record
         Last_Line      : Nazar.Models.Text_Writer.Line_Cursor;
         Text_View      : Gtk.Text_View.Gtk_Text_View;
         Text_Buffer    : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Command_Buffer : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Class_Name
     (View : Root_Gtk_Console_View)
      return String
   is ("nazar-gtk-console-view");

   overriding procedure Update_From_Model
     (View : in out Root_Gtk_Console_View);

   type Model_Access is
     access all Nazar.Models.Console.Root_Console_Model'Class;

   function Console_Model
     (View : Root_Gtk_Console_View'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Console;
