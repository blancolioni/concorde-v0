with Gdk.Event;
with Gdk.Types.Keysyms;

with Gtk.Style_Context;

with Nazar.Interfaces.Text_Writer;

package body Nazar.Views.Gtk_Views.Console is

   function From_Object is
     new Nazar.Views.Gtk_Views.From_Gtk_Object
       (Root_Gtk_Console_View, Nazar_Gtk_Console_View);

   function On_Text_View_Key_Press
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key)
      return Boolean;

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

         Text_View.On_Key_Press_Event
           (On_Text_View_Key_Press'Access, Result.Object);

      end return;
   end Gtk_Console_View;

   function Nazar_Gtk_Console_View_Create
     return Nazar_View
   is
      Text_View : constant Gtk.Text_View.Gtk_Text_View :=
        Gtk.Text_View.Gtk_Text_View_New;
      Result    : constant Nazar_Gtk_Console_View :=
        new Root_Gtk_Console_View;
   begin
      Gtk.Style_Context.Get_Style_Context (Text_View).Add_Class
        ("nazar-console");

      Result.Text_View := Text_View;
      Result.Text_Buffer := Result.Text_View.Get_Buffer;
      Result.Initialize (Result.Text_View);

      Text_View.On_Key_Press_Event
        (On_Text_View_Key_Press'Access, Result.Object);

      return Nazar_View (Result);

   end Nazar_Gtk_Console_View_Create;

   ----------------------------
   -- On_Text_View_Key_Press --
   ----------------------------

   function On_Text_View_Key_Press
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key)
      return Boolean
   is
      View : constant Nazar_Gtk_Console_View := From_Object (Self);
   begin

      case Event.Keyval is
         when Gdk.Types.Keysyms.GDK_Return =>
            declare
               Command : constant String :=
                 Ada.Strings.Unbounded.To_String (View.Command_Buffer);
            begin
               View.Text_Buffer.Insert_At_Cursor ((1 => Character'Val (10)));
               if Command = "" then
                  View.Text_Buffer.Insert_At_Cursor
                    (View.Console_Model.Get_Prompt_Text);
               else
                  View.Emit_Command_Signal (Command);
               end if;
               View.Command_Buffer :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
               return True;
            end;

         when Gdk.Types.Keysyms.GDK_BackSpace =>
            declare
               use Ada.Strings.Unbounded;
               Cmd : Unbounded_String renames View.Command_Buffer;
            begin
               if Length (Cmd) = 0 then
                  return True;
               else
                  Cmd := Head (Cmd, Length (Cmd) - 1);
                  return False;
               end if;
            end;

         when 32 .. 126 =>
            Ada.Strings.Unbounded.Append (View.Command_Buffer,
                                          (1 => Character'Val (Event.Keyval)));
            return False;
         when others =>
            return True;
      end case;
   end On_Text_View_Key_Press;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Root_Gtk_Console_View)
   is

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
   end Update_From_Model;

end Nazar.Views.Gtk_Views.Console;
