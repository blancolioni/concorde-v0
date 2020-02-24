with Ada.Text_IO;

with Glib;
with Glib.Error;

with Gtk.Builder;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;

with Concorde.Paths;

package body Concorde.UI.Gtk_UI is

   type Root_Gtk_UI_Record is
     new UI_Interface with
      record
         null;
      end record;

   overriding procedure Start (UI : in out Root_Gtk_UI_Record);
   overriding procedure Stop
     (UI : in out Root_Gtk_UI_Record;
      Message : String);

   Local_Gtk_UI : aliased Root_Gtk_UI_Record;

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class);

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   ----------------
   -- Get_Gtk_UI --
   ----------------

   function Get_Gtk_UI
      return Concorde_UI
   is
   begin
      Local_Gtk_UI := (null record);
      return Local_Gtk_UI'Access;
   end Get_Gtk_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Gtk_UI_Record) is
      pragma Unreferenced (UI);
      Builder : Gtk.Builder.Gtk_Builder;
      UI_Path : constant String :=
                  Concorde.Paths.Config_File
                    ("ui/concorde.glade");
   begin
      Gtk.Main.Init;
      Gtk.Builder.Gtk_New (Builder);

      declare
         use type Glib.Guint;
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => UI_Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Error opening GUI definition: " & UI_Path
               & ": " & Glib.Error.Get_Message (Error));
            return;
         end if;
      end;

      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
           Gtk.Window.Gtk_Window
             (Builder.Get_Object ("Top_Level"));
      begin
         Main_Window.On_Destroy (Destroy_Handler'Access);
         Main_Window.Set_Hide_Titlebar_When_Maximized (True);
         Main_Window.Show_All;
         Main_Window.Maximize;
      end;

      Gtk.Main.Main;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (UI      : in out Root_Gtk_UI_Record;
      Message : String)
   is
      pragma Unreferenced (UI);
   begin
      Ada.Text_IO.Put_Line
        ("UI stopping: " & Message);
      Gtk.Main.Main_Quit;
   end Stop;

end Concorde.UI.Gtk_UI;
