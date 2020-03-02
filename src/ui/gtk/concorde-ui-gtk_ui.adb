with Ada.Text_IO;

with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;

with Cairo;

with Nazar.Controllers.Console;
with Nazar.Views.Gtk_Views.Console;

with Concorde.UI.Models.Console;

package body Concorde.UI.Gtk_UI is

   type Root_Gtk_UI_Record is
     new UI_Interface with
      record
         Window         : Gtk.Window.Gtk_Window;
         Top_View       : Nazar.Views.Gtk_Views.Console.Nazar_Gtk_Console_View;
         Top_Model      : Concorde.UI.Models.Console.Concorde_Console_Model;
         Top_Controller : Nazar.Controllers.Console.Root_Console_Controller;
         Surface        : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Width          : Natural := 0;
         Height         : Natural := 0;
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
      return UI_Type
   is
   begin
      Local_Gtk_UI := (others => <>);
      return Local_Gtk_UI'Access;
   end Get_Gtk_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Gtk_UI_Record) is
   begin
      Gtk.Main.Init;
      Gtk.Window.Gtk_New (UI.Window);
      UI.Window.On_Destroy (Destroy_Handler'Access);

      UI.Top_Model :=
        Concorde.UI.Models.Console.Console_Model;
      UI.Top_View :=
        Nazar.Views.Gtk_Views.Console.Gtk_Console_View
          (UI.Top_Model);
      UI.Top_Controller.Start_Console
        (UI.Top_Model, UI.Top_View);

      UI.Window.Add (UI.Top_View.Widget);

      UI.Window.Maximize;
      UI.Window.Show_All;

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
