with Ada.Text_IO;

with Glib.Error;

with Gdk.Display;
with Gdk.Screen;

with Gtk.Css_Provider;
with Gtk.Main;
with Gtk.Style_Context;
with Gtk.Widget;
with Gtk.Window;

with Cairo;

with Nazar.Controllers.Draw;
with Nazar.Models.Draw;

with Nazar.Models.Layout;

with Nazar.Views.Gtk_Views.Console;
with Nazar.Views.Gtk_Views.Draw;
with Nazar.Views.Gtk_Views.Layout;

with Nazar.Controllers.Console;

with Concorde.UI.Models.Console;
with Concorde.UI.Models.Galaxy;

with Concorde.Handles.Faction;
with Concorde.Handles.Faction;

with Concorde.Options;
with Concorde.Paths;

package body Concorde.UI.Gtk_UI is

   type Root_Gtk_UI_Record is
     new UI_Interface with
      record
         Window         : Gtk.Window.Gtk_Window;
         Top_View       : Nazar.Views.Gtk_Views.Nazar_Gtk_View;
         Top_Model      : Nazar.Models.Nazar_Model;
         Top_Controller : Nazar.Controllers.Nazar_Controller;
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

      declare
         --  use type Glib.Error.GError;
         use Gtk.Css_Provider;
         Error    : aliased Glib.Error.GError;
         Theme_Name : constant String :=
           Concorde.Options.Display_Theme;
         Have_Theme : constant Boolean :=
           Theme_Name /= "";
         Theme : constant Gtk.Css_Provider.Gtk_Css_Provider :=
           (if Have_Theme
            then Gtk.Css_Provider.Get_Named (Theme_Name)
            else null);
         Override  : constant Gtk.Css_Provider.Gtk_Css_Provider :=
           Gtk.Css_Provider.Gtk_Css_Provider_New;
         Display    : constant Gdk.Display.Gdk_Display :=
           Gdk.Display.Get_Default;
         Screen   : constant Gdk.Screen.Gdk_Screen :=
           Gdk.Screen.Get_Default_Screen (Display);
      begin

         if not Gtk.Css_Provider.Load_From_Path
           (Override,
            Concorde.Paths.Config_File
              ("theme/gtk/concorde.css"),
            Error'Access)
         then
            Ada.Text_IO.Put_Line
              (Glib.Error.Get_Message (Error));
         end if;

         Gtk.Style_Context.Add_Provider_For_Screen
           (Screen   => Screen,
            Provider => +Theme,
            Priority => 600);

         Gtk.Style_Context.Add_Provider_For_Screen
           (Screen   => Screen,
            Provider => +Override,
            Priority => 700);

      end;

      declare
         use Nazar.Controllers.Console;
         use Nazar.Views.Gtk_Views.Console;
         use Nazar.Views.Gtk_Views.Layout;
         Console            : constant Models.Console.Concorde_Console_Model :=
                                Models.Console.Console_Model
                                  (Default_Scope => "/home");
         Galaxy_Model       : constant Nazar.Models.Draw.Nazar_Draw_Model :=
                                Concorde.UI.Models.Galaxy.Galaxy_Model
                                  (Concorde.Handles.Faction.Get
                                     (Concorde.Handles.Faction
                                      .First_By_Top_Record
                                        (Concorde.Handles.R_Faction)));

         Galaxy_Controller  : Nazar.Controllers.Draw
           .Nazar_Draw_Controller_Record;
         Galaxy_View        : constant Nazar.Views.Gtk_Views.Draw
           .Nazar_Gtk_Draw_View :=
             Nazar.Views.Gtk_Views.Draw.Gtk_Draw_View
               (Galaxy_Model);
         Console_View       : constant Nazar_Gtk_Console_View :=
                                Gtk_Console_View (Console);
         Console_Controller : Nazar_Console_Controller_Record;
         Layout_Model        : constant Nazar.Models.Layout
           .Nazar_Layout_Model :=
             Nazar.Models.Layout.Layout_Model_New;
         Layout_View        : constant Nazar_Gtk_Layout_View :=
                                Gtk_Layout_View (Layout_Model);
      begin
         Galaxy_Controller.Start_Draw (Galaxy_Model, Galaxy_View);
         Console_Controller.Start_Console (Console, Console_View);

         Layout_Model.Attach (Galaxy_View, 0, 12, 0, 8);
         Layout_Model.Attach (Console_View, 0, 6, 8, 12);

         UI.Top_View := Nazar.Views.Gtk_Views.Nazar_Gtk_View (Layout_View);
         UI.Top_Model := Nazar.Models.Nazar_Model (Layout_Model);
      end;

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
