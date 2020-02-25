with Ada.Containers.Indefinite_Holders;
with Ada.Text_IO;

with Glib;

with Gdk.Event;
with Gdk.Window;

with Gtk.Drawing_Area;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;

with Cairo;

with Concorde.Db.Faction;
with Concorde.Handles.Faction;

with Concorde.UI.Views.Galaxy;
with Concorde.UI.Cairo_Views;

package body Concorde.UI.Gtk_UI is

   package View_Object_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.UI.Views.View_Object_Interface'Class,
        Concorde.UI.Views."=");

   type Root_Gtk_UI_Record is
     new UI_Interface with
      record
         Window     : Gtk.Window.Gtk_Window;
         Draw       : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Surface    : Cairo.Cairo_Surface := Cairo.Null_Surface;
         View       : Concorde.UI.Cairo_Views.Cairo_View;
         Top_Object : View_Object_Holders.Holder;
         Width      : Natural := 0;
         Height     : Natural := 0;
      end record;

   overriding procedure Start (UI : in out Root_Gtk_UI_Record);
   overriding procedure Stop
     (UI : in out Root_Gtk_UI_Record;
      Message : String);

   Local_Gtk_UI : aliased Root_Gtk_UI_Record;

   function Configure_Handler
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   function Draw_Handler
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean;

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Clear
     (Surface : Cairo.Cairo_Surface);

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Surface : Cairo.Cairo_Surface)
   is
      Cr : constant Cairo.Cairo_Context := Cairo.Create (Surface);
   begin
      Cairo.Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Cairo.Paint (Cr);
      Cairo.Destroy (Cr);
   end Clear;

   -----------------------
   -- Configure_Handler --
   -----------------------

   function Configure_Handler
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      use Glib;
      use type Cairo.Cairo_Surface;
      Width  : constant Gint := Event.Width;
      Height : constant Gint := Event.Height;
      UI     : Root_Gtk_UI_Record renames Local_Gtk_UI;
   begin

      if UI.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (UI.Surface);
      end if;

      UI.Width := Natural (Width);
      UI.Height := Natural (Height);

      UI.Surface :=
        Gdk.Window.Create_Similar_Surface
           (Self.Get_Window,
            Cairo.Cairo_Content_Color,
            Self.Get_Allocated_Width,
            Self.Get_Allocated_Height);
      Clear (UI.Surface);

      UI.View.Set_Size (UI.Width, UI.Height);
      UI.View.Draw;

      declare
         Cr : constant Cairo.Cairo_Context :=
           Cairo.Create (UI.Surface);
      begin
         UI.View.Paint (Cr, 0.0, 0.0);
         Cairo.Destroy (Cr);
      end;

      return True;

   end Configure_Handler;

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

   ------------------
   -- Draw_Handler --
   ------------------

   function Draw_Handler
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Cairo.Set_Source_Surface (Cr, Local_Gtk_UI.Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return False;
   end Draw_Handler;

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

      Gtk.Drawing_Area.Gtk_New (UI.Draw);
      UI.Window.Add (UI.Draw);

      UI.Draw.On_Configure_Event (Configure_Handler'Access);
      UI.Draw.On_Draw (Draw_Handler'Access);

      UI.Top_Object :=
        View_Object_Holders.To_Holder
          (Concorde.UI.Views.Galaxy.Galaxy_View
             (Concorde.Handles.Faction.Get
                (Concorde.Db.Faction.First_Reference_By_Top_Record
                     (Concorde.Db.R_Faction))));

      UI.View :=
        Concorde.UI.Cairo_Views.Create_View
          (UI.Top_Object.Element);

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
