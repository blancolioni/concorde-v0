with Ada.Numerics;
with Ada.Text_IO;

with Gdk.Event;
with Gdk.Window;

package body Concorde.UI.Gtk_Views is

   Pi : constant := Ada.Numerics.Pi;

   type Move_Command is new Root_Gtk_Draw_Command with
      record
         X, Y : Glib.Gdouble;
      end record;

   overriding procedure Execute
     (Command : Move_Command;
      Context : in out Draw_Context);

   type Line_Command is new Root_Gtk_Draw_Command with
      record
         X, Y : Glib.Gdouble;
      end record;

   overriding procedure Execute
     (Command : Line_Command;
      Context : in out Draw_Context);

   type Arc_Command is new Root_Gtk_Draw_Command with
      record
         Radius : Glib.Gdouble;
         Start  : Glib.Gdouble;
         Finish : Glib.Gdouble;
      end record;

   overriding procedure Execute
     (Command : Arc_Command;
      Context : in out Draw_Context);

   type Color_Command is new Root_Gtk_Draw_Command with
      record
         Color : Concorde.Color.Concorde_Color;
      end record;

   overriding procedure Execute
     (Command : Color_Command;
      Context : in out Draw_Context);

   type Render_Command is new Root_Gtk_Draw_Command with
      record
         Fill     : Boolean;
         Preserve : Boolean;
      end record;

   overriding procedure Execute
     (Command : Render_Command;
      Context : in out Draw_Context);

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

   ---------
   -- Arc --
   ---------

   overriding function Arc
     (Factory : Gtk_Command_Factory;
      Radius  : Non_Negative_Real;
      Start   : Real := 0.0;
      Finish  : Real := 360.0)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Arc_Command'
        (Radius => Glib.Gdouble (Radius),
         Start  => Glib.Gdouble (Start * Pi / 180.0),
         Finish => Glib.Gdouble (Finish * Pi / 180.0));
   end Arc;

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

   -----------
   -- Color --
   -----------

   overriding function Color
     (Factory : Gtk_Command_Factory;
      To      : Concorde.Color.Concorde_Color)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Color_Command'
        (Color => To);
   end Color;

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
      View   : constant Gtk_View_Widget := Gtk_View_Widget (Self);
   begin
      View.Width := Natural (Width);
      View.Height := Natural (Height);

      Ada.Text_IO.Put_Line
        ("view:" & Width'Image & Height'Image);

      if View.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (View.Surface);
      end if;

      View.Surface :=
        Gdk.Window.Create_Similar_Surface
           (Self.Get_Window,
            Cairo.Cairo_Content_Color,
            Self.Get_Allocated_Width,
            Self.Get_Allocated_Height);
      Clear (View.Surface);

      return True;

   end Configure_Handler;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      use type Cairo.Cairo_Surface;
      View   : constant Gtk_View_Widget := Gtk_View_Widget (W);
   begin
      if View.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (View.Surface);
      end if;
   end Destroy_Handler;

   ------------------
   -- Draw_Handler --
   ------------------

   function Draw_Handler
     (Self : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr   : Cairo.Cairo_Context)
      return Boolean
   is
      View   : constant Gtk_View_Widget := Gtk_View_Widget (Self);
   begin
      Cairo.Set_Source_Surface (Cr, View.Surface, 0.0, 0.0);
      Cairo.Paint (Cr);
      return False;
   end Draw_Handler;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Arc_Command;
      Context : in out Draw_Context)
   is
   begin
      Cairo.Arc
        (Cr     => Context.Cr,
         Xc     => Context.X,
         Yc     => Context.Y,
         Radius => Command.Radius,
         Angle1 => Command.Start,
         Angle2 => Command.Finish);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Color_Command;
      Context : in out Draw_Context)
   is
   begin
      Cairo.Set_Source_Rgba
        (Context.Cr,
         Glib.Gdouble (Command.Color.Red),
         Glib.Gdouble (Command.Color.Green),
         Glib.Gdouble (Command.Color.Blue),
         Glib.Gdouble (Command.Color.Alpha));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Line_Command;
      Context : in out Draw_Context)
   is
   begin
      Cairo.Line_To (Context.Cr, Command.X, Command.Y);
      Context.X := Command.X;
      Context.Y := Command.Y;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Move_Command;
      Context : in out Draw_Context)
   is
   begin
      Cairo.Move_To (Context.Cr, Command.X, Command.Y);
      Context.X := Command.X;
      Context.Y := Command.Y;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Render_Command;
      Context : in out Draw_Context)
   is
   begin
      if Command.Fill then
         if Command.Preserve then
            Cairo.Fill_Preserve (Context.Cr);
         else
            Cairo.Fill (Context.Cr);
         end if;
      else
         if Command.Preserve then
            Cairo.Stroke_Preserve (Context.Cr);
         else
            Cairo.Stroke (Context.Cr);
         end if;
      end if;
   end Execute;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (View : out Gtk_View_Widget) is
   begin
      View := Gtk_View_Widget_New;
   end Gtk_New;

   -------------------------
   -- Gtk_View_Widget_New --
   -------------------------

   function Gtk_View_Widget_New return Gtk_View_Widget is
   begin
      return Widget : constant Gtk_View_Widget := new Gtk_View_Widget_Record do
         Initialize (Widget);
      end return;
   end Gtk_View_Widget_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (View : not null access Gtk_View_Widget_Record'Class)
   is
   begin
      Gtk.Drawing_Area.Initialize (View);
      View.On_Configure_Event (Configure_Handler'Access);
      View.On_Draw (Draw_Handler'Access);
      View.On_Destroy (Destroy_Handler'Access);
   end Initialize;

   -------------
   -- Line_To --
   -------------

   overriding function Line_To
     (Factory : Gtk_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Line_Command'
        (X => Glib.Gdouble (X),
         Y => Glib.Gdouble (Y));
   end Line_To;

   -------------
   -- Move_To --
   -------------

   overriding function Move_To
     (Factory : Gtk_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Move_Command'
        (X => Glib.Gdouble (X),
         Y => Glib.Gdouble (Y));
   end Move_To;

   ------------
   -- Render --
   ------------

   overriding function Render
     (Factory  : Gtk_Command_Factory;
      Fill     : Boolean := False;
      Preserve : Boolean := False)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Render_Command'
        (Fill     => Fill,
         Preserve => Preserve);
   end Render;

end Concorde.UI.Gtk_Views;
