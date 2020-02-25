private with Glib;
private with Gtk.Drawing_Area;
private with Cairo;
private with Concorde.Color;

with Gtk.Widget;

with Concorde.UI.Views;

private package Concorde.UI.Gtk_Views is

   type Gtk_View_Widget_Record is
     new Gtk.Widget.Gtk_Widget_Record
     and Concorde.UI.Views.View_Interface
   with private;

   type Gtk_View_Widget is access all Gtk_View_Widget_Record'Class;

   procedure Gtk_New (View : out Gtk_View_Widget);

   procedure Initialize
     (View : not null access Gtk_View_Widget_Record'Class);

   function Gtk_View_Widget_New return Gtk_View_Widget;

   type Gtk_Command_Factory is
     new Concorde.UI.Views.Draw_Command_Factory with private;

private

   type Gtk_View_Widget_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     and Concorde.UI.Views.View_Interface with
      record
         Commands : Concorde.UI.Views.Draw_Command_List;
         Width    : Natural := 0;
         Height   : Natural := 0;
         Surface  : Cairo.Cairo_Surface := Cairo.Null_Surface;
      end record;

   type Gtk_Command_Factory is
     new Concorde.UI.Views.Draw_Command_Factory with null record;

   overriding function Move_To
     (Factory : Gtk_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command;

   overriding function Line_To
     (Factory : Gtk_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command;

   overriding function Arc
     (Factory : Gtk_Command_Factory;
      Radius  : Non_Negative_Real;
      Start   : Real := 0.0;
      Finish  : Real := 360.0)
      return Concorde.UI.Views.Draw_Command;

   overriding function Render
     (Factory  : Gtk_Command_Factory;
      Fill     : Boolean := False;
      Preserve : Boolean := False)
      return Concorde.UI.Views.Draw_Command;

   overriding function Color
     (Factory : Gtk_Command_Factory;
      To      : Concorde.Color.Concorde_Color)
      return Concorde.UI.Views.Draw_Command;

   type Draw_Context is
      record
         Cr     : Cairo.Cairo_Context;
         X, Y   : Glib.Gdouble;
         Filled : Boolean := False;
      end record;

   type Root_Gtk_Draw_Command is
     abstract new Concorde.UI.Views.Draw_Command_Interface with null record;

   procedure Execute
     (Command : Root_Gtk_Draw_Command;
      Context : in out Draw_Context)
   is abstract;

end Concorde.UI.Gtk_Views;
