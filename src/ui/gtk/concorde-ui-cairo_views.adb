with Ada.Numerics;

with Glib;
with Cairo.Image_Surface;

package body Concorde.UI.Cairo_Views is

   Pi : constant := Ada.Numerics.Pi;

   type Move_Command is new Root_Cairo_Draw_Command with
      record
         X, Y : Real;
      end record;

   overriding procedure Execute
     (Command : Move_Command;
      Context : in out Draw_Context);

   type Line_Command is new Root_Cairo_Draw_Command with
      record
         X, Y : Real;
      end record;

   overriding procedure Execute
     (Command : Line_Command;
      Context : in out Draw_Context);

   type Arc_Command is new Root_Cairo_Draw_Command with
      record
         Radius : Concorde.UI.Views.Measurement;
         Start  : Glib.Gdouble;
         Finish : Glib.Gdouble;
      end record;

   overriding procedure Execute
     (Command : Arc_Command;
      Context : in out Draw_Context);

   type Color_Command is new Root_Cairo_Draw_Command with
      record
         Color : Concorde.Color.Concorde_Color;
      end record;

   overriding procedure Execute
     (Command : Color_Command;
      Context : in out Draw_Context);

   type Render_Command is new Root_Cairo_Draw_Command with
      record
         Fill     : Boolean;
         Preserve : Boolean;
      end record;

   overriding procedure Execute
     (Command : Render_Command;
      Context : in out Draw_Context);

   procedure Clear
     (Surface : Cairo.Cairo_Surface;
      Color   : Concorde.Color.Concorde_Color);

   procedure Get_Cairo_XY
     (Context           : Draw_Context;
      View_X, View_Y    : Real;
      Cairo_X, Cairo_Y  : out Glib.Gdouble);

   procedure Get_Cairo_XY
     (Context           : Draw_Context;
      Cairo_X, Cairo_Y  : out Glib.Gdouble);

   ---------
   -- Arc --
   ---------

   overriding function Arc
     (Factory : Cairo_Command_Factory;
      Radius  : Concorde.UI.Views.Measurement;
      Start   : Real := 0.0;
      Finish  : Real := 360.0)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Arc_Command'
        (Radius => Radius,
         Start  => Glib.Gdouble (Start * Pi / 180.0),
         Finish => Glib.Gdouble (Finish * Pi / 180.0));
   end Arc;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Surface : Cairo.Cairo_Surface;
      Color   : Concorde.Color.Concorde_Color)
   is
      Cr : constant Cairo.Cairo_Context := Cairo.Create (Surface);
   begin
      Cairo.Set_Source_Rgba
        (Cr,
         Glib.Gdouble (Color.Red), Glib.Gdouble (Color.Green),
         Glib.Gdouble (Color.Blue), Glib.Gdouble (Color.Alpha));
      Cairo.Paint (Cr);
      Cairo.Destroy (Cr);
   end Clear;

   -----------
   -- Color --
   -----------

   overriding function Color
     (Factory : Cairo_Command_Factory;
      To      : Concorde.Color.Concorde_Color)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Color_Command'
        (Color => To);
   end Color;

   -----------------
   -- Create_View --
   -----------------

   function Create_View return Cairo_View is
   begin
      return new Cairo_View_Record;
   end Create_View;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (View   : in out Cairo_View_Record;
      Object : Concorde.UI.Views.View_Object_Interface'Class)
   is
      Factory  : Cairo_Command_Factory;
      Commands : Concorde.UI.Views.Draw_Command_List;
      Context  : Draw_Context :=
        Draw_Context'
          (Cr     => Cairo.Create (View.Surface),
           others => <>);

      procedure Execute (Command : Concorde.UI.Views.Draw_Command'Class);

      -------------
      -- Execute --
      -------------

      procedure Execute (Command : Concorde.UI.Views.Draw_Command'Class) is
         Cairo_Command : Root_Cairo_Draw_Command'Class renames
           Root_Cairo_Draw_Command'Class (Command);
      begin
         Cairo_Command.Execute (Context);
      end Execute;

   begin
      Cairo.Translate
        (Context.Cr,
         Glib.Gdouble (Real (View.Width) / 2.0 - View.Centre_X),
         Glib.Gdouble (Real (View.Height) / 2.0 - View.Centre_Y));
      Context.Scale := View.Scale;

      Clear (View.Surface, Object.Background_Color);
      Object.Get_Draw_Commands (Factory, Commands);
      Commands.Iterate (Execute'Access);
   end Draw;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Arc_Command;
      Context : in out Draw_Context)
   is
      R : constant Non_Negative_Real :=
        Concorde.UI.Views.Measure
          (Command.Radius, Context.Scale);
      Xc, Yc : Glib.Gdouble;
   begin
      Get_Cairo_XY (Context, Xc, Yc);
      Cairo.Arc
        (Cr     => Context.Cr,
         Xc     => Xc,
         Yc     => Yc,
         Radius => Glib.Gdouble (R),
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
      Xc, Yc : Glib.Gdouble;
   begin
      Get_Cairo_XY (Context, Command.X, Command.Y, Xc, Yc);
      Cairo.Line_To (Context.Cr, Xc, Yc);
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
      Xc, Yc : Glib.Gdouble;
   begin
      Get_Cairo_XY (Context, Command.X, Command.Y, Xc, Yc);
      Cairo.Move_To (Context.Cr, Xc, Yc);
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

   ------------------
   -- Get_Cairo_XY --
   ------------------

   procedure Get_Cairo_XY
     (Context           : Draw_Context;
      Cairo_X, Cairo_Y  : out Glib.Gdouble)
   is
   begin
      Get_Cairo_XY (Context, Context.X, Context.Y, Cairo_X, Cairo_Y);
   end Get_Cairo_XY;

   ------------------
   -- Get_Cairo_XY --
   ------------------

   procedure Get_Cairo_XY
     (Context           : Draw_Context;
      View_X, View_Y    : Real;
      Cairo_X, Cairo_Y  : out Glib.Gdouble)
   is
   begin
      Cairo_X := Glib.Gdouble (View_X * Context.Scale);
      Cairo_Y := Glib.Gdouble (View_Y * Context.Scale);
   end Get_Cairo_XY;

   -------------
   -- Line_To --
   -------------

   overriding function Line_To
     (Factory : Cairo_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Line_Command'(X, Y);
   end Line_To;

   -------------
   -- Move_To --
   -------------

   overriding function Move_To
     (Factory : Cairo_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Move_Command'(X, Y);
   end Move_To;

   -----------
   -- Paint --
   -----------

   procedure Paint
     (View : in out Cairo_View_Record'Class;
      Cr   : Cairo.Cairo_Context;
      X, Y : Real)
   is
   begin
      Cairo.Set_Source_Surface
        (Cr, View.Surface, Glib.Gdouble (X), Glib.Gdouble (Y));
      Cairo.Paint (Cr);
   end Paint;

   ------------
   -- Render --
   ------------

   overriding function Render
     (Factory  : Cairo_Command_Factory;
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

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (View   : in out Cairo_View_Record'Class;
      Width  : Natural;
      Height : Natural)
   is
      use type Cairo.Cairo_Surface;
   begin
      if Width = View.Width and then Height = View.Height then
         return;
      end if;

      if View.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (View.Surface);
      end if;

      View.Width := Width;
      View.Height := Height;

      View.Surface :=
        Cairo.Image_Surface.Create
          (Format => Cairo.Image_Surface.Cairo_Format_ARGB32,
           Width  => Glib.Gint (Width),
           Height => Glib.Gint (Height));
   end Set_Size;

end Concorde.UI.Cairo_Views;
