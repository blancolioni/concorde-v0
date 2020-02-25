with Ada.Numerics;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib;
with Cairo.Image_Surface;

with Concorde.Real_Images;

package body Concorde.UI.Cairo_Views is

   Pi : constant := Ada.Numerics.Pi;

   function Image (R : Real) return String renames
     Concorde.Real_Images.Approximate_Image;

   type Move_Command is new Root_Cairo_Draw_Command with
      record
         Relative : Boolean;
         Move_X   : Concorde.UI.Views.Measurement;
         Move_Y   : Concorde.UI.Views.Measurement;
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

   type Font_Command is new Root_Cairo_Draw_Command with
      record
         Family : Ada.Strings.Unbounded.Unbounded_String;
         Size   : Non_Negative_Real;
         Bold   : Boolean;
         Italic : Boolean;
      end record;

   overriding procedure Execute
     (Command : Font_Command;
      Context : in out Draw_Context);

   type Text_Command is new Root_Cairo_Draw_Command with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Execute
     (Command : Text_Command;
      Context : in out Draw_Context);

   type Real_Property_Command is new Root_Cairo_Draw_Command with
      record
         Property : Concorde.UI.Views.Real_Property;
         Value    : Real;
      end record;

   overriding procedure Execute
     (Command : Real_Property_Command;
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

   type Stack_Command is new Root_Cairo_Draw_Command with
      record
         Push : Boolean;
      end record;

   overriding procedure Execute
     (Command : Stack_Command;
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

   function Create_View
     (Object : Concorde.UI.Views.View_Object_Interface'Class)
      return Cairo_View
   is
      Initial_View : constant Concorde.UI.Views.View_Port :=
        Object.Initial_View;
   begin
      return new Cairo_View_Record'
        (View_Object => View_Object_Holders.To_Holder (Object),
         View_Port   => Initial_View,
         others      => <>);
   end Create_View;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (View   : in out Cairo_View_Record)
   is
      Factory  : Cairo_Command_Factory;
      Commands : Concorde.UI.Views.Draw_Command_List;
      Context  : Draw_Context :=
        Draw_Context'
          (Cr     => Cairo.Create (View.Surface),
           others => <>);

      Object : Concorde.UI.Views.View_Object_Interface'Class renames
        View.View_Object.Element;

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

      Centre_X : constant Real :=
        View.View_Port.Left + View.View_Port.Width / 2.0;
      Centre_Y : constant Real :=
        View.View_Port.Top + View.View_Port.Height / 2.0;

      Scale_X  : constant Real :=
        Real (View.Width) / View.View_Port.Width;
      Scale_Y  : constant Real :=
        Real (View.Height) / View.View_Port.Height;

   begin
      Context.Width := Real (View.Width);
      Context.Height := Real (View.Height);
      Context.Centre_X := Centre_X;
      Context.Centre_Y := Centre_Y;
      Context.Scale := Real'Min (Scale_X, Scale_Y);

      Ada.Text_IO.Put_Line
        ("draw: view size " & Image (Context.Width)
         & "x" & Image (Context.Height)
         & "; centre (" & Image (Centre_X) & "," & Image (Centre_Y) & ")"
         & "; scale x " & Image (Scale_X) & " y " & Image (Scale_Y));

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
        Concorde.UI.Views.Measure (Command.Radius);
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
      use Glib;
      Xc, Yc : Glib.Gdouble;
      DX     : constant Real := Concorde.UI.Views.Measure (Command.Move_X);
      DY     : constant Real := Concorde.UI.Views.Measure (Command.Move_Y);
   begin
      if not Command.Relative then
         Context.X := 0.0;
         Context.Y := 0.0;
      end if;

      if Concorde.UI.Views.Is_View (Command.Move_X) then
         Context.X := Context.X + DX;
      end if;

      if Concorde.UI.Views.Is_View (Command.Move_Y) then
         Context.Y := Context.Y + DY;
      end if;

      Get_Cairo_XY (Context, Context.X, Context.Y, Xc, Yc);

      if Concorde.UI.Views.Is_Screen (Command.Move_X) then
         Xc := Xc + Gdouble (DX);
      end if;

      if Concorde.UI.Views.Is_Screen (Command.Move_Y) then
         Yc := Yc + Gdouble (DY);
      end if;

      Cairo.Move_To (Context.Cr, Xc, Yc);

   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Font_Command;
      Context : in out Draw_Context)
   is
   begin
      Cairo.Select_Font_Face
        (Cr     => Context.Cr,
         Family => Ada.Strings.Unbounded.To_String (Command.Family),
         Slant  =>
           (if Command.Italic
            then Cairo.Cairo_Font_Slant_Italic
            else Cairo.Cairo_Font_Slant_Normal),
         Weight =>
           (if Command.Bold
            then Cairo.Cairo_Font_Weight_Bold
            else Cairo.Cairo_Font_Weight_Normal));
      Cairo.Set_Font_Size
        (Context.Cr, Glib.Gdouble (Command.Size));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Text_Command;
      Context : in out Draw_Context)
   is
   begin
      Cairo.Show_Text
        (Context.Cr, Ada.Strings.Unbounded.To_String (Command.Text));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Real_Property_Command;
      Context : in out Draw_Context)
   is
      use all type Concorde.UI.Views.Real_Property;
   begin
      case Command.Property is
         when Line_Width =>
            Cairo.Set_Line_Width (Context.Cr, Glib.Gdouble (Command.Value));
      end case;
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
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : Stack_Command;
      Context : in out Draw_Context)
   is
   begin
      if Command.Push then
         Cairo.Save (Context.Cr);
      else
         Cairo.Restore (Context.Cr);
      end if;
   end Execute;

   ----------
   -- Font --
   ----------

   overriding function Font
     (Factory  : Cairo_Command_Factory;
      Family   : String;
      Size     : Non_Negative_Real;
      Bold     : Boolean := False;
      Italic   : Boolean := False)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Font_Command'
        (Family => Ada.Strings.Unbounded.To_Unbounded_String (Family),
         Size   => Size,
         Bold   => Bold,
         Italic => Italic);
   end Font;

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
      use Glib;
   begin
      Cairo_X :=
        Gdouble ((View_X - Context.Centre_X) * Context.Scale)
      + Gdouble (Context.Width) / 2.0;
      Cairo_Y :=
        Gdouble ((View_Y - Context.Centre_Y) * Context.Scale)
          + Gdouble (Context.Height) / 2.0;
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

   overriding function Move
     (Factory  : Cairo_Command_Factory;
      Relative : Boolean;
      Move_X   : Concorde.UI.Views.Measurement;
      Move_Y   : Concorde.UI.Views.Measurement)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Move_Command'(Relative, Move_X, Move_Y);
   end Move;

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

   --------------
   -- Property --
   --------------

   overriding function Property
     (Factory : Cairo_Command_Factory;
      Prop    : Concorde.UI.Views.Real_Property;
      Value   : Real)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Real_Property_Command'
        (Property => Prop,
         Value    => Value);
   end Property;

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

   -------------
   -- Restore --
   -------------

   overriding function Restore
     (Factory  : Cairo_Command_Factory)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Stack_Command'
        (Push => False);
   end Restore;

   ----------
   -- Save --
   ----------

   overriding function Save
     (Factory  : Cairo_Command_Factory)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Stack_Command'
        (Push => True);
   end Save;

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

   ----------
   -- Text --
   ----------

   overriding function Text
     (Factory  : Cairo_Command_Factory;
      S        : String)
      return Concorde.UI.Views.Draw_Command
   is
      pragma Unreferenced (Factory);
   begin
      return Text_Command'
        (Text => Ada.Strings.Unbounded.To_Unbounded_String (S));
   end Text;

end Concorde.UI.Cairo_Views;
