private with Concorde.Color;

with Cairo;
with Concorde.UI.Views;

private package Concorde.UI.Cairo_Views is

   type Cairo_View_Record is
     new Concorde.UI.Views.View_Interface
   with private;

   procedure Set_Size
     (View   : in out Cairo_View_Record'Class;
      Width  : Natural;
      Height : Natural);

   procedure Paint
     (View : in out Cairo_View_Record'Class;
      Cr   : Cairo.Cairo_Context;
      X, Y : Real);

   type Cairo_View is access all Cairo_View_Record'Class;

   function Create_View return Cairo_View;

private

   type Cairo_View_Record is
     new Concorde.UI.Views.View_Interface with
      record
         Commands : Concorde.UI.Views.Draw_Command_List;
         Width    : Natural := 0;
         Height   : Natural := 0;
         Surface  : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Centre_X : Real    := 0.0;
         Centre_Y : Real    := 0.0;
         Scale    : Real    := 5.0;
      end record;

   overriding procedure Draw
     (View   : in out Cairo_View_Record;
      Object : Concorde.UI.Views.View_Object_Interface'Class);

   type Cairo_Command_Factory is
     new Concorde.UI.Views.Draw_Command_Factory with null record;

   overriding function Move_To
     (Factory : Cairo_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command;

   overriding function Line_To
     (Factory : Cairo_Command_Factory;
      X, Y    : Real)
      return Concorde.UI.Views.Draw_Command;

   overriding function Arc
     (Factory : Cairo_Command_Factory;
      Radius  : Concorde.UI.Views.Measurement;
      Start   : Real := 0.0;
      Finish  : Real := 360.0)
      return Concorde.UI.Views.Draw_Command;

   overriding function Render
     (Factory  : Cairo_Command_Factory;
      Fill     : Boolean := False;
      Preserve : Boolean := False)
      return Concorde.UI.Views.Draw_Command;

   overriding function Color
     (Factory : Cairo_Command_Factory;
      To      : Concorde.Color.Concorde_Color)
      return Concorde.UI.Views.Draw_Command;

   type Draw_Context is
      record
         Cr     : Cairo.Cairo_Context;
         X, Y   : Real;
         Scale  : Non_Negative_Real := 1.0;
         Filled : Boolean := False;
      end record;

   type Root_Cairo_Draw_Command is
     abstract new Concorde.UI.Views.Draw_Command_Interface with null record;

   procedure Execute
     (Command : Root_Cairo_Draw_Command;
      Context : in out Draw_Context)
   is abstract;

end Concorde.UI.Cairo_Views;
