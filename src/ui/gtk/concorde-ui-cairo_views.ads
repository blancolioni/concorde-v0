private with Ada.Containers.Indefinite_Holders;
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

   function Create_View
     (Object : Concorde.UI.Views.View_Object_Interface'Class)
     return Cairo_View;

private

   package View_Object_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.UI.Views.View_Object_Interface'Class,
        Concorde.UI.Views."=");

   type Cairo_View_Record is
     new Concorde.UI.Views.View_Interface with
      record
         Commands    : Concorde.UI.Views.Draw_Command_List;
         Width       : Natural := 0;
         Height      : Natural := 0;
         Surface     : Cairo.Cairo_Surface := Cairo.Null_Surface;
         View_Object : View_Object_Holders.Holder;
         View_Port   : Concorde.UI.Views.View_Port;
      end record;

   overriding procedure Draw
     (View   : in out Cairo_View_Record);

   type Cairo_Command_Factory is
     new Concorde.UI.Views.Draw_Command_Factory with null record;

   overriding function Move
     (Factory  : Cairo_Command_Factory;
      Relative : Boolean;
      Move_X   : Concorde.UI.Views.Measurement;
      Move_Y   : Concorde.UI.Views.Measurement)
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

   overriding function Font
     (Factory  : Cairo_Command_Factory;
      Family   : String;
      Size     : Non_Negative_Real;
      Bold     : Boolean := False;
      Italic   : Boolean := False)
      return Concorde.UI.Views.Draw_Command;

   overriding function Text
     (Factory  : Cairo_Command_Factory;
      S        : String)
      return Concorde.UI.Views.Draw_Command;

   overriding function Save
     (Factory  : Cairo_Command_Factory)
      return Concorde.UI.Views.Draw_Command;

   overriding function Restore
     (Factory  : Cairo_Command_Factory)
      return Concorde.UI.Views.Draw_Command;

   overriding function Render
     (Factory  : Cairo_Command_Factory;
      Fill     : Boolean := False;
      Preserve : Boolean := False)
      return Concorde.UI.Views.Draw_Command;

   overriding function Property
     (Factory : Cairo_Command_Factory;
      Prop    : Concorde.UI.Views.Real_Property;
      Value   : Real)
      return Concorde.UI.Views.Draw_Command;

   overriding function Color
     (Factory : Cairo_Command_Factory;
      To      : Concorde.Color.Concorde_Color)
      return Concorde.UI.Views.Draw_Command;

   type Draw_Context is
      record
         Cr                 : Cairo.Cairo_Context;
         X, Y               : Real;
         Centre_X, Centre_Y : Real;
         Width, Height      : Real;
         Scale              : Non_Negative_Real := 1.0;
         Filled             : Boolean := False;
      end record;

   type Root_Cairo_Draw_Command is
     abstract new Concorde.UI.Views.Draw_Command_Interface with null record;

   procedure Execute
     (Command : Root_Cairo_Draw_Command;
      Context : in out Draw_Context)
   is abstract;

end Concorde.UI.Cairo_Views;
