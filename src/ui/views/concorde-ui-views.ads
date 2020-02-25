private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Concorde.Color;

with Concorde.UI.Values;

package Concorde.UI.Views is

   type View_Port is
      record
         Left, Top     : Real;
         Width, Height : Non_Negative_Real;
      end record;

   type Measurement is private;

   function Absolute (M : Real) return Measurement;
   function Relative (M : Real) return Measurement;

   function Measure
     (M     : Measurement;
      Scale : Non_Negative_Real)
      return Real;

   type Draw_Command_Interface is interface;

   subtype Draw_Command is Draw_Command_Interface'Class;

   type Draw_Command_Factory is interface;

   function Move_To
     (Factory : Draw_Command_Factory;
      X, Y    : Real)
      return Draw_Command
      is abstract;

   function Line_To
     (Factory : Draw_Command_Factory;
      X, Y    : Real)
      return Draw_Command
      is abstract;

   function Arc
     (Factory : Draw_Command_Factory;
      Radius  : Measurement;
      Start   : Real := 0.0;
      Finish  : Real := 360.0)
      return Draw_Command
      is abstract;

   function Render
     (Factory  : Draw_Command_Factory;
      Fill     : Boolean := False;
      Preserve : Boolean := False)
      return Concorde.UI.Views.Draw_Command
      is abstract;

   function Color
     (Factory : Draw_Command_Factory;
      To      : Concorde.Color.Concorde_Color)
      return Draw_Command
      is abstract;

   function Color
     (Factory : Draw_Command_Factory'Class;
      Red     : Unit_Real;
      Green   : Unit_Real;
      Blue    : Unit_Real;
      Alpha   : Unit_Real := 1.0)
      return Draw_Command;

   type Draw_Command_List is tagged private;

   procedure Add
     (List    : in out Draw_Command_List'Class;
      Command : Draw_Command);

   procedure Iterate
     (List : Draw_Command_List'Class;
      Process : not null access
        procedure (Command : Draw_Command'Class));

   type View_Object_Interface is interface;

   function Display_Title
     (Object : View_Object_Interface)
      return String
      is abstract;

   function Display_Area
     (Object : View_Object_Interface)
      return View_Port
      is abstract;

   function Background_Color
     (Object : View_Object_Interface)
      return Concorde.Color.Concorde_Color
      is abstract;

   function Has_Changed
     (Object : View_Object_Interface)
      return Boolean
      is abstract;

   procedure Clear_Changed
     (Object : in out View_Object_Interface)
   is abstract;

   procedure Get_Draw_Commands
     (Object   : View_Object_Interface;
      Factory  : Draw_Command_Factory'Class;
      Commands : in out Draw_Command_List'Class)
   is abstract;

   procedure Iterate_Children
     (Object : View_Object_Interface;
      Process : not null access
        procedure (Child : View_Object_Interface'Class))
   is abstract;

   procedure Iterate_Properties
     (Object  : View_Object_Interface;
      Process : not null access
        procedure (Property_Name : String;
                   Property_Value : Concorde.UI.Values.Value_Type))
   is abstract;

   type View_Interface is interface;

   procedure Draw
     (View   : in out View_Interface;
      Object : View_Object_Interface'Class)
   is abstract;

private

   type Measurement_Type is (Absolute, Relative);

   type Measurement is
      record
         Measure : Measurement_Type;
         Value   : Real;
      end record;

   function Absolute (M : Real) return Measurement is (Absolute, M);

   function Relative (M : Real) return Measurement is (Relative, M);

   function Measure
     (M     : Measurement;
      Scale : Non_Negative_Real)
      return Real
   is (case M.Measure is
          when Absolute => M.Value,
          when Relative => M.Value * Scale);

   package Draw_Command_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Draw_Command'Class);

   type Draw_Command_List is tagged
      record
         Commands : Draw_Command_Lists.List;
      end record;

   function Color
     (Factory : Draw_Command_Factory'Class;
      Red     : Unit_Real;
      Green   : Unit_Real;
      Blue    : Unit_Real;
      Alpha   : Unit_Real := 1.0)
      return Draw_Command
   is (Factory.Color ((Red, Green, Blue, Alpha)));

end Concorde.UI.Views;
