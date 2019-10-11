private with Ada.Containers.Indefinite_Vectors;
private with WL.String_Maps;

with Concorde.Contexts;
with Concorde.Json;
with Concorde.Writers;

package Concorde.Commands is

   type Argument_List is private;

   No_Arguments : constant Argument_List;

   function Argument_Count
     (List : Argument_List)
      return Natural;

   function Argument
     (List   : Argument_List;
      Index  : Positive)
      return String
     with Pre => Index <= Argument_Count (List);

   function Contains
     (List : Argument_List;
      Name : String)
      return Boolean;

   function Argument
     (List : Argument_List;
      Name : String)
      return String
     with Pre => Contains (List, Name);

   function Argument
     (List          : Argument_List;
      Name          : String;
      Default_Value : String)
      return String;

   type Root_Concorde_Command is abstract tagged private;

   function Administrator_Only
     (Command : Root_Concorde_Command)
      return Boolean;

   procedure Perform
     (Command   : Root_Concorde_Command;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Concorde.Writers.Writer_Interface'Class;
      Arguments : Argument_List)
   is abstract;

   procedure Execute
     (Command   : Root_Concorde_Command'Class;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Concorde.Writers.Writer_Interface'Class;
      Arguments : Argument_List);

   procedure Execute_Command_Line
     (Line    : String;
      Context   : in out Concorde.Contexts.Context_Type;
      Writer    : in out Concorde.Writers.Writer_Interface'Class);

   procedure Register
     (Command_Name : String;
      Command      : Root_Concorde_Command'Class);

private

   type Root_Concorde_Command is abstract tagged null record;

   function Administrator_Only
     (Command : Root_Concorde_Command)
      return Boolean
   is (False);

   package Argument_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Argument_Maps is
     new WL.String_Maps (String);

   type Argument_List is
      record
         Vector : Argument_Vectors.Vector;
         Map    : Argument_Maps.Map;
      end record;

   No_Arguments : constant Argument_List := (others => <>);

   function Argument_Count
     (List : Argument_List)
      return Natural
   is (List.Vector.Last_Index);

   function Contains
     (List : Argument_List;
      Name : String)
      return Boolean
   is (List.Map.Contains (Name));

   function Argument
     (List   : Argument_List;
      Index  : Positive)
      return String
   is (List.Vector.Element (Index));

   function Argument
     (List : Argument_List;
      Name : String)
      return String
   is (List.Map.Element (Name));

   function Argument
     (List          : Argument_List;
      Name          : String;
      Default_Value : String)
      return String
   is (if Contains (List, Name)
       then Argument (List, Name)
       else Default_Value);

end Concorde.Commands;
