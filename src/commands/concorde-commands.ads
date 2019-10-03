private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Vectors;
private with WL.String_Maps;

with Concorde.Sessions;

package Concorde.Commands is

   type Writer_Interface is limited interface;

   procedure Put
     (Writer : in out Writer_Interface;
      Text   : String)
   is abstract;

   procedure New_Line
     (Writer : in out Writer_Interface)
   is abstract;

   procedure Put_Error
     (Writer  : in out Writer_Interface;
      Message : String)
   is abstract;

   procedure Put_Line
     (Writer : in out Writer_Interface'Class;
      Text   : String);

   type Identifier_List is private;

   procedure Put_Identifier_List
     (Writer : in out Writer_Interface'Class;
      List   : Identifier_List);

   procedure Add
     (To         : in out Identifier_List;
      Identifier : String);

   function Null_Writer return Writer_Interface'Class;

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
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List)
   is abstract;

   procedure Execute
     (Command   : Root_Concorde_Command'Class;
      Session   : in out Concorde.Sessions.Concorde_Session;
      Writer    : in out Writer_Interface'Class;
      Arguments : Argument_List);

   procedure Execute_Command_Line
     (Line    : String;
      Session : in out Concorde.Sessions.Concorde_Session;
      Writer  : in out Writer_Interface'Class);

   procedure Register
     (Command_Name : String;
      Command      : Root_Concorde_Command'Class);

private

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Identifier_List is
      record
         List : String_Lists.List;
      end record;

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
