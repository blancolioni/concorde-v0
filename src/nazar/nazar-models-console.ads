private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;
private with WL.String_Maps;

private with Nazar.Interfaces.Text_Writer;
private with Nazar.Models.Scope;

with Nazar.Interfaces.Commands;
with Nazar.Interfaces.Hierarchy;
with Nazar.Interfaces.Strings;
with Nazar.Models.Text_Writer;

package Nazar.Models.Console is

   type Console_Command is
     new Nazar.Interfaces.Commands.Arguments_Interface
   with private;

   function Name (Command : Console_Command) return String;

   overriding function Argument_Count
     (Command : Console_Command)
      return Natural;

   overriding function Argument
     (Command : Console_Command;
      Index   : Positive)
      return String;

   overriding function Has_Binding
     (Command : Console_Command;
      Name    : String)
      return Boolean;

   overriding function Binding
     (Command : Console_Command;
      Name    : String)
      return String;

   type Root_Console_Model is
     new Nazar.Models.Text_Writer.Root_Text_Writer_Model
   with private;

   type Nazar_Console_Model is access all Root_Console_Model'Class;

   procedure Initialize
     (Model         : in out Root_Console_Model;
      Root          : Nazar.Interfaces.Hierarchy.Node_Reference_Class;
      Environment   : not null access Nazar.Interfaces.Strings
      .String_Environment_Interface'Class;
      Default_Scope : String);

   procedure Set_Command
     (Model   : in out Root_Console_Model;
      Name    : String;
      Command : Nazar.Interfaces.Commands.Command_Interface'Class);

   procedure Execute_Command_Line
     (Model : in out Root_Console_Model'Class;
      Line  : String);

   procedure Execute_Single_Command
     (Model : in out Root_Console_Model'Class;
      Line  : String);

   function History_Length
     (Model : Root_Console_Model'Class)
      return Natural;

   function Get_History
     (Model   : Root_Console_Model'Class;
      Offset  : Integer)
      return String
     with Pre => Offset /= 0 and then abs Offset <= History_Length (Model);

   procedure Append_History
     (Model : in out Root_Console_Model'Class;
      Item  : String);

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package String_Maps is
     new WL.String_Maps (String);

   type Console_Command is
     new Nazar.Interfaces.Commands.Arguments_Interface with
      record
         Valid   : Boolean := False;
         Command : Ada.Strings.Unbounded.Unbounded_String;
         Vector  : String_Vectors.Vector;
         Map     : String_Maps.Map;
      end record;

   procedure Execute
     (Model   : in out Root_Console_Model;
      Command : Nazar.Models.Console.Console_Command'Class);

   function Name (Command : Console_Command) return String
   is (Ada.Strings.Unbounded.To_String (Command.Command));

   overriding function Argument_Count
     (Command : Console_Command)
      return Natural
   is (Command.Vector.Last_Index);

   overriding function Argument
     (Command : Console_Command;
      Index   : Positive)
      return String
   is (Command.Vector (Index));

   overriding function Has_Binding
     (Command : Console_Command;
      Name    : String)
      return Boolean
   is (Command.Map.Contains (Name));

   overriding function Binding
     (Command : Console_Command;
      Name    : String)
      return String
   is (Command.Map.Element (Name));

   type Environment_Access is
     access all Nazar.Interfaces.Strings.String_Environment_Interface'Class;

   package Command_Maps is
     new WL.String_Maps
       (Nazar.Interfaces.Commands.Command_Interface'Class,
        Nazar.Interfaces.Commands."=");

   type Root_Console_Model is
     new Nazar.Models.Text_Writer.Root_Text_Writer_Model with
      record
         Scope       : Nazar.Models.Scope.Nazar_Scope_Model;
         History     : String_Vectors.Vector;
         Environment : Environment_Access;
         Commands    : Command_Maps.Map;
      end record;

   type Internal_Command is
     abstract new Nazar.Interfaces.Commands.Command_Interface with
      record
         Scope : Nazar.Models.Scope.Nazar_Scope_Model;
      end record;

   procedure Internal_Command_Version
     (Command_Name : String;
      Writer       : in out
        Nazar.Interfaces.Text_Writer.Text_Writer_Interface'Class);

   procedure Iterate_Lines
     (Lines   : String;
      Process : not null access
        procedure (Line : String));

end Nazar.Models.Console;
