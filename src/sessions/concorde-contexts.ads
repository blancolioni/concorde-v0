private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with WL.String_Maps;

with Concorde.File_System;

package Concorde.Contexts is

   Context_Error           : exception;

   type Context_Type is tagged private;

   procedure Create_Context
     (Context       : in out Context_Type;
      Root          : Concorde.File_System.Node_Id;
      Default_Scope : String);

   function Current_Node
     (Context : Context_Type)
     return Concorde.File_System.Node_Interface'Class;

   function Find_Node
     (Context : Context_Type;
      Path    : String)
      return Concorde.File_System.Node_Id;

   procedure Set_Default_Scope
     (Context : in out Context_Type);

   function Change_Scope
     (Context : in out Context_Type;
      Path    : String)
     return Boolean;

   procedure New_Scope
     (Context : in out Context_Type;
      Scope   : String);

   procedure Bind
     (Context : in out Context_Type;
      Scope   : String;
      Node    : Concorde.File_System.Node_Interface'Class);

   procedure Push_Scope
     (Context : in out Context_Type);

   procedure Pop_Scope
     (Context : in out Context_Type);

   function Value
     (Context : Context_Type;
      Name    : String;
      Default : String := "")
      return String;

   procedure Set_Value
     (Context : in out Context_Type;
      Name    : String;
      Value   : String);

   function History_Length
     (Context : Context_Type)
      return Natural;

   function Get_History
     (Context : Context_Type;
      Offset  : Integer)
      return String
     with Pre => Offset /= 0 and then abs Offset < History_Length (Context);

   procedure Append_History
     (Context : in out Context_Type;
      Item    : String);

private

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package Scope_Vectors is
     new Ada.Containers.Vectors
       (Positive, String_Vectors.Vector, String_Vectors."=");

   package Environment_Maps is
     new WL.String_Maps (String);

   package Node_Id_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.File_System.Node_Id'Class,
        Concorde.File_System."=");

   package Node_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.File_System.Node_Interface'Class,
        Concorde.File_System."=");

   type Context_Type is tagged
      record
         History      : String_Vectors.Vector;
         Current_Path : String_Vectors.Vector;
         Home_Path    : String_Vectors.Vector;
         Environment  : Environment_Maps.Map;
         Scope_Stack  : Scope_Vectors.Vector;
         Root         : Node_Id_Holders.Holder;
      end record;

   procedure Set_Parent_Scope
     (Context : in out Context_Type);

   procedure Set_Child_Scope
     (Context    : in out Context_Type;
      Child_Name : String)
     with Pre => Current_Node (Context).Has_Child (Child_Name);

end Concorde.Contexts;
