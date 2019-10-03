private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with WL.String_Maps;

package Concorde.Contexts is

   type Context_Node_Interface is interface;

   function Contents
     (Node : Context_Node_Interface)
      return String
      is abstract;

   function Has_Children
     (Node : Context_Node_Interface)
      return Boolean
      is abstract;

   function Has_Child
     (Node : Context_Node_Interface;
      Name : String)
      return Boolean
      is abstract;

   function Get_Child
     (Node  : Context_Node_Interface;
      Child : String)
      return Context_Node_Interface'Class
      is abstract;

   procedure Iterate_Children
     (Node    : Context_Node_Interface;
      Process : not null access
        procedure (Name : String;
                   Child : Context_Node_Interface'Class))
   is abstract;

   function System_Root return Context_Node_Interface'Class;

   type Context_Type is tagged private;

   procedure Create_Context
     (Context       : in out Context_Type;
      Root          : Context_Node_Interface'Class;
      Default_Scope : String);

   function Current_Node
     (Context : Context_Type)
     return Context_Node_Interface'Class;

   procedure Set_Default_Scope
     (Context : in out Context_Type);

   function Change_Scope
     (Context : in out Context_Type;
      Path    : String)
     return Boolean;

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

   package Node_Holders is
     new Ada.Containers.Indefinite_Holders (Context_Node_Interface'Class);

   type Context_Type is tagged
      record
         History      : String_Vectors.Vector;
         Current_Path : String_Vectors.Vector;
         Home_Path    : String_Vectors.Vector;
         Environment  : Environment_Maps.Map;
         Scope_Stack  : Scope_Vectors.Vector;
         Root         : Node_Holders.Holder;
      end record;

   procedure Set_Parent_Scope
     (Context : in out Context_Type);

   procedure Set_Child_Scope
     (Context    : in out Context_Type;
      Child_Name : String)
     with Pre => Current_Node (Context).Has_Child (Child_Name);

end Concorde.Contexts;
