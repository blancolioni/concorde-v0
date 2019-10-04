package Concorde.File_System is

   type Node_Id is private;

   No_Node_Id : constant Node_Id;

   type Node_Interface is interface;

   function Contents
     (Node : Node_Interface)
      return String
      is abstract;

   function Is_Leaf
     (Node : Node_Interface)
      return Boolean
      is abstract;

   function Has_Child
     (Node : Node_Interface;
      Name : String)
      return Boolean
      is abstract
     with Pre'Class => not Is_Leaf (Node);

   function Get_Child
     (Node  : Node_Interface;
      Child : String)
      return Node_Id
      is abstract
     with Pre'Class => not Is_Leaf (Node);

   procedure Bind_Child
     (Node  : in out Node_Interface;
      Name  : String;
      Child : Node_Id)
   is abstract
     with Pre'Class => not Is_Leaf (Node);

   procedure Delete_Child
     (Node   : in out Node_Interface;
      Name   : String)
   is abstract
     with Pre'Class => not Is_Leaf (Node) and then Has_Child (Node, Name);

   procedure Iterate_Children
     (Node    : Node_Interface;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Interface'Class))
   is abstract
     with Pre'Class => not Is_Leaf (Node);

   function Create
     (Node : Node_Interface'Class)
      return Node_Id;

   function Get
     (Id : Node_Id)
      return Node_Interface'Class;

   procedure Delete
     (Node : Node_Id);

   function Update
     (Node : Node_Id)
      return access Node_Interface'Class;

   type Leaf_Node is
     abstract new Node_Interface with private;

private

   type Node_Id is new Natural;

   No_Node_Id : constant Node_Id := 0;

   subtype Real_Node_Id is Node_Id range 1 .. Node_Id'Last;

   type Leaf_Node is
     abstract new Node_Interface with null record;

   overriding function Is_Leaf
     (Node : Leaf_Node)
      return Boolean;

   overriding function Has_Child
     (Node : Leaf_Node;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Leaf_Node;
      Child : String)
      return Node_Id;

   overriding procedure Bind_Child
     (Node  : in out Leaf_Node;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Leaf_Node;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Leaf_Node;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Interface'Class));

end Concorde.File_System;
