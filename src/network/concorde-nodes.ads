with Concorde.Handles.Network;
with Concorde.Handles.Node;

package Concorde.Nodes is

   type Node_Handle is private;

   function Get_Handle
     (Node    : Concorde.Handles.Node.Node_Class)
      return Node_Handle;

   function Get_Handle
     (Tag : String)
      return Node_Handle;

   function Get_Tag
     (Handle : Node_Handle)
      return String;

   type Value_Container is private;

   function New_Container
     (Network : Concorde.Handles.Network.Network_Class)
     return Value_Container;

   type Value_Handle is private;

   function Current_Value
     (Node : Value_Handle)
      return Real;

   function Current_Value
     (Container : Value_Container;
      Node      : Node_Handle)
      return Real;

   function Current_Value
     (Container : Value_Container;
      Tag       : String)
      return Real;

   function Inertial_Value
     (Node      : Value_Handle;
      Inertia   : Non_Negative_Real;
      Smoothing : Non_Negative_Real)
      return Real;

   procedure Update_Value
     (Node      : Value_Handle;
      New_Value : Real);

   procedure Set_Value
     (Node      : Value_Handle;
      New_Value : Real);

   procedure Set_Value
     (Container : Value_Container;
      Node      : Node_Handle;
      New_Value : Real);

   procedure Commit_Value (Node : Value_Handle);

   procedure Commit_Value
     (Container : Value_Container;
      Node      : Node_Handle);

   procedure Update_Value
     (Container : Value_Container;
      Tag       : String;
      New_Value : Real);

   procedure Update_Value
     (Container : Value_Container;
      Node      : Node_Handle;
      New_Value : Real);

   function Get_Handle
     (Container : Value_Container;
      Node    : Node_Handle)
      return Value_Handle;

   function Get_Handle
     (Container : Value_Container;
      Tag     : String)
      return Value_Handle;

   function Node_Count return Natural;

   procedure Iterate
     (Container : Value_Container;
      Process   : not null access
        procedure (Node : Concorde.Handles.Node.Node_Handle;
                   Value : Real));

   type Node_Observer is interface;

   procedure Notify
     (Observer : in out Node_Observer;
      Handle   : Concorde.Nodes.Value_Handle;
      Value    : Real)
   is abstract;

   procedure Add_Observer
     (Handle   : Value_Handle;
      Observer : not null access Node_Observer'Class);

   procedure Remove_Observer
     (Handle   : Value_Handle;
      Observer : not null access Node_Observer'Class);

private

   type Value_Container is new Positive;

   type Node_Handle is new Positive;

   type Value_Handle is new Positive;

end Concorde.Nodes;
