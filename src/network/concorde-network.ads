with Concorde.Handles.Calculation;
with Concorde.Handles.Network;

package Concorde.Network is

   type Network_Type is private;

   type Network_Node_Type is private;

   type Network_Value_Type is private;

   function Current_Value
     (Network : Network_Type;
      Tag     : String)
      return Real;

   function Current_Value
     (Value : Network_Value_Type)
      return Real;

   function Inertial_Value
     (Value     : Network_Value_Type;
      Inertia   : Non_Negative_Real;
      Smoothing : Non_Negative_Real)
      return Real;

   function Inertial_Value
     (Network   : Network_Type;
      Node      : Network_Node_Type;
      Inertia   : Non_Negative_Real;
      Smoothing : Non_Negative_Real)
      return Real;

   procedure Update_Value
     (Network   : Network_Type;
      Tag       : String;
      New_Value : Real);

   procedure Update_Value
     (Value     : Network_Value_Type;
      New_Value : Real);

   function Get_Network_Value
     (Network : Network_Type;
      Node    : Network_Node_Type)
      return Network_Value_Type;

   function Get_Network_Value
     (Network : Network_Type;
      Tag     : String)
      return Network_Value_Type;

   function Get_Network_Value
     (Network : Concorde.Handles.Network.Network_Class;
      Tag     : String)
      return Network_Value_Type;

   function Get_Node
     (Tag     : String)
      return Network_Node_Type;

   function Get_Tag
     (Node : Network_Node_Type)
      return String;

   function Get_Network
     (From : Concorde.Handles.Network.Network_Class)
      return Network_Type;

   procedure Create_Initial_Network
     (Network : Concorde.Handles.Network.Network_Class;
      Initial_Value : not null access
        function (Tag : String) return Real);

   procedure Update_Network (Network : Network_Type);

   procedure Load_Network;

   procedure Save_Network;

   function Evaluate
     (Network     : Network_Type;
      Calculation : Concorde.Handles.Calculation.Calculation_Class)
      return Real;

   function Evaluate
     (Network     : Concorde.Handles.Network.Network_Class;
      Calculation : Concorde.Handles.Calculation.Calculation_Class)
      return Real;

   type Node_Observer is interface;

   procedure Notify
     (Observer : in out Node_Observer;
      Value    : Network_Value_Type)
   is abstract;

   procedure Add_Observer
     (Value    : Network_Value_Type;
      Observer : not null access Node_Observer'Class);

   procedure Remove_Observer
     (Value    : Network_Value_Type;
      Observer : not null access Node_Observer'Class);

private

   type Network_Record;

   type Network_Type is access Network_Record;

   type Network_Node_Record;

   type Network_Node_Type is access Network_Node_Record;

   type Network_Value_Record;

   type Network_Value_Type is access Network_Value_Record;

   function Current_Value
     (Network : Network_Type;
      Tag     : String)
      return Real
   is (Current_Value (Get_Network_Value (Network, Tag)));

   function Inertial_Value
     (Network   : Network_Type;
      Node      : Network_Node_Type;
      Inertia   : Non_Negative_Real;
      Smoothing : Non_Negative_Real)
      return Real
   is (Inertial_Value (Get_Network_Value (Network, Node), Inertia, Smoothing));

   function Get_Network_Value
     (Network : Network_Type;
      Tag     : String)
      return Network_Value_Type
   is (Get_Network_Value (Network, Get_Node (Tag)));

   function Get_Network_Value
     (Network : Concorde.Handles.Network.Network_Class;
      Tag     : String)
      return Network_Value_Type
   is (Get_Network_Value (Get_Network (Network), Get_Node (Tag)));

   function Evaluate
     (Network     : Concorde.Handles.Network.Network_Class;
      Calculation : Concorde.Handles.Calculation.Calculation_Class)
      return Real
   is (Evaluate (Get_Network (Network), Calculation));

end Concorde.Network;
