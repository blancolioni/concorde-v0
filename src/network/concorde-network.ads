with Concorde.Nodes;

with Concorde.Handles.Calculation;
with Concorde.Handles.Network;

package Concorde.Network is

   type Network_Handle is private;

   function Current_Value
     (Network : Network_Handle;
      Tag     : String)
      return Real;

   procedure Update_Value
     (Network   : Network_Handle;
      Tag       : String;
      New_Value : Real);

   function Get_Handle
     (Network : Network_Handle;
      Node    : Concorde.Nodes.Node_Handle)
      return Concorde.Nodes.Value_Handle;

   function Get_Handle
     (Network : Network_Handle;
      Tag     : String)
      return Concorde.Nodes.Value_Handle;

   function Get_Handle
     (Network : Concorde.Handles.Network.Network_Class;
      Node    : Concorde.Nodes.Node_Handle)
      return Concorde.Nodes.Value_Handle;

   function Get_Handle
     (Network : Concorde.Handles.Network.Network_Class;
      Tag     : String)
      return Concorde.Nodes.Value_Handle;

   function Get_Network_Handle
     (Network : Concorde.Handles.Network.Network_Class)
      return Network_Handle;

   procedure Create_Initial_Network
     (Network : Concorde.Handles.Network.Network_Class;
      Initial_Value : not null access
        function (Tag : String) return Real);

   procedure Update_Network (Network : Network_Handle);

   procedure Load_Network;

   procedure Save_Network;

   function Evaluate
     (Network     : Concorde.Handles.Network.Network_Class;
      Calculation : Concorde.Handles.Calculation.Calculation_Class)
      return Real;

private

   type Network_Handle is new Positive;

   function Get_Value_Container
     (Handle : Network_Handle)
      return Concorde.Nodes.Value_Container;

   function Current_Value
     (Network : Network_Handle;
      Tag     : String)
      return Real
   is (Concorde.Nodes.Current_Value
       (Get_Value_Container (Network), Tag));

   function Get_Handle
     (Network : Network_Handle;
      Node    : Concorde.Nodes.Node_Handle)
      return Concorde.Nodes.Value_Handle
   is (Concorde.Nodes.Get_Handle
       (Get_Value_Container (Network), Node));

   function Get_Handle
     (Network : Network_Handle;
      Tag     : String)
      return Concorde.Nodes.Value_Handle
   is (Concorde.Nodes.Get_Handle
       (Get_Value_Container (Network), Tag));

   function Get_Handle
     (Network : Concorde.Handles.Network.Network_Class;
      Node    : Concorde.Nodes.Node_Handle)
      return Concorde.Nodes.Value_Handle
   is (Concorde.Nodes.Get_Handle
       (Get_Value_Container (Get_Network_Handle (Network)), Node));

   function Get_Handle
     (Network : Concorde.Handles.Network.Network_Class;
      Tag     : String)
      return Concorde.Nodes.Value_Handle
   is (Concorde.Nodes.Get_Handle
       (Get_Value_Container (Get_Network_Handle (Network)), Tag));

end Concorde.Network;
