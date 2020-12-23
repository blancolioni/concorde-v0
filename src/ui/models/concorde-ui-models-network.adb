with Concorde.Network;
with Concorde.Nodes;

package body Concorde.UI.Models.Network is

   type Network_Node_Model_Record is
     new Nazar.Models.Numeric.Nazar_Float_Model_Record
     and Concorde.Nodes.Node_Observer with
      record
         Handle : Concorde.Nodes.Value_Handle;
      end record;

   overriding procedure Notify
     (Observer : in out Network_Node_Model_Record;
      Handle   : Concorde.Nodes.Value_Handle;
      Value    : Real);

   ------------------------
   -- Network_Node_Model --
   ------------------------

   function Network_Node_Model
     (Network : Concorde.Handles.Network.Network_Class;
      Tag     : String)
      return Nazar.Models.Numeric.Nazar_Float_Model
   is
      Handle : constant Concorde.Nodes.Value_Handle :=
                 Concorde.Network.Get_Handle (Network, Tag);
      Model : Network_Node_Model_Record :=
        Network_Node_Model_Record'
                   (Nazar.Models.Numeric.Nazar_Float_Model_Record with
                    Handle => Handle);

      Value : constant Unit_Real :=
                Concorde.Nodes.Current_Value (Handle);
   begin
      Model.Initialize
        (Min           => 0.0,
         Max           => 100.0,
         Step          => 1.0,
         Initial_Value =>
           Nazar.Nazar_Float (Value * 100.0));
      return new Network_Node_Model_Record'(Model);

   end Network_Node_Model;

   ------------
   -- Notify --
   ------------

   overriding procedure Notify
     (Observer : in out Network_Node_Model_Record;
      Handle   : Concorde.Nodes.Value_Handle;
      Value    : Real)
   is
      pragma Unreferenced (Handle);
   begin
      Observer.Set_Current
        (Nazar.Nazar_Float
           (100.0 * Value));
   end Notify;

end Concorde.UI.Models.Network;
