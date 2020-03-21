with Concorde.Network;

with Concorde.Db.Node;

package body Concorde.UI.Models.Network is

   type Network_Node_Model_Record is
     new Nazar.Models.Numeric.Nazar_Float_Model_Record
     and Concorde.Network.Node_Observer with
      record
         Network : Concorde.Db.Network_Reference;
         Node    : Concorde.Db.Node_Reference;
      end record;

   overriding procedure Notify
     (Observer : in out Network_Node_Model_Record;
      Network  : Concorde.Db.Network_Reference;
      Node     : Concorde.Db.Node_Reference);

   ------------------------
   -- Network_Node_Model --
   ------------------------

   function Network_Node_Model
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Nazar.Models.Numeric.Nazar_Float_Model
   is
      use Concorde.Db;
      Node : constant Concorde.Db.Node.Node_Type :=
        Concorde.Db.Node.Get_By_Tag (Tag);
      pragma Assert (Node.Has_Element);
      pragma Assert (Node.Content = Setting);
      Model : Network_Node_Model_Record :=
        Network_Node_Model_Record'
          (Nazar.Models.Numeric.Nazar_Float_Model_Record with
           Network => Network,
           Node    => Node.Get_Node_Reference);

      Value : constant Unit_Real :=
        Concorde.Network.Current_Value (Network, Node.Get_Node_Reference);
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
      Network  : Concorde.Db.Network_Reference;
      Node     : Concorde.Db.Node_Reference)
   is
   begin
      Observer.Set_Current
        (Nazar.Nazar_Float
           (100.0 * Concorde.Network.Current_Value (Network, Node)));
   end Notify;

end Concorde.UI.Models.Network;
