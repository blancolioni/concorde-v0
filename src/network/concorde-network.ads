with Concorde.Db;

package Concorde.Network is

   function Current_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Real;

   function Current_Value
     (Network : Concorde.Db.Network_Reference;
      Node    : Concorde.Db.Node_Reference)
      return Real;

   function Inertial_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String;
      Inertia : Non_Negative_Real)
      return Real;

   function Inertial_Value
     (Network : Concorde.Db.Network_Reference;
      Node    : Concorde.Db.Node_Reference;
      Inertia : Non_Negative_Real)
      return Real;

   procedure Create_Initial_Network
     (Network : Concorde.Db.Network_Reference;
      Initial_Value : not null access
        function (Tag : String) return Real);

   procedure Set_New_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String;
      Value   : Real);

   procedure Commit_New_Value
     (Network    : Concorde.Db.Network_Reference;
      Tag        : String);

   function Evaluate
     (Network     : Concorde.Db.Network_Reference;
      Calculation : Concorde.Db.Calculation_Reference)
      return Real;

   procedure Load_Network;

   procedure Update
     (Network : Concorde.Db.Network_Reference);

end Concorde.Network;
