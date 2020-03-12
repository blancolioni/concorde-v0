with Concorde.Calendar;

with Concorde.Db;

package Concorde.Network is

   function Current_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Real;

   function Previous_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Real;

   function Last_Change
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Concorde.Calendar.Time;

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

   procedure Update
     (Network : Concorde.Db.Network_Reference);

end Concorde.Network;
