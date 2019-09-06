with Concorde.Money;

with Concorde.Db.Agent;

package Concorde.Agents is

   function Cash
     (Account : Concorde.Db.Account_Reference)
      return Concorde.Money.Money_Type;

   function Cash
     (Agent : Concorde.Db.Agent.Agent_Type)
      return Concorde.Money.Money_Type;

   procedure Add_Cash
     (Agent : Concorde.Db.Agent.Agent_Type;
      Cash  : Concorde.Money.Money_Type);

   procedure Add_Cash
     (Account : Concorde.Db.Account_Reference;
      Cash    : Concorde.Money.Money_Type);

   procedure Remove_Cash
     (Account : Concorde.Db.Account_Reference;
      Cash    : Concorde.Money.Money_Type);

   procedure Move_Assets
     (From     : Concorde.Db.Agent.Agent_Type;
      To       : Concorde.Db.Agent.Agent_Type;
      Fraction : Unit_Real);

   procedure Log_Agent
     (Agent   : Concorde.Db.Agent_Reference;
      Message : String);

end Concorde.Agents;
