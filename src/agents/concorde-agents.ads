with Concorde.Money;

with Concorde.Handles.Account;
with Concorde.Handles.Agent;

package Concorde.Agents is

   function Cash
     (Agent : Concorde.Handles.Agent.Agent_Class)
      return Concorde.Money.Money_Type;

   procedure Add_Cash
     (Agent : Concorde.Handles.Agent.Agent_Class;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String);

   procedure Add_Cash
     (Account : Concorde.Handles.Account.Account_Class;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String);

   procedure Remove_Cash
     (Agent : Concorde.Handles.Agent.Agent_Class;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String);

   procedure Remove_Cash
     (Account : Concorde.Handles.Account.Account_Class;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String);

   function Limit_Cash
     (Account : Concorde.Handles.Account.Account_Class)
      return Concorde.Money.Money_Type;

   function New_Account
     (Starting_Balance : Concorde.Money.Money_Type;
      Guarantor        : Concorde.Handles.Account.Account_Class :=
        Concorde.Handles.Account.Empty_Handle)
      return Concorde.Handles.Account.Account_Handle;

   function Describe
     (Agent : Concorde.Handles.Agent.Agent_Class)
      return String;

   procedure Log_Agent
     (Agent   : Concorde.Handles.Agent.Agent_Class;
      Context : String;
      Message : String);

end Concorde.Agents;
