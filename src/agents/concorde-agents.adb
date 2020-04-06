with Concorde.Calendar;
with Concorde.Logging;

with Concorde.Db.Account;
with Concorde.Db.Historical_Account;

package body Concorde.Agents is

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Agent : Concorde.Db.Agent.Agent_Type;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String)
   is
   begin
      Add_Cash (Agent.Account, Cash, Tag);
   end Add_Cash;

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Account : Concorde.Db.Account_Reference;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String)
   is
      use type Concorde.Money.Money_Type;
      Rec : constant Concorde.Db.Account.Account_Type :=
              Concorde.Db.Account.Get (Account);
      Old_Cash : constant Concorde.Money.Money_Type := Rec.Cash;
      New_Cash : constant Concorde.Money.Money_Type := Old_Cash + Cash;
   begin
      Concorde.Db.Account.Update_Account (Account)
        .Set_Cash (New_Cash)
        .Set_Earn (Rec.Earn + Cash)
        .Done;
      Concorde.Db.Historical_Account.Create
        (Account    => Account,
         Time_Stamp => Concorde.Calendar.Clock,
         Tag        => Tag,
         Change     => Cash,
         Cash       => New_Cash);
   end Add_Cash;

   ----------
   -- Cash --
   ----------

   function Cash
     (Account : Concorde.Db.Account_Reference)
      return Concorde.Money.Money_Type
   is
   begin
      return Concorde.Db.Account.Get (Account).Cash;
   end Cash;

   ----------
   -- Cash --
   ----------

   function Cash
     (Agent : Concorde.Db.Agent.Agent_Type)
      return Concorde.Money.Money_Type
   is
   begin
      return Cash (Agent.Account);
   end Cash;

   ---------------
   -- Log_Agent --
   ---------------

   procedure Log_Agent
     (Agent   : Concorde.Db.Agent_Reference;
      Message : String)
   is
   begin
      Concorde.Logging.Log
        (Actor    => "Agent" & Concorde.Db.To_String (Agent),
         Location => "",
         Category => "",
         Message  => Message);
   end Log_Agent;

   -----------------
   -- New_Account --
   -----------------

   function New_Account
     (Starting_Balance : Concorde.Money.Money_Type;
      Guarantor        : Concorde.Db.Account_Reference :=
        Concorde.Db.Null_Account_Reference)
      return Concorde.Db.Account_Reference
   is
   begin
      return Concorde.Db.Account.Create
        (Guarantor  => Guarantor,
         Start_Cash => Starting_Balance,
         Cash       => Starting_Balance,
         Earn       => Concorde.Money.Zero,
         Spend      => Concorde.Money.Zero);
   end New_Account;

   -----------------
   -- Remove_Cash --
   -----------------

   procedure Remove_Cash
     (Agent : Concorde.Db.Agent.Agent_Type;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String)
   is
   begin
      Remove_Cash (Agent.Account, Cash, Tag);
   end Remove_Cash;

   -----------------
   -- Remove_Cash --
   -----------------

   procedure Remove_Cash
     (Account : Concorde.Db.Account_Reference;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String)
   is
      use type Concorde.Money.Money_Type;
      use type Concorde.Db.Account_Reference;
      Rec : constant Concorde.Db.Account.Account_Type :=
        Concorde.Db.Account.Get (Account);
      Guarantor : constant Concorde.Db.Account_Reference :=
        Rec.Guarantor;
      Old_Cash : constant Concorde.Money.Money_Type := Rec.Cash;
      New_Cash : constant Concorde.Money.Money_Type := Old_Cash - Cash;
   begin
      if New_Cash < Concorde.Money.Zero
        and then Guarantor /= Concorde.Db.Null_Account_Reference
      then
         Concorde.Db.Account.Update_Account (Account)
           .Set_Cash (Concorde.Money.Zero)
           .Set_Spend (Rec.Spend + Cash)
           .Done;
         Concorde.Db.Historical_Account.Create
           (Account    => Account,
            Time_Stamp => Concorde.Calendar.Clock,
            Tag        => Tag,
            Change     => Concorde.Money.Zero - Cash,
            Cash       => Concorde.Money.Zero);
         Remove_Cash (Guarantor, Cash,
                      "xfer acct"
                      & Concorde.Db.To_String (Account));
      else
         Concorde.Db.Account.Update_Account (Account)
           .Set_Cash (New_Cash)
           .Set_Spend (Rec.Spend + Cash)
           .Done;
         Concorde.Db.Historical_Account.Create
           (Account    => Account,
            Time_Stamp => Concorde.Calendar.Clock,
            Tag        => Tag,
            Change     => Concorde.Money.Zero - Cash,
            Cash       => New_Cash);
      end if;
   end Remove_Cash;

end Concorde.Agents;
