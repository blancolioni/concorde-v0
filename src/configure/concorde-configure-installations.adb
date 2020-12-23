with Concorde.Calendar;

with Concorde.Configure.Commodities;

with Concorde.Agents;
with Concorde.Facilities;
with Concorde.Worlds;

with Concorde.Handles.Faction;
with Concorde.Handles.Installation;

package body Concorde.Configure.Installations is

   ----------------------------
   -- Configure_Installation --
   ----------------------------

   procedure Configure_Installation
     (Owner  : Concorde.Handles.Faction.Faction_Handle;
      Sector : Concorde.Handles.World_Sector_Reference;
      Config : Tropos.Configuration)
   is
      Cash : constant Concorde.Money.Money_Type :=
               Concorde.Money.To_Money
                 (Real (Float'(Config.Get ("cash"))));
      Account : constant Concorde.Handles.Account_Reference :=
        Concorde.Agents.New_Account
          (Cash, Concorde.Handles.Faction.Get (Owner).Account);
      Ref : constant Concorde.Handles.Installation_Reference :=
              Concorde.Handles.Installation.Create
                (Transported_Size => 1000.0,
                 Active           => True,
                 Scheduled        => False,
                 Next_Event       => Concorde.Calendar.Clock,
                 Manager          =>
                   Config.Get ("manager", "default-installation"),
                 Last_Earn        => Concorde.Money.Zero,
                 Last_Spend       => Concorde.Money.Zero,
                 Account          => Account,
                 Production       => Concorde.Handles.Null_Production_Reference,
                 Capacity         => Concorde.Quantities.To_Quantity (1.0e6),
                 Faction          => Owner,
                 World            => Concorde.Worlds.Get_World (Sector),
                 World_Sector     => Sector,
                 Facility         =>
                   Concorde.Facilities.Get (Config.Config_Name));
   begin
      Concorde.Configure.Commodities.Configure_Stock
        (Has_Stock => Concorde.Handles.Installation.Get (Ref),
         Config    => Config);
   end Configure_Installation;

end Concorde.Configure.Installations;
