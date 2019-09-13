with Concorde.Calendar;

with Concorde.Configure.Commodities;

with Concorde.Agents;
with Concorde.Facilities;
with Concorde.Worlds;

with Concorde.Db.Faction;
with Concorde.Db.Installation;

package body Concorde.Configure.Installations is

   ----------------------------
   -- Configure_Installation --
   ----------------------------

   procedure Configure_Installation
     (Owner  : Concorde.Db.Faction_Reference;
      Sector : Concorde.Db.World_Sector_Reference;
      Config : Tropos.Configuration)
   is
      Cash : constant Concorde.Money.Money_Type :=
               Concorde.Money.To_Money
                 (Real (Float'(Config.Get ("cash"))));
      Account : constant Concorde.Db.Account_Reference :=
        Concorde.Agents.New_Account
          (Cash, Concorde.Db.Faction.Get (Owner).Account);
      Ref : constant Concorde.Db.Installation_Reference :=
              Concorde.Db.Installation.Create
                (Transported_Size => 1000.0,
                 Active           => True,
                 Scheduled        => False,
                 Next_Event       => Concorde.Calendar.Clock,
                 Manager          =>
                   Config.Get ("manager", "default-installation"),
                 Account          => Account,
                 Production       => Concorde.Db.Null_Production_Reference,
                 Capacity         => Concorde.Quantities.To_Quantity (1.0e6),
                 Faction          => Owner,
                 World            => Concorde.Worlds.Get_World (Sector),
                 World_Sector     => Sector,
                 Facility         =>
                   Concorde.Facilities.Get (Config.Config_Name));
   begin
      Concorde.Configure.Commodities.Configure_Stock
        (Has_Stock => Concorde.Db.Installation.Get (Ref),
         Config    => Config);
   end Configure_Installation;

end Concorde.Configure.Installations;
