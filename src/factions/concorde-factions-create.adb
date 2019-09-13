with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.String_Sets;

with Concorde.Calendar;
with Concorde.Configure;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Star_Systems;
with Concorde.Sectors;
with Concorde.Stock;
with Concorde.Terrain;
with Concorde.Worlds;

with Concorde.Configure.Commodities;
with Concorde.Configure.Installations;

with Concorde.Db.Account;
with Concorde.Db.Deposit;
with Concorde.Db.Faction;
with Concorde.Db.Market;
with Concorde.Db.Pop;
with Concorde.Db.Script;
with Concorde.Db.Script_Line;
with Concorde.Db.Sector_Use;
with Concorde.Db.Sector_Zone;
with Concorde.Db.Star_System_Distance;
with Concorde.Db.World;
with Concorde.Db.World_Sector;
with Concorde.Db.User;
with Concorde.Db.Utility_Class;

package body Concorde.Factions.Create is

--     Log_Faction_Creation : constant Boolean := False;

   function Find_Homeworld
     return Concorde.Db.World_Reference;

   function Find_Home_Sector
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.World_Sector_Reference;

   procedure Initialize_Zone
     (Faction        : Concorde.Db.Faction_Reference;
      Sector         : Concorde.Db.World_Sector_Reference;
      Sector_Use     : Concorde.Db.Sector_Use_Reference;
      Zone_Config    : Tropos.Configuration);

   --------------------
   -- Create_Faction --
   --------------------

   function Create_Faction
     (User        : Concorde.Db.User_Reference;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Concorde.Color.Concorde_Color;
      Setup       : Tropos.Configuration)
      return Concorde.Db.Faction_Reference
   is
      use Concorde.Db;
      Capital : constant Concorde.Db.World_Reference :=
                  Find_Homeworld;
   begin
      if Capital = Null_World_Reference then
         return Null_Faction_Reference;
      end if;

      declare
         Cash    : constant Concorde.Money.Money_Type :=
                     Concorde.Configure.Configure_Money
                       (Setup, "cash", 1000.0);
         Account : constant Concorde.Db.Account_Reference :=
                     Concorde.Db.Account.Create
                       (Guarantor  => Concorde.Db.Null_Account_Reference,
                        Start_Cash => Cash,
                        Cash       => Cash,
                        Earn       => Concorde.Money.Zero,
                        Spend      => Concorde.Money.Zero);
         Sector  : constant Concorde.Db.World_Sector_Reference :=
           Find_Home_Sector (Capital);
         Neighbours : constant Concorde.Worlds.World_Sector_Array :=
           Concorde.Worlds.Get_Neighbours (Sector);
         Faction : constant Concorde.Db.Faction_Reference :=
                     Concorde.Db.Faction.Create
                       (Name          => Name,
                        Adjective     =>
                          (if Adjective = "" then Name else Adjective),
                        Plural_Name   =>
                          (if Plural_Name = "" then Name else Plural_Name),
                        Active        => True,
                        Scheduled     => False,
                        Next_Event    => Concorde.Calendar.Clock,
                        Manager       => "default-faction",
                        Account       => Account,
                        Capacity      => Concorde.Quantities.Zero,
                        Red           => Color.Red,
                        Green         => Color.Green,
                        Blue          => Color.Blue,
                        User          => User,
                        Capital_System =>
                          Concorde.Worlds.Star_System (Capital),
                        Capital_World  => Capital);

         Script     : constant Concorde.Db.Script_Reference :=
                        Concorde.Db.Script.Create ("rc", User);
         Line_Index : Natural := 0;

      begin

         Initialize_Zone
           (Faction     => Faction,
            Sector      => Sector,
            Sector_Use  =>
              Concorde.Db.Sector_Use.Get_Reference_By_Tag ("urban"),
            Zone_Config => Setup.Child ("capital-sector"));

         declare
            Next : Natural := 0;
         begin
            for Sector_Use_Config of Setup.Child ("sector-use") loop
               declare
                  Sector_Use : constant Concorde.Db.Sector_Use_Reference :=
                    Concorde.Db.Sector_Use.Get_Reference_By_Tag
                      (Sector_Use_Config.Config_Name);
               begin
                  for I in 1 .. Sector_Use_Config.Get ("count") loop
                     Next := Next + 1;
                     exit when Next > Neighbours'Last;

                     Initialize_Zone (Faction, Neighbours (Next), Sector_Use,
                                      Sector_Use_Config.Child ("zones"));

                  end loop;
               end;
               exit when Next > Neighbours'Last;
            end loop;
         end;

         for Pop_Config of
           Setup.Child ("pops")
         loop
            declare
               Size : constant Concorde.Quantities.Quantity_Type :=
                        Concorde.Quantities.To_Quantity
                          (Real (Float'(Pop_Config.Get ("size"))));
               Cash : constant Concorde.Money.Money_Type :=
                        Concorde.Money.To_Money
                          (Real (Float'(Pop_Config.Get ("cash"))));
               Account : constant Concorde.Db.Account_Reference :=
                 Concorde.Agents.New_Account (Cash);
               Pop : constant Concorde.Db.Pop_Reference :=
                 Concorde.Db.Pop.Create
                   (Transported_Size => Concorde.Quantities.To_Real (Size),
                    Active           => True,
                    Scheduled        => False,
                    Next_Event       => Concorde.Calendar.Clock,
                    Manager          => "default-pop",
                    Account          => Account,
                    Production       => Concorde.Db.Null_Production_Reference,
                    Capacity         => Concorde.Quantities.Scale (Size, 10.0),
                    Faction          => Faction,
                    World            => Capital,
                    World_Sector     => Sector,
                    Employer         => Concorde.Db.Null_Employer_Reference,
                    Utility_Class    =>
                      Concorde.Db.Utility_Class.Get_Reference_By_Name
                        ("default-pop"),
                    Size             => Size,
                    Salary           => Concorde.Money.Zero,
                    Hours            => 1.0,
                    Health           => 1.0,
                    Happy            => 1.0,
                    Education        => 1.0);
            begin
               Concorde.Configure.Commodities.Configure_Stock
                 (Concorde.Db.Pop.Get (Pop), Pop_Config.Child ("skills"),
                  Factor => Concorde.Quantities.To_Real (Size));
            end;
         end loop;

         for Installation_Config of
           Setup.Child ("installations")
         loop
            Concorde.Configure.Installations.Configure_Installation
              (Faction, Sector, Installation_Config);
         end loop;

         if not Setup.Contains ("init-script") then
            Ada.Text_IO.Put_Line
              ("warning: no initial script in " & Setup.Config_Name);
         end if;

         for Command of Setup.Child ("init-script") loop
            Line_Index := Line_Index + 1;
            Concorde.Db.Script_Line.Create
              (Script => Script,
               Index  => Line_Index,
               Line   => Command.Config_Name);
         end loop;

         Concorde.Db.World.Update_World (Capital)
           .Set_Owner
             (Concorde.Db.Faction.Get (Faction).Get_Owner_Reference)
           .Done;

         Concorde.Db.Market.Create
           (World => Capital);

         return Faction;
      end;
   end Create_Faction;

   ---------------------
   -- Create_Factions --
   ---------------------

   procedure Create_Factions
     (Faction_Config : Tropos.Configuration;
      Setup_Config   : Tropos.Configuration)
   is
   begin
      for Config of Faction_Config loop
         Ada.Text_IO.Put_Line
           ("new faction: " & Config.Get ("name"));
         declare
            use type Concorde.Db.Faction_Reference;
            User : constant Concorde.Db.User_Reference :=
                     Concorde.Db.User.Create
                       (Login         => Config.Config_Name,
                        Password      => "",
                        Administrator => False);
            Faction : constant Concorde.Db.Faction_Reference :=
                        Create_Faction
                          (User        => User,
                           Name        => Config.Get ("name"),
                           Adjective   =>
                             Config.Get ("adjective", Config.Get ("name")),
                           Plural_Name =>
                             Config.Get ("plural", Config.Get ("name")),
                           Color       =>
                             Concorde.Color.From_String
                               (Config.Get ("color", "#ff0000")),
                           Setup       => Setup_Config);
         begin
            if Faction = Concorde.Db.Null_Faction_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "failed to create faction");
            end if;
         end;
      end loop;
   end Create_Factions;

   ----------------------
   -- Find_Home_Sector --
   ----------------------

   function Find_Home_Sector
     (World : Concorde.Db.World_Reference)
      return Concorde.Db.World_Sector_Reference
   is
      function Score_Sector
        (Sector : Concorde.Db.World_Sector.World_Sector_Type)
         return Real;

      ------------------
      -- Score_Sector --
      ------------------

      function Score_Sector
        (Sector : Concorde.Db.World_Sector.World_Sector_Type)
         return Real
      is
         Score : Real := 0.0;
      begin
         if Concorde.Terrain.Is_Water (Sector.Terrain) then
            return Real'First;
         end if;

         declare
            Ns : constant Concorde.Worlds.World_Sector_Array :=
                   Concorde.Worlds.Get_Neighbours
                     (Sector.Get_World_Sector_Reference);
         begin
            for N of Ns loop
               for Deposit of
                 Concorde.Db.Deposit.Select_By_World_Sector (N)
               loop
                  Score :=
                    Real'Max
                      (Score,
                         Deposit.Accessibility
--                      + Deposit.Abundance / 1.0e6 * Deposit.Accessibility
                       * (1.0 - Concorde.Terrain.Hazard
                         (Concorde.Worlds.Get_Terrain (N))));
               end loop;
            end loop;

            return Score * (1.0 - Concorde.Terrain.Hazard (Sector.Terrain));
         end;
      end Score_Sector;

   begin
      return Concorde.Worlds.Best_Sector (World, Score_Sector'Access);
   end Find_Home_Sector;

   --------------------
   -- Find_Homeworld --
   --------------------

   function Find_Homeworld
     return Concorde.Db.World_Reference
   is

      package Star_System_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Concorde.Db.Star_System_Reference,
           Concorde.Db."=");

      Queue : Star_System_Lists.List;
      Checked : WL.String_Sets.Set;

      function Check_World
        (World : Concorde.Db.World_Reference)
         return Boolean;

      -----------------
      -- Check_World --
      -----------------

      function Check_World
        (World : Concorde.Db.World_Reference)
         return Boolean
      is
      begin
         return Concorde.Worlds.Habitability (World) > 0.7;
      end Check_World;

   begin

      Queue.Append (Concorde.Star_Systems.First);
      Checked.Insert
        (Concorde.Star_Systems.Name (Concorde.Star_Systems.First));

      while not Queue.Is_Empty loop
         declare
            use Concorde.Star_Systems;
            Star_System : constant Concorde.Db.Star_System_Reference :=
                            Queue.First_Element;
         begin
            Queue.Delete_First;

            if not Claimed (Star_System) then
               declare
                  Selection : constant Concorde.Worlds.World_Selection :=
                                Concorde.Star_Systems.Terrestrial_Worlds
                                  (Star_System);
               begin
                  if not Selection.Is_Empty then
                     for W of Selection.Get_Worlds loop
                        if Check_World (W) then
                           Claim (Star_System);
                           return W;
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            for Neighbour of
              Db.Star_System_Distance
                .Select_Star_System_Range_Bounded_By_Distance
                  (Star_System, 0.0, 99.0)
            loop
               declare
                  Neighbour_Name : constant String :=
                                     Name (Neighbour.To);
               begin
                  if not Checked.Contains (Neighbour_Name) then
                     Checked.Insert (Neighbour_Name);
                     Queue.Append (Neighbour.To);
                  end if;
               end;
            end loop;
         end;
      end loop;

      return Concorde.Db.Null_World_Reference;
   end Find_Homeworld;

   ---------------------
   -- Initialize_Zone --
   ---------------------

   procedure Initialize_Zone
     (Faction     : Concorde.Db.Faction_Reference;
      Sector      : Concorde.Db.World_Sector_Reference;
      Sector_Use  : Concorde.Db.Sector_Use_Reference;
      Zone_Config : Tropos.Configuration)
   is
      Owner      : constant Concorde.Db.Owner_Reference :=
        Concorde.Db.Faction.Get (Faction).Get_Owner_Reference;
      Has_Stock  : constant Concorde.Db.Has_Stock_Reference :=
        Concorde.Sectors.Has_Stock_Reference (Sector)
        with Unreferenced;
   begin

      Concorde.Db.World_Sector.Update_World_Sector (Sector)
        .Set_Sector_Use (Sector_Use)
        .Set_Owner (Owner)
        .Done;

      for Available_Config of Zone_Config loop
         if not Commodities.Exists
           (Available_Config.Config_Name)
         then
            raise Constraint_Error with
              "zone: no such commodity: "
              & Available_Config.Config_Name;
         end if;

         declare
            General_Commodity : constant Commodities.Commodity_Reference :=
              Commodities.Get (Available_Config.Config_Name);
            Tag               : constant String :=
              Commodities.Title_Tag (Sector, General_Commodity);
            Price             : constant Concorde.Money.Price_Type :=
              Commodities.Initial_Price (General_Commodity);
            Ref : constant Concorde.Db.Sector_Zone_Reference :=
              Concorde.Db.Sector_Zone.Create
                (Commodity_Class =>
                   Concorde.Commodities.Title_Category,
                 Initial_Price   => Price,
                 Mass            => 1.0,
                 Density         => 1.0,
                 Tag             => Tag,
                 Sector_Use      => Sector_Use,
                 World_Sector    => Sector);

            Stock : constant Concorde.Db.Has_Stock_Reference :=
              Concorde.Db.Faction.Get (Faction).Get_Has_Stock_Reference;
            Commodity : constant Concorde.Db.Commodity_Reference :=
              Concorde.Db.Sector_Zone.Get (Ref).Get_Commodity_Reference;
            Lease     : constant Concorde.Db.Commodity_Reference :=
              Concorde.Commodities.Lease
                (Commodity, 360);
            Quantity          : constant Concorde.Quantities.Quantity_Type :=
              Concorde.Quantities.To_Quantity
                (Real (Float'(Available_Config.Value)));
         begin
            Concorde.Stock.Add_Initial_Stock
              (Stock, Commodity, Quantity);
            Concorde.Stock.Add_Initial_Stock
              (Stock, Lease, Quantity);
         end;
      end loop;

   end Initialize_Zone;

end Concorde.Factions.Create;
