with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;
with WL.String_Sets;

with Concorde.Calendar;
with Concorde.Elementary_Functions;
with Concorde.Real_Images;
with Concorde.Random;

with Concorde.Money;

with Concorde.Network;
with Concorde.Worlds;

with Concorde.Colonies.Sectors;
with Concorde.Individuals.Create;

with Concorde.Db.Account;
with Concorde.Db.Colony;
with Concorde.Db.Colony_Policy;
with Concorde.Db.Colony_Pop_Group;
with Concorde.Db.Colony_Price;
with Concorde.Db.Colony_Sector;
with Concorde.Db.Colony_Zone;
with Concorde.Db.Commodity;
with Concorde.Db.Economic_Sector;
with Concorde.Db.Faction;
with Concorde.Db.Group_Influence;
with Concorde.Db.Network_Value;
with Concorde.Db.Policy;
with Concorde.Db.Pop;
with Concorde.Db.Pop_Group;
with Concorde.Db.Pop_Group_Member;
with Concorde.Db.Sector_Use;
with Concorde.Db.World_Sector;
with Concorde.Db.Zone;

package body Concorde.Colonies.Create is

   procedure Create_Initial_Pops
     (Faction               : Concorde.Db.Faction_Reference;
      World                 : Concorde.Db.World_Reference;
      World_Sector          : Concorde.Db.World_Sector_Reference;
      Colony                : Concorde.Db.Colony_Reference;
      Config                : Tropos.Configuration;
      Apathy                : Unit_Real;
      Gini                  : Unit_Real;
      Pops_Per_Wealth_Group : Positive);

   package Sector_Size_Maps is
     new WL.String_Maps (Non_Negative_Real);

   procedure Initialize_Zone
     (Colony      : Concorde.Db.Colony_Reference;
      Sector      : Concorde.Db.World_Sector_Reference;
      Zone_Config : Tropos.Configuration;
      Sizes       : in out Sector_Size_Maps.Map);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image
     with Unreferenced;

   -------------------------
   -- Create_Initial_Pops --
   -------------------------

   procedure Create_Initial_Pops
     (Faction               : Concorde.Db.Faction_Reference;
      World                 : Concorde.Db.World_Reference;
      World_Sector          : Concorde.Db.World_Sector_Reference;
      Colony                : Concorde.Db.Colony_Reference;
      Config                : Tropos.Configuration;
      Apathy                : Unit_Real;
      Gini                  : Unit_Real;
      Pops_Per_Wealth_Group : Positive)
   is
      use Concorde.Elementary_Functions;

      function Initial_Setting
        (Name    : String;
         Default : Real := 0.0)
         return Real
      is (Real (Long_Float'(Config.Get (Name, Long_Float (Default)))));

      Total_Pop : constant Non_Negative_Real :=
                    Initial_Setting
                      ("total-population");

      package Pop_Group_Vectors is
        new Ada.Containers.Vectors
          (Positive, Concorde.Db.Pop_Group_Reference,
           Concorde.Db."=");

      All_Groups : Pop_Group_Vectors.Vector;

      type Income_Group is
         record
            Group           : Concorde.Db.Pop_Group_Reference;
            Income          : Non_Negative_Real;
            Relative_Income : Unit_Real;
            Proportion      : Unit_Real;
         end record;

      package Income_Group_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Income_Group);

      function Less_Than (Left, Right : Income_Group) return Boolean
      is (Left.Income < Right.Income);

      package Income_Group_Sorting is
        new Income_Group_Lists.Generic_Sorting (Less_Than);

      Income_Groups     : Income_Group_Lists.List;

      Total_Income     : Non_Negative_Real := 0.0;

      Socialist  : constant Concorde.Db.Pop_Group_Reference :=
                     Concorde.Db.Pop_Group.Get_Reference_By_Tag ("socialist");
      Capitalist : constant Concorde.Db.Pop_Group_Reference :=
                     Concorde.Db.Pop_Group.Get_Reference_By_Tag ("capitalist");
      Everybody  : constant Concorde.Db.Pop_Group_Reference :=
                     Concorde.Db.Pop_Group.Get_Reference_By_Tag ("everybody");

      P_Socialist : constant Unit_Real :=
                      Initial_Setting ("socialist",
                                       Concorde.Db.Pop_Group.Get (Socialist)
                                       .Proportion);
      P_Capitalist : constant Unit_Real :=
                       Initial_Setting ("capitalist",
                                        Concorde.Db.Pop_Group.Get (Capitalist)
                                        .Proportion);

      procedure Calculate_Distribution;

      function Recalculate_Gini (Power : Non_Negative_Real) return Unit_Real;

      function Current_Gini return Unit_Real;

      procedure Add_Pop_Group
        (IG : Income_Group);

      -------------------
      -- Add_Pop_Group --
      -------------------

      procedure Add_Pop_Group
        (IG : Income_Group)
      is
         Group_Size : constant Real :=
                        IG.Proportion * Total_Pop / 1000.0;
         Pop_Count  : constant Real := Real (Pops_Per_Wealth_Group);
         Size       : constant Real := Group_Size / Pop_Count;
      begin

         Concorde.Db.Colony_Pop_Group.Create
           (Colony    => Colony,
            Pop_Group => IG.Group,
            Size      => Group_Size,
            Income    => Concorde.Money.To_Money (IG.Income));

         for I in 1 .. Pops_Per_Wealth_Group loop
            declare
               Groups            : Pop_Group_Vectors.Vector;
               Left_Bias         : constant Non_Negative_Real :=
                                     1.5 - Sqrt (IG.Relative_Income);
               Left              : constant Unit_Real :=
                                     Concorde.Random.Unit_Random ** Left_Bias;
               Considered_Groups : WL.String_Sets.Set;

               procedure Add_Group
                 (G : Concorde.Db.Pop_Group_Reference);

               ---------------
               -- Add_Group --
               ---------------

               procedure Add_Group
                 (G : Concorde.Db.Pop_Group_Reference)
               is
               begin
                  Groups.Append (G);
                  Considered_Groups.Include
                    (Concorde.Db.Pop_Group.Get (G).Tag);
               end Add_Group;

            begin
               Add_Group (Everybody);
               Add_Group (IG.Group);
               if Left < P_Socialist then
                  Add_Group (Socialist);
               elsif Left > 1.0 - P_Capitalist then
                  Add_Group (Capitalist);
               end if;

               for Group of Concorde.Db.Pop_Group.Scan_By_Tag loop
                  if not Considered_Groups.Contains (Group.Tag) then
                     Considered_Groups.Include (Group.Tag);
                     declare
                        Chance : Unit_Real :=
                                   Initial_Setting
                                     (Group.Tag, Group.Proportion);
                     begin
                        for Current_Group of Groups loop
                           declare
                              use Concorde.Db, Concorde.Db.Group_Influence;
                              From : constant Pop_Group_Reference :=
                                       Current_Group;
                              To   : constant Pop_Group_Reference :=
                                       Group.Get_Pop_Group_Reference;
                              Influence : constant Group_Influence_Type :=
                                            Get_By_Group_Influence
                                              (From, To);
                           begin
                              Chance := Chance * (1.0 + Influence.Influence);
                           end;
                        end loop;

                        if Concorde.Random.Unit_Random <= Chance then
                           Add_Group (Group.Get_Pop_Group_Reference);
                        end if;
                     end;
                  end if;
               end loop;

               declare
                  Pop : constant Concorde.Db.Pop_Reference :=
                          Concorde.Db.Pop.Create
                            (Faction          => Faction,
                             Colony           => Colony,
                             World            => World,
                             World_Sector     => World_Sector,
                             Size             => Size,
                             Wealth_Group     => IG.Group,
                             Apathy           => Apathy);
               begin
                  for Group of Groups loop
                     Concorde.Db.Pop_Group_Member.Create
                       (Pop, Group);
                  end loop;
               end;
            end;
         end loop;
      end Add_Pop_Group;

      ----------------------------
      -- Calculate_Distribution --
      ----------------------------

      procedure Calculate_Distribution is
         Low  : Non_Negative_Real := 0.5;
         High : Non_Negative_Real := 1.5;
      begin
         loop
            declare
               Power : constant Non_Negative_Real := (High + Low) / 2.0;
               G     : constant Unit_Real :=
                         Recalculate_Gini (Power);
            begin
               if abs (G - Gini) < 0.005 then
                  Ada.Text_IO.Put_Line
                    ("power:" & Natural'Image (Natural (Power * 100.0)) & "%");
                  exit;
               elsif G < Gini then
                  High := Power;
               else
                  Low := Power;
               end if;
            end;
         end loop;

      end Calculate_Distribution;

      ------------------
      -- Current_Gini --
      ------------------

      function Current_Gini return Unit_Real is
         B            : Unit_Real := 0.0;
         Total_Income : Non_Negative_Real := 0.0;
      begin

         for IG of Income_Groups loop
            Total_Income := Total_Income
              + IG.Income * IG.Proportion * Total_Pop;
         end loop;

         for IG of Income_Groups loop
            B := B
              + IG.Proportion
              * (IG.Proportion * Total_Pop * IG.Income / Total_Income)
              * 0.7;
         end loop;

         return 1.0 - 2.0 * Unit_Real'Min (B, 0.5);
      end Current_Gini;

      ----------------------
      -- Recalculate_Gini --
      ----------------------

      function Recalculate_Gini
        (Power : Non_Negative_Real)
         return Unit_Real
      is

         procedure Distribute_Pops;

         ---------------------
         -- Distribute_Pops --
         ---------------------

         procedure Distribute_Pops is
            Total_Income     : Non_Negative_Real := 0.0;
            Total_Proportion : Non_Negative_Real := 0.0;
         begin

            for IG of Income_Groups loop
               Total_Income := Total_Income + IG.Income;
            end loop;

            for IG of Income_Groups loop
               Total_Proportion := Total_Proportion +
                 (Total_Income / IG.Income) ** Power;
            end loop;

            for IG of Income_Groups loop
               IG.Proportion :=
                 (Total_Income / IG.Income) ** Power
                 / Total_Proportion;
            end loop;
         end Distribute_Pops;

      begin
         Distribute_Pops;
         return Current_Gini;
      end Recalculate_Gini;

   begin
      Ada.Text_IO.Put_Line
        ("Total population:"
         & Concorde.Real_Images.Approximate_Image (Total_Pop));

      declare
         use all type Concorde.Db.Record_Type;
         Min_Income : Non_Negative_Real := Non_Negative_Real'Last;
         Max_Income : Non_Negative_Real := Non_Negative_Real'First;
      begin
         for Group of Concorde.Db.Pop_Group.Scan_By_Tag loop

            All_Groups.Append (Group.Get_Pop_Group_Reference);

            if Group.Top_Record = R_Wealth_Group then
               declare
                  Income      : constant Non_Negative_Real :=
                                  Initial_Setting
                                    (Group.Tag & "-base-income");
               begin
                  if Income < Min_Income then
                     Min_Income := Income;
                  end if;
                  if Income > Max_Income then
                     Max_Income := Income;
                  end if;
                  Income_Groups.Append
                    (Income_Group'
                       (Group           => Group.Get_Pop_Group_Reference,
                        Income          => Income,
                        Relative_Income => 0.0,
                        Proportion      => 0.0));
                  Total_Income := Total_Income + Income;
               end;
            end if;
         end loop;

         for IG of Income_Groups loop
            IG.Relative_Income :=
              (IG.Income - Min_Income) / (Max_Income - Min_Income);
         end loop;
      end;

      Income_Group_Sorting.Sort (Income_Groups);

      Calculate_Distribution;

      declare
         Total_Income : Non_Negative_Real := 0.0;
      begin

         for IG of Income_Groups loop
            Total_Income := Total_Income
              + IG.Income * IG.Proportion * Total_Pop;
         end loop;

         Ada.Text_IO.Put ("GROUP");
         Ada.Text_IO.Set_Col (20);
         Ada.Text_IO.Put ("SIZE");
         Ada.Text_IO.Set_Col (40);
         Ada.Text_IO.Put ("%TOTAL");
         Ada.Text_IO.Set_Col (60);
         Ada.Text_IO.Put ("%INCOME");
         Ada.Text_IO.Set_Col (80);
         Ada.Text_IO.Put ("REL INCOME");
         Ada.Text_IO.Set_Col (100);
         Ada.Text_IO.Put ("#POPS");
         Ada.Text_IO.New_Line;

         for IG of Income_Groups loop
            Ada.Text_IO.Put (Concorde.Db.Pop_Group.Get (IG.Group).Tag);
            Ada.Text_IO.Set_Col (20);
            Ada.Text_IO.Put
              (Natural'Image (Natural (Total_Pop * IG.Proportion)));
            Ada.Text_IO.Set_Col (40);
            Ada.Text_IO.Put
              (Natural'Image (Natural (IG.Proportion * 100.0)) & "%");
            Ada.Text_IO.Set_Col (60);
            Ada.Text_IO.Put
              (Natural'Image
                 (Natural
                      (IG.Proportion * Total_Pop * IG.Income / Total_Income
                       * 100.0)) & "%");
            Ada.Text_IO.Set_Col (80);
            Ada.Text_IO.Put
              (Natural'Image
                 (Natural (IG.Relative_Income * 100.0)) & "%");
            Ada.Text_IO.Set_Col (100);
            Ada.Text_IO.New_Line;
         end loop;

      end;

      Ada.Text_IO.Put_Line
        ("gini:"
         & Natural'Image (Natural (Current_Gini * 100.0))
         & "%");

      for IG of Income_Groups loop
         Add_Pop_Group (IG);
      end loop;

      for I in 1 .. Natural (Log (Total_Pop, 10.0) * 10.0) loop
         declare
            use Concorde.Calendar;
            Age_In_Years : constant Non_Negative_Real :=
                             Real'Max
                               (18.0,
                                Concorde.Random.Normal_Random (10.0) + 40.0);
            Age_Duration : constant Duration :=
                             Duration (Age_In_Years) * Days (360.0);
         begin
            Concorde.Individuals.Create.New_Individual
              (Colony, Concorde.Calendar.Clock - Age_Duration);
         end;
      end loop;

   end Create_Initial_Pops;

   ---------------------
   -- Initialize_Zone --
   ---------------------

   procedure Initialize_Zone
     (Colony      : Concorde.Db.Colony_Reference;
      Sector      : Concorde.Db.World_Sector_Reference;
      Zone_Config : Tropos.Configuration;
      Sizes       : in out Sector_Size_Maps.Map)
   is
   begin

      Concorde.Db.Colony_Sector.Create
        (Colony       => Colony,
         World_Sector => Sector);

      for Config of Zone_Config loop
         declare
            Tag : constant String := Config.Config_Name;
            Zone : constant Concorde.Db.Zone_Reference :=
                     Concorde.Db.Zone.Get_Reference_By_Tag (Tag);
            Size : constant Non_Negative_Real :=
                     Non_Negative_Real
                       (Float'(Config.Value));
         begin
            Concorde.Db.Colony_Zone.Create
              (Colony       => Colony,
               World_Sector => Sector,
               Zone         => Zone,
               Size         => Size);
            if Sizes.Contains (Tag) then
               Sizes (Tag) := Sizes (Tag) + Size;
            else
               Sizes.Insert (Tag, Size);
            end if;
         end;
      end loop;
   end Initialize_Zone;

   ----------------
   -- New_Colony --
   ----------------

   procedure New_Colony
     (World   : Concorde.Db.World_Reference;
      Sector  : Concorde.Db.World_Sector_Reference;
      Faction : Concorde.Db.Faction_Reference;
      Config  : Tropos.Configuration)
   is

      function Get (Name : String;
                    Default : Real := 0.0)
                    return Real
      is (Real (Long_Float'(Config.Get (Name, Long_Float (Default)))));

      Cash : constant Concorde.Money.Money_Type :=
               Concorde.Money.To_Money (Get ("cash"));
      Account : constant Concorde.Db.Account_Reference :=
                  Concorde.Db.Account.Create
                    (Guarantor  => Concorde.Db.Faction.Get (Faction).Account,
                     Start_Cash => Cash,
                     Cash       => Cash,
                     Earn       => Concorde.Money.Zero,
                     Spend      => Concorde.Money.Zero);
      Colony : constant Concorde.Db.Colony_Reference :=
                 Concorde.Db.Colony.Create
                    (Active            => True,
                     Scheduled         => False,
                     Next_Event        => Concorde.Calendar.Clock,
                     Manager           => "default-colony",
                     Account           => Account,
                     Last_Earn         => Concorde.Money.Zero,
                     Last_Spend        => Concorde.Money.Zero,
                     World             => World,
                     Faction           => Faction,
                     Capital           => Sector,
                     Plurality         => Get ("plurality"));

      Network   : constant Concorde.Db.Network_Reference :=
                    Concorde.Db.Colony.Get (Colony).Get_Network_Reference;

      Sector_Size : Sector_Size_Maps.Map;

      package Setting_Maps is new WL.String_Maps (Real);
      Overrides : Setting_Maps.Map;

      procedure Set_Override
        (Name  : String;
         Value : Real);

      function Initial_Value
        (Tag : String)
         return Real
      is (if Overrides.Contains (Tag)
          then Overrides.Element (Tag)
          else Real (Long_Float'(Config.Get (Tag, 0.0))));

      ------------------
      -- Set_Override --
      ------------------

      procedure Set_Override
        (Name  : String;
         Value : Real)
      is
      begin
         if Overrides.Contains (Name) then
            raise Constraint_Error with
              "redefined: " & Name;
         end if;
         Overrides.Insert (Name, Value);
      end Set_Override;

   begin

      for Commodity of
        Concorde.Db.Commodity.Scan_By_Tag
      loop
         Concorde.Db.Colony_Price.Create
           (Colony    => Colony,
            Commodity => Commodity.Get_Commodity_Reference,
            Price     => Commodity.Base_Price);
      end loop;

      Ada.Text_IO.Put_Line ("creating initial population");

      Create_Initial_Pops
        (Faction               => Faction,
         World                 => World,
         World_Sector          => Sector,
         Colony                => Colony,
         Config                => Config,
         Apathy                => Get ("apathy"),
         Gini                  => Get ("gini", 0.5),
         Pops_Per_Wealth_Group => 10);

      Ada.Text_IO.Put_Line ("allocating pop groups");

      declare
         Sizes  : Setting_Maps.Map;
         Total  : Non_Negative_Real := 0.0;
      begin
         for Pop of
           Concorde.Db.Pop.Select_By_Colony
             (Colony)
         loop
            for Group_Member of
              Concorde.Db.Pop_Group_Member.Select_By_Pop
                (Pop.Get_Pop_Reference)
            loop
               declare
                  Tag : constant String :=
                          Concorde.Db.Pop_Group.Get
                            (Group_Member.Pop_Group)
                            .Tag;
               begin
                  if not Sizes.Contains (Tag) then
                     Sizes.Insert (Tag, 0.0);
                  end if;
                  declare
                     E : Real renames Sizes (Tag);
                  begin
                     E := E + Pop.Size;
                  end;
               end;
            end loop;
            Total := Total + Pop.Size;
         end loop;

         for Position in Sizes.Iterate loop
            declare
               Key  : constant String := Setting_Maps.Key (Position);
               Size : constant Real := Setting_Maps.Element (Position);
            begin
               Set_Override
                 (Key & "-population", Size);
               Set_Override
                 (Key & "-proportion",
                  Size / Total);
               Set_Override (Key, 0.0);
            end;
         end loop;
      end;

      Ada.Text_IO.Put_Line ("populating initial zones");

      Set_Override
        ("environment",
         Concorde.Worlds.Habitability (World) * 2.0 - 1.0);

      Concorde.Db.World_Sector.Update_World_Sector (Sector)
        .Set_Sector_Use (Concorde.Db.Sector_Use.Get_Reference_By_Tag ("urban"))
          .Set_Faction (Faction)
          .Done;

      Initialize_Zone
        (Colony      => Colony,
         Sector      => Sector,
         Zone_Config => Config.Child ("capital-sector"),
         Sizes       => Sector_Size);

      declare

         Sector_Use_Config : constant Tropos.Configuration :=
                               Config.Child ("sector-use");

         package Remaining_Sector_Maps is
           new WL.String_Maps (Natural);

         Remaining : Remaining_Sector_Maps.Map;

         function Assign_Sector
           (Sector : Concorde.Db.World_Sector_Reference)
            return Boolean;

         -------------------
         -- Assign_Sector --
         -------------------

         function Assign_Sector
           (Sector : Concorde.Db.World_Sector_Reference)
            return Boolean
         is
            Best_Sector_Use : Concorde.Db.Sector_Use_Reference :=
                                Concorde.Db.Null_Sector_Use_Reference;
            Best_Score      : Real := 0.0;
            Remaining_Count : Natural := 0;
         begin
            for Position in Remaining.Iterate loop
               declare
                  Tag : constant String :=
                          Remaining_Sector_Maps.Key (Position);
                  Sector_Use : constant Concorde.Db.Sector_Use_Reference :=
                                 Concorde.Db.Sector_Use.Get_Reference_By_Tag
                                   (Tag);
                  Count      : constant Natural :=
                                 Remaining_Sector_Maps.Element (Position);
                  Score      : constant Real :=
                                 Concorde.Colonies.Sectors.Score_Sector
                                   (Sector, Tag);
               begin
                  Remaining_Count := Remaining_Count + Count;
                  if Score > Best_Score then
                     Best_Score := Score;
                     Best_Sector_Use := Sector_Use;
                  end if;
               end;
            end loop;

            if Remaining_Count = 0 then
               return False;
            end if;

            if Best_Score > 0.0 then
               declare
                  Tag        : constant String :=
                                 Concorde.Db.Sector_Use.Get
                                   (Best_Sector_Use).Tag;
                  Use_Config : constant Tropos.Configuration :=
                                 Sector_Use_Config.Child (Tag);
                  Count      : constant Positive := Remaining.Element (Tag);
               begin
                  Concorde.Db.World_Sector.Update_World_Sector (Sector)
                    .Set_Faction (Faction)
                    .Set_Sector_Use (Best_Sector_Use)
                    .Done;

                  Initialize_Zone
                    (Colony      => Colony,
                     Sector      => Sector,
                     Zone_Config => Use_Config.Child ("zones"),
                     Sizes       => Sector_Size);

                  if Count = 1 then
                     Remaining.Delete (Tag);
                  else
                     Remaining (Tag) := Remaining (Tag) - 1;
                  end if;
               end;
            end if;

            return Remaining_Count > 1;

         end Assign_Sector;

      begin

         for Sector_Use_Config of Config.Child ("sector-use") loop
            Remaining.Insert
              (Sector_Use_Config.Config_Name,
               Sector_Use_Config.Get ("count"));
         end loop;

         Concorde.Worlds.Circular_Scan
           (Start   => Sector,
            Process => Assign_Sector'Access);
      end;

      Ada.Text_IO.Put_Line ("creating economy network");

      for Zone of Concorde.Db.Zone.Scan_By_Tag loop
         if Sector_Size.Contains (Zone.Tag) then
            Set_Override
              (Zone.Tag, 1000.0 * Sector_Size.Element (Zone.Tag));
         end if;
      end loop;

      for Economic_Sector of
        Concorde.Db.Economic_Sector.Scan_By_Tag
      loop
         declare
            Zone          : constant Concorde.Db.Zone.Zone_Type :=
                              Concorde.Db.Zone.Get (Economic_Sector.Zone);
            Zone_Size     : constant Non_Negative_Real :=
                              (if Sector_Size.Contains (Zone.Tag)
                               then Sector_Size.Element (Zone.Tag)
                               else 0.0);
            Relative_Size : constant Non_Negative_Real :=
                              Initial_Value (Economic_Sector.Tag);
         begin
            Set_Override
              (Economic_Sector.Tag, Relative_Size * Zone_Size);
         end;
      end loop;

      Concorde.Network.Create_Initial_Network
        (Network       => Network,
         Initial_Value => Initial_Value'Access);

      for Policy of Concorde.Db.Policy.Scan_By_Tag loop
         Concorde.Db.Colony_Policy.Create
           (Colony  => Colony,
            Policy  => Policy.Get_Policy_Reference,
            Setting =>
              Concorde.Db.Network_Value.Get_Reference_By_Network_Value
                (Network, Policy.Get_Node_Reference),
            Revenue => Concorde.Money.Zero,
            Expense => Concorde.Money.Zero);
      end loop;

   end New_Colony;

end Concorde.Colonies.Create;
