with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with WL.String_Maps;
with WL.String_Sets;

with Concorde.Calendar;
with Concorde.Elementary_Functions;
with Concorde.Real_Images;
with Concorde.Random;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Network;

with Concorde.Db.Account;
with Concorde.Db.Colony;
with Concorde.Db.Colony_Pop_Group;
with Concorde.Db.Faction;
with Concorde.Db.Group_Influence;
with Concorde.Db.Pop;
with Concorde.Db.Pop_Group;
with Concorde.Db.Pop_Group_Member;

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
         Group_Size : constant Natural :=
                        Natural
                          (Real'Ceiling
                             (IG.Proportion * Total_Pop));
         Pop_Count  : constant Positive := Pops_Per_Wealth_Group;
         Size       : constant Natural := Group_Size / Pop_Count;
      begin

         Concorde.Db.Colony_Pop_Group.Create
           (Colony    => Colony,
            Pop_Group => IG.Group,
            Size      => Group_Size,
            Income    => Concorde.Money.To_Money (IG.Income));

         for I in 1 .. Pop_Count loop
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
         Min_Income : Non_Negative_Real := Non_Negative_Real'Last;
         Max_Income : Non_Negative_Real := Non_Negative_Real'First;
      begin
         for Group of Concorde.Db.Pop_Group.Scan_By_Tag loop

            All_Groups.Append (Group.Get_Pop_Group_Reference);

            if Group.Is_Wealth_Group then
               declare
                  Income      : constant Non_Negative_Real :=
                                  Initial_Setting
                                    (Group.Tag & "-income");
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

   end Create_Initial_Pops;

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
      Total_Pop : constant Real := Get ("total-population");

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
                     Capacity          =>
                       Concorde.Quantities.To_Quantity (1.0e6),
                     World             => World,
                     Faction           => Faction,
                     Capital           => Sector,
                     Plurality         => Get ("plurality"));

      Network   : constant Concorde.Db.Network_Reference :=
                    Concorde.Db.Colony.Get (Colony).Get_Network_Reference;

      package Setting_Maps is new WL.String_Maps (Real);
      Pop_Settings : Setting_Maps.Map;

      function Initial_Value
        (Tag : String)
         return Real
      is (if Pop_Settings.Contains (Tag)
          then Pop_Settings.Element (Tag)
          else Real (Long_Float'(Config.Get (Tag, 0.0))));

   begin
      Create_Initial_Pops
        (Faction               => Faction,
         World                 => World,
         World_Sector          => Sector,
         Colony                => Colony,
         Config                => Config,
         Apathy                => Get ("apathy"),
         Gini                  => Get ("gini", 0.5),
         Pops_Per_Wealth_Group => 10);

      declare
         Sizes  : Setting_Maps.Map;
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
                     E := E + Non_Negative_Real (Pop.Size);
                  end;
               end;
            end loop;
         end loop;

         for Position in Sizes.Iterate loop
            declare
               Key  : constant String := Setting_Maps.Key (Position);
               Size : constant Real := Setting_Maps.Element (Position);
            begin
               Pop_Settings.Insert
                 (Key & "-population", Size);
               Pop_Settings.Insert
                 (Key & "-proportion",
                  Size / Total_Pop);
               Pop_Settings.Insert (Key, 0.75);
            end;
         end loop;
      end;

      Concorde.Network.Create_Initial_Network
        (Network       => Network,
         Initial_Value => Initial_Value'Access);

   end New_Colony;

end Concorde.Colonies.Create;
