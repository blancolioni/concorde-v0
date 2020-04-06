with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Colonies;
with Concorde.Worlds;

with Concorde.Logging;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Network;

with Concorde.Db.Colony;
with Concorde.Db.Colony_Policy;
with Concorde.Db.Colony_Price;
with Concorde.Db.Colony_Zone;
with Concorde.Db.Commodity;
with Concorde.Db.Deposit;
with Concorde.Db.Network_Value;
with Concorde.Db.Node;
with Concorde.Db.Policy;
with Concorde.Db.Pop;
with Concorde.Db.Pop_Group;
with Concorde.Db.Pop_Group_Demand;
with Concorde.Db.Pop_Group_Member;
with Concorde.Db.Resource;
with Concorde.Db.Wealth_Group;
with Concorde.Db.Zone;

package body Concorde.Managers.Colonies is

   type Pop_Group_Values is
      record
         Group       : Concorde.Db.Pop_Group_Reference;
         Income      : Concorde.Db.Network_Value_Reference;
         Tax_Rate    : Concorde.Db.Network_Value_Reference;
         Tax_Evasion : Concorde.Db.Network_Value_Reference;
         Happiness   : Concorde.Db.Network_Value_Reference;
      end record;

   package Pop_Group_Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Pop_Group_Values);

   type Root_Colony_Manager_Type is
     new Root_Manager_Type with
      record
         Colony      : Concorde.Db.Colony_Reference;
         Group_Nodes : Pop_Group_Value_Lists.List;
      end record;

   overriding function Identifier
     (Manager : Root_Colony_Manager_Type)
      return String
   is ("default-colony-manager");

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager_Type);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Colony_Manager_Type)
   is
      Colony : constant Concorde.Db.Colony.Colony_Type :=
                 Concorde.Db.Colony.Get (Manager.Colony);
      Network : constant Concorde.Db.Network_Reference :=
                  Colony.Get_Network_Reference;

      procedure Update_Colony_Prices;

      procedure Update_Population_Demands;

      procedure Update_Mining_Production;

      procedure Update_Population_Sizes;

      --------------------------
      -- Update_Colony_Prices --
      --------------------------

      procedure Update_Colony_Prices is
      begin
         for Colony_Price of
           Concorde.Db.Colony_Price.Select_By_Colony (Manager.Colony)
         loop
            declare
               Tag : constant String :=
                       Concorde.Db.Commodity.Get (Colony_Price.Commodity).Tag
                     & "-base-price";
               Price : constant Real :=
                         Concorde.Money.To_Real (Colony_Price.Price);
            begin
               Concorde.Network.Set_New_Value
                 (Network, Tag, Price);
               Concorde.Network.Commit_New_Value (Network, Tag);
            end;
         end loop;
      end Update_Colony_Prices;

      ------------------------------
      -- Update_Mining_Production --
      ------------------------------

      procedure Update_Mining_Production is

         package Resource_Mining_Maps is
           new WL.String_Maps (Non_Negative_Real);

         Mined_Resource : Resource_Mining_Maps.Map;

         Mine : constant Concorde.Db.Zone_Reference :=
                  Concorde.Db.Zone.Get_Reference_By_Tag ("mine");
      begin

         for Colony_Zone of
           Concorde.Db.Colony_Zone.Select_By_Colony_Zone
             (Colony => Manager.Colony,
              Zone   => Mine)
         loop
            declare
               Sector  : constant Concorde.Db.World_Sector_Reference :=
                           Colony_Zone.World_Sector;
               Deposit : constant Concorde.Db.Deposit.Deposit_Type :=
                           Concorde.Db.Deposit.Get_By_World_Sector (Sector);
            begin
               if Deposit.Has_Element then
                  declare
                     Tag : constant String :=
                             Concorde.Db.Resource.Get (Deposit.Resource).Tag;
                     Concentration : constant Real :=
                                       Deposit.Concentration;
                     Available     : constant Real :=
                                       Concorde.Quantities.To_Real
                                         (Deposit.Available);
                     Factor        : constant Real :=
                                       Real'Max
                                         (Concentration / 10.0,
                                          Concorde.Random.Normal_Random
                                            (Concentration / 10.0)
                                          + Concentration);
                     Mined         : constant Non_Negative_Real :=
                                       Factor * Colony_Zone.Size;
                     Remaining     : constant Non_Negative_Real :=
                                       Real'Max (Available - Mined / 100.0,
                                                 0.0);
                     New_Conc      : constant Real :=
                                       Concentration
                                         * (1.0 - Mined / Remaining / 100.0);
                  begin
                     if Mined > 0.0 then
                        if not Mined_Resource.Contains (Tag) then
                           Mined_Resource.Insert (Tag, Mined);
                        else
                           Mined_Resource (Tag) :=
                             Mined_Resource (Tag) + Mined;
                        end if;

                        Concorde.Logging.Log
                          (Actor    => "mine",
                           Location => Concorde.Worlds.Name (Colony.World),
                           Category => Tag,
                           Message  =>
                             "available "
                           & Image (Available)
                           & "; concentration "
                           & Image (Concentration * 100.0)
                           & "%"
                           & "; factor "
                           & Image (Factor * 100.0) & "%"
                           & "; mined "
                           & Image (Mined)
                           & " remaining "
                           & Image (Remaining)
                           & " new concentration "
                           & Image (New_Conc * 100.0) & "%");
                        Concorde.Db.Deposit.Update_Deposit
                          (Deposit.Get_Deposit_Reference)
                          .Set_Concentration (New_Conc)
                          .Set_Difficulty (1.0 - New_Conc)
                          .Set_Available
                            (Concorde.Quantities.To_Quantity (Remaining))
                          .Done;
                     end if;
                  end;
               end if;
            end;
         end loop;

         for Position in Mined_Resource.Iterate loop
            declare
               Tag : constant String :=
                       Resource_Mining_Maps.Key (Position) & "-production";
               Quantity : constant Real :=
                            Resource_Mining_Maps.Element (Position);
            begin
               Concorde.Network.Set_New_Value (Network, Tag, Quantity);
               Concorde.Network.Commit_New_Value (Network, Tag);
            end;
         end loop;

      end Update_Mining_Production;

      procedure Update_Population_Demands is
         package Real_Maps is
           new WL.String_Maps (Real);

         Remaining_Map : Real_Maps.Map;

         function Remaining
           (Commodity : Concorde.Db.Commodity_Reference)
            return Non_Negative_Real;

         procedure Receive
           (Commodity : Concorde.Db.Commodity_Reference;
            Quantity  : Real);

         -------------
         -- Receive --
         -------------

         procedure Receive
           (Commodity : Concorde.Db.Commodity_Reference;
            Quantity  : Real)
         is
            Tag : constant String :=
                    Concorde.Db.Commodity.Get (Commodity).Tag;
         begin
            pragma Assert (Remaining_Map.Contains (Tag),
                           "remaining map did not contain " & Tag);
            Remaining_Map (Tag) := Remaining_Map (Tag) - Quantity;
         end Receive;

         ---------------
         -- Remaining --
         ---------------

         function Remaining
           (Commodity : Concorde.Db.Commodity_Reference)
            return Non_Negative_Real
         is
            Tag : constant String := Concorde.Db.Commodity.Get (Commodity).Tag;
         begin
            if not Remaining_Map.Contains (Tag) then
               Remaining_Map.Insert
                 (Tag,
                  Concorde.Network.Current_Value
                    (Network, Tag & "-supply"));
            end if;
            return Remaining_Map.Element (Tag);
         end Remaining;

      begin
         for Pop_Group of
           Concorde.Db.Wealth_Group.Scan_By_Priority
         loop

            for Pop_Group_Demand of
              Concorde.Db.Pop_Group_Demand.Select_By_Pop_Group
                (Pop_Group.Get_Pop_Group_Reference)
            loop
               declare
                  Commodity : constant Concorde.Db.Commodity_Reference :=
                                 Pop_Group_Demand.Commodity;
                  Demand_Tag : constant String :=
                                 Pop_Group.Tag
                                 & "-"
                                 & Concorde.Db.Commodity.Get (Commodity).Tag
                               & "-"
                                 & "demand";
                  Receive_Tag : constant String :=
                                  Pop_Group.Tag
                                  & "-"
                                  & Concorde.Db.Commodity.Get (Commodity).Tag
                                  & "-"
                                  & "recv";

                  Demand     : constant Real :=
                                  Concorde.Network.Current_Value
                                    (Network, Demand_Tag);
                  Supply     : constant Real :=
                                 Remaining (Commodity);
                  Received   : constant Real := Real'Min (Supply, Demand);
               begin
                  Concorde.Network.Set_New_Value
                    (Network, Receive_Tag, Received);
                  Concorde.Network.Commit_New_Value
                    (Network, Receive_Tag);
                  Receive (Commodity, Received);
               end;
            end loop;
         end loop;
      end Update_Population_Demands;

      -----------------------------
      -- Update_Population_Sizes --
      -----------------------------

      procedure Update_Population_Sizes is

         package Pop_Group_Maps is
           new WL.String_Maps (Real);

         Group_Size : Pop_Group_Maps.Map;

         procedure Add_Pop
           (Group : Concorde.Db.Pop_Group_Reference;
            Size  : Non_Negative_Real);

         -------------
         -- Add_Pop --
         -------------

         procedure Add_Pop
           (Group : Concorde.Db.Pop_Group_Reference;
            Size  : Non_Negative_Real)
         is
            Tag : constant String :=
                    Concorde.Db.Pop_Group.Get (Group).Tag;
         begin
            if not Group_Size.Contains (Tag) then
               Group_Size.Insert (Tag, 0.0);
            end if;
            Group_Size (Tag) := Group_Size (Tag) + Size;
         end Add_Pop;

      begin
         for Pop_Group of Concorde.Db.Pop_Group.Scan_By_Tag loop
            Group_Size.Insert (Pop_Group.Tag, 0.0);
         end loop;

         for Pop of Concorde.Db.Pop.Select_By_Colony (Manager.Colony) loop
            for Group of
              Concorde.Db.Pop_Group_Member.Select_By_Pop
                (Pop.Get_Pop_Reference)
            loop
               Add_Pop (Group.Pop_Group, Pop.Size);
            end loop;
         end loop;

         for Pop_Group of Concorde.Db.Pop_Group.Scan_By_Tag loop
            Concorde.Network.Set_New_Value
              (Colony.Get_Network_Reference, Pop_Group.Tag & "-population",
               Group_Size (Pop_Group.Tag));
            Concorde.Network.Commit_New_Value
              (Colony.Get_Network_Reference, Pop_Group.Tag & "-population");
            Concorde.Logging.Log
              (Actor    => Pop_Group.Tag,
               Location => Concorde.Worlds.Name (Colony.World),
               Category => "size",
               Message  =>
                 Natural'Image
                   (Natural (Group_Size.Element (Pop_Group.Tag))));
         end loop;

      end Update_Population_Sizes;

   begin
      Concorde.Logging.Log
        (Actor    => Concorde.Worlds.Name (Colony.World),
         Location => "colony",
         Category => "managed",
         Message  => "activating");

      Update_Colony_Prices;

      Update_Population_Demands;

      Update_Population_Sizes;

      Update_Mining_Production;

      Concorde.Network.Update (Colony.Get_Network_Reference);

      for Policy of
        Concorde.Db.Policy.Scan_By_Tag
      loop
         Concorde.Db.Colony_Policy.Update_Colony_Policy
           (Concorde.Db.Colony_Policy.Get_Reference_By_Colony_Policy
              (Manager.Colony, Policy.Get_Policy_Reference))
           .Set_Revenue (Concorde.Money.Zero)
           .Set_Expense (Concorde.Money.Zero)
           .Done;
      end loop;

      for Pop of Concorde.Db.Pop.Select_By_Colony (Manager.Colony) loop
         Concorde.Colonies.Daily_Tax_Revenue
           (Manager.Colony, Pop.Get_Pop_Reference);
      end loop;

      for Policy of
        Concorde.Db.Policy.Scan_By_Tag
      loop
         Concorde.Colonies.Execute_Daily_Policy
           (Colony => Manager.Colony,
            Policy => Policy.Get_Policy_Reference);
      end loop;

      declare
         Birth_Rate : constant Unit_Real :=
                        Concorde.Network.Current_Value
                          (Colony.Get_Network_Reference, "birth-rate");
         Death_Rate : constant Unit_Real :=
                        Concorde.Network.Current_Value
                          (Colony.Get_Network_Reference, "death-rate");
      begin
         for Pop of
           Concorde.Db.Pop.Select_By_Colony (Manager.Colony)
         loop
            declare
               Births : constant Non_Negative_Real :=
                          Pop.Size * (Birth_Rate / 360.0);
               Deaths : constant Non_Negative_Real :=
                          Pop.Size * (Death_Rate / 360.0);
               New_Size : constant Non_Negative_Real :=
                            Real'Max (Pop.Size + Births - Deaths, 0.0);
            begin
               Concorde.Db.Pop.Update_Pop (Pop.Get_Pop_Reference)
                 .Set_Size (New_Size)
                 .Done;
            end;
         end loop;
      end;

      Manager.Set_Next_Update_Delay (Concorde.Calendar.Days (1));

   end Activate;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference) return Manager_Type
   is
      use Concorde.Calendar;
      Colony  : constant Concorde.Db.Colony.Colony_Type :=
                  Concorde.Db.Colony.Get_Colony (Managed);
      Manager : Root_Colony_Manager_Type :=
                  Root_Colony_Manager_Type'
                    (Managed         => Managed,
                     Is_Active       => True,
                     Has_Next_Update => True,
                     Next_Update     =>
                       Concorde.Calendar.Clock
                     + Concorde.Calendar.Days (Concorde.Random.Unit_Random),
                     Colony          => Colony.Get_Colony_Reference,
                     Group_Nodes     => Pop_Group_Value_Lists.Empty_List);

      procedure Add_Group
        (Name : String);

      ---------------
      -- Add_Group --
      ---------------

      procedure Add_Group
        (Name : String)
      is
         function Get
           (Suffix : String)
            return Concorde.Db.Network_Value_Reference;

         ---------
         -- Get --
         ---------

         function Get
           (Suffix : String)
            return Concorde.Db.Network_Value_Reference
         is
            use type Concorde.Db.Node_Reference;
            Tag : constant String :=
                    Name & (if Suffix = "" then "" else "-" & Suffix);
            Node : constant Concorde.Db.Node_Reference :=
                     Concorde.Db.Node.Get_Reference_By_Tag (Tag);
         begin
            pragma Assert (Node /= Concorde.Db.Null_Node_Reference);
            return Concorde.Db.Network_Value.Get_Reference_By_Network_Value
              (Colony.Get_Network_Reference, Node);
         end Get;

         Group : constant Pop_Group_Values := Pop_Group_Values'
           (Group       => Concorde.Db.Pop_Group.Get_Reference_By_Tag (Name),
            Income      => Get ("income"),
            Tax_Rate    => Get ("income-tax-rate"),
            Tax_Evasion => Get ("tax-evasion"),
            Happiness   => Get (""));
      begin
         Manager.Group_Nodes.Append (Group);
      end Add_Group;

   begin
      for Group of
        Concorde.Db.Wealth_Group.Scan_By_Tag
      loop
         Add_Group (Group.Tag);
      end loop;
      return new Root_Colony_Manager_Type'(Manager);
   end Create_Default_Manager;

end Concorde.Managers.Colonies;
